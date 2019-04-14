use super::*;

use std::cmp;
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub enum Exp {
    ID(TokenInfo, String),
    BoolConst(TokenInfo, bool),
    IntConst(TokenInfo, i64),
    RealConst(TokenInfo, f64),
    CharConst(TokenInfo, char),
    StrConst(TokenInfo, String),
    Tuple(TokenInfo, Vec<Rc<Exp>>),
    Unary(TokenInfo, Token, Rc<Exp>),
    Binary(TokenInfo, Token, Rc<Exp>, Rc<Exp>),
    If(TokenInfo, Rc<Exp>, Rc<Exp>, Rc<Exp>),
    FnExp(TokenInfo, Rc<Exp>, Rc<Exp>),
    App(TokenInfo, Rc<Exp>, Rc<Exp>),
    Block(TokenInfo, Vec<Rc<Stmt>>, Option<Rc<Exp>>),

    //used for partial evaluation
    Value(Rc<Value>),
    Scope(Scope, Rc<Exp>)
}

#[derive(Debug, Clone)]
pub enum Partial {
    Value(Rc<Value>),
    Exp(Rc<Exp>)
}

const MAX_DEPTH: usize = 64;

impl Exp {
    pub fn eval(&self, scope: &Scope) -> Result<Rc<Value>, EvalError> {
        let mut res = self.eval_max(scope, MAX_DEPTH)?;
        loop {
            match res {
                Partial::Value(val) => {
                    break Ok(val);
                },
                Partial::Exp(exp) => {
                    //eprintln!("{}", exp);
                    let depth = exp.depth();
                    res = exp.eval_max(scope,
                        (depth + MAX_DEPTH - 1) / MAX_DEPTH * MAX_DEPTH)?;
                }
            }
        }
    }

    pub fn eval_max(&self, scope: &Scope, depth: usize) -> Result<Partial, EvalError> {
        if depth == 0 {
            return Ok(Partial::Exp(Rc::new(self.clone())))
        }

        match self {
            Exp::ID(token, id) =>
                match scope.get(id) {
                    Some(val) =>
                        Ok(Partial::Value(Rc::clone(val))),
                    None =>
                        Err(EvalError::new(token,
                            format!("unbound identifier `{}`", id)))
                },
            Exp::BoolConst(_, b) =>
                Ok(Partial::Value(Rc::new(Value::Bool(*b)))),
            Exp::IntConst(_, num) =>
                Ok(Partial::Value(Rc::new(Value::Int(*num)))),
            Exp::RealConst(_, num) =>
                Ok(Partial::Value(Rc::new(Value::Real(*num)))),
            Exp::CharConst(_, c) =>
                Ok(Partial::Value(Rc::new(Value::Char(*c)))),
            Exp::StrConst(_, s) =>
                Ok(Partial::Value(Rc::new(Value::Str(s.clone(), s.chars().count() as i64)))),

            Exp::Tuple(token, exps) => {
                let mut res = Vec::new();
                let mut is_value = true;
                for exp in exps {
                    if is_value {
                        let val = exp.eval_max(scope, depth - 1)?;
                        if let Partial::Exp(_) = val {
                            is_value = false;
                        }
                        res.push(val);
                    } else {
                        res.push(Partial::Exp(Rc::clone(exp)));
                    }
                }

                if is_value {
                    Ok(Partial::Value(Rc::new(Value::Tuple(
                        res.into_iter().map(Partial::into_value).collect()))))
                } else {
                    Ok(Partial::Exp(Rc::new(Exp::Tuple(token.clone(),
                        res.into_iter().map(Partial::into_exp).collect()))))
                }
            },

            Exp::Unary(token, op, rhs) => {
                let rval = rhs.eval_max(scope, depth - 1)?;
                let rval = match rval {
                    Partial::Value(val) => val,
                    Partial::Exp(part) => {
                        return Ok(Partial::Exp(Rc::new(Exp::Unary(
                            token.clone(), op.clone(), part))));
                    }
                };

                match rval.eval_unary(&token.token) {
                    Ok(val) => Ok(Partial::Value(val)),
                    Err(e) => Err(EvalError::new(&token, e))
                }
            },

            Exp::Binary(token, op, lhs, rhs) => {
                match token.token {
                    Token::ANDAND | Token::PIPEPIPE => {
                        let lval = lhs.eval_max(scope, depth - 1)?;
                        let lval = match lval {
                            Partial::Value(val) => val,
                            Partial::Exp(part) =>
                                return Ok(Partial::Exp(Rc::new(Exp::Binary(
                                    token.clone(), op.clone(), part, Rc::clone(rhs)))))
                        };

                        let short =
                            if let Token::ANDAND = token.token {
                                false
                            } else {
                                true
                            };

                        return Ok(match lval.as_ref() {
                            Value::Bool(b) if *b == short =>
                                Partial::Value(Rc::new(Value::Bool(short))),
                            Value::Bool(_) => {
                                let rval = rhs.eval_max(scope, depth - 1)?;
                                let rval = match rval {
                                    Partial::Value(val) => val,
                                    Partial::Exp(part) =>
                                        return Ok(Partial::Exp(Rc::new(Exp::Binary(
                                            token.clone(), op.clone(), Rc::new(Exp::Value(lval)), part))))
                                };

                                match rval.as_ref() {
                                    Value::Bool(b) =>
                                        Partial::Value(Rc::new(Value::Bool(*b))),
                                    _ =>
                                        return Err(EvalError::new(&token,
                                            format!("invalid values for binary {}: _, {}", token, rval)))
                                }
                            },
                            _ =>
                                return Err(EvalError::new(&token,
                                    format!("invalid values for binary {}: {}, _", token, lval)))
                        })
                    },
                    _ => {}
                }

                let lval = match lhs.eval_max(scope, depth - 1)? {
                    Partial::Value(val) => val,
                    Partial::Exp(part) =>
                        return Ok(Partial::Exp(Rc::new(Exp::Binary(
                            token.clone(), op.clone(), part, Rc::clone(rhs)))))
                };

                let rval = match rhs.eval_max(scope, depth - 1)? {
                    Partial::Value(val) => val,
                    Partial::Exp(part) =>
                        return Ok(Partial::Exp(Rc::new(Exp::Binary(
                            token.clone(), op.clone(), Rc::new(Exp::Value(lval)), part))))
                };

                match lval.eval_binary(&token.token, rval.as_ref()) {
                    Ok(val) => Ok(Partial::Value(val)),
                    Err(e) => Err(EvalError::new(&token, e))
                }
            },

            Exp::If(token, cond, texp, fexp) => {
                let cval = cond.eval_max(scope, depth - 1)?;
                let cval = match cval {
                    Partial::Value(val) => val,
                    Partial::Exp(part) => {
                        return Ok(Partial::Exp(Rc::new(Exp::If(
                            token.clone(), part, Rc::clone(texp), Rc::clone(fexp)))));
                    }
                };

                match cval.as_ref() {
                    Value::Bool(true) =>
                        texp.eval_max(scope, depth - 1),
                    Value::Bool(false) =>
                        fexp.eval_max(scope, depth - 1),
                    val =>
                        Err(EvalError::new(&token,
                            format!("invalid value for if condition: {}", val)))
                }
            },

            Exp::FnExp(_, arg, exp) => {
                let mut scope2 = Scope::new();
                for next in self.unbound() {
                    if let Some(val) = scope.get(&next) {
                        scope2.insert(next, Rc::clone(val));
                    }
                }
                Ok(Partial::Value(Rc::new(
                    Value::FnVal(scope2, None, vec![Rc::clone(arg)], Rc::clone(exp)))))
            },

            Exp::App(token, func, arg) => {
                let fval = match func.eval_max(scope, depth - 1)? {
                    Partial::Value(val) => val,
                    Partial::Exp(part) =>
                        return Ok(Partial::Exp(Rc::new(Exp::App(
                            token.clone(), part, Rc::clone(arg)))))
                };

                let aval = match arg.eval_max(scope, depth - 1)? {
                    Partial::Value(val) => val,
                    Partial::Exp(part) =>
                        return Ok(Partial::Exp(Rc::new(Exp::App(
                            token.clone(), Rc::new(Exp::Value(fval)), part))))
                };

                match fval.as_ref() {
                    Value::FnVal(scope, name, fargs, fexp) => {
                        let mut scope2 = scope.clone();
                        if let Some(name) = name {
                            scope2.insert(name.clone(), Rc::clone(&fval));
                        }
                        assign_arg(&mut scope2, &fargs[0], &aval)
                            .map_err(|err| err.with_token(token))?;
                        if fargs.len() > 1 {
                            Ok(Partial::Value(Rc::new(
                                Value::FnVal(scope2, None, (&fargs[1..]).to_vec(), Rc::clone(fexp)))))
                        } else {
                            Ok(match fexp.eval_max(&scope2, depth - 1)? {
                                Partial::Exp(exp) =>
                                    if let Exp::Scope(_, _) = exp.as_ref() {
                                        Partial::Exp(exp)
                                    } else {
                                        Partial::Exp(Rc::new(
                                            Exp::Scope(scope2, exp)))
                                    },
                                v => v
                            })
                        }
                    },
                    Value::Intrinsic(ifn) =>
                        match (ifn.f)(&scope, aval) {
                            Ok(val) => Ok(Partial::Value(val)),
                            Err(e) => Err(EvalError::new(&token, e))
                        },
                    Value::Tuple(vals) => {
                        match aval.as_ref() {
                            Value::Tuple(vals2) if vals2.is_empty() =>
                                Ok(Partial::Value(Rc::new(Value::Int(vals.len() as i64)))),
                            Value::Int(num) => {
                                if *num < 0 || *num as usize >= vals.len() {
                                    Err(EvalError::new(&token,
                                        format!("out of range: {} {}", fval, aval)))
                                } else {
                                    Ok(Partial::Value(Rc::clone(&vals[*num as usize])))
                                }
                            }
                            _ =>
                                Err(EvalError::new(&token,
                                    format!("invalid index: {} {}", fval, aval)))
                        }
                    },
                    Value::Str(s, len) => {
                        match aval.as_ref() {
                            Value::Tuple(vals) if vals.is_empty() =>
                                Ok(Partial::Value(Rc::new(Value::Int(*len)))),
                            Value::Int(off) if *off >= 0 && off < len =>
                                Ok(Partial::Value(Rc::new(
                                    Value::Char(s.chars().nth(*off as usize).unwrap())))),
                            Value::Tuple(vals) if vals.len() == 2 => {
                                let (off, count) = match (vals[0].as_ref(), vals[1].as_ref()) {
                                    (Value::Int(off), Value::Int(count))
                                        if off < len && *count >= 0 && off + count <= *len =>
                                            (off, count),
                                    _ => return Err(EvalError::new(&token,
                                        format!("invalid index: {} {}", fval, aval)))
                                };
                                Ok(Partial::Value(Rc::new(
                                    Value::Str(s.chars().skip(*off as usize).take(*count as usize).collect(), *count))))
                            }
                            _ =>
                                Err(EvalError::new(&token,
                                    format!("invalid index: {} {}", fval, aval)))
                        }
                    },
                    val =>
                        Err(EvalError::new(&token,
                            format!("cannot call value: {}", val)))
                }
            },

            Exp::Block(_, stmts, exp) => {
                let mut scope2 = scope.clone();
                for stmt in stmts {
                    stmt.eval(&mut scope2)?;
                }
                match exp {
                    Some(exp) =>
                        Ok(match exp.eval_max(&scope2, depth - 1)? {
                            Partial::Exp(exp) =>
                                if let Exp::Scope(_, _) = exp.as_ref() {
                                    Partial::Exp(exp)
                                } else {
                                    Partial::Exp(Rc::new(
                                        Exp::Scope(scope2, exp)))
                                },
                            v => v
                        }),
                    None => Ok(Partial::Value(Rc::new(Value::Tuple(vec![]))))
                }
            },

            Exp::Value(val) =>
                Ok(Partial::Value(Rc::clone(val))),

            Exp::Scope(scope2, exp) => {
                Ok(match exp.eval_max(scope2, depth - 1)? {
                    Partial::Exp(exp) =>
                        if let Exp::Scope(_, _) = exp.as_ref() {
                            Partial::Exp(exp)
                        } else {
                            Partial::Exp(Rc::new(
                                Exp::Scope(scope2.clone(), exp)))
                        },
                    v => v
                })
            }
        }
    }

    pub fn depth(&self) -> usize {
        match self {
            Exp::ID(_, _)
          | Exp::BoolConst(_, _)
          | Exp::IntConst(_, _)
          | Exp::RealConst(_, _)
          | Exp::CharConst(_, _)
          | Exp::StrConst(_, _)
          | Exp::FnExp(_, _, _)
          | Exp::Value(_) =>
                1,
            Exp::Tuple(_, exps) => {
                let mut depth = 0;
                for next in exps {
                    depth = cmp::max(depth, next.depth());
                }
                1 + depth
            },
            Exp::Unary(_, _, rhs) =>
                1 + rhs.depth(),
            Exp::Binary(_, _, lhs, rhs) =>
                1 + cmp::max(lhs.depth(), rhs.depth()),
            Exp::If(_, cond, texp, fexp) =>
                1 + cmp::max(cond.depth(), cmp::max(texp.depth(), fexp.depth())),
            Exp::App(_, func, arg) =>
                1 + cmp::max(func.depth(), arg.depth()),
            Exp::Block(_, _, Some(exp)) =>
                1 + exp.depth(),
            Exp::Block(_, _, None) =>
                1,
            Exp::Scope(_, exp) =>
                1 + exp.depth()
        }
    }

    pub fn unbound(&self) -> HashSet<String> {
        match self {
            Exp::BoolConst(_, _)
          | Exp::IntConst(_, _)
          | Exp::RealConst(_, _)
          | Exp::CharConst(_, _)
          | Exp::StrConst(_, _)
          | Exp::Value(_) =>
                HashSet::new(),
            Exp::ID(_, id) => {
                let mut res = HashSet::new();
                res.insert(id.clone());
                res
            },
            Exp::Tuple(_, exps) => {
                let mut res = HashSet::new();
                for next in exps.iter() {
                    res.extend(next.unbound());
                }
                res
            },
            Exp::Unary(_, _, rhs) =>
                rhs.unbound(),
            Exp::Binary(_, _, lhs, rhs) => {
                let mut res = lhs.unbound();
                res.extend(rhs.unbound());
                res
            },
            Exp::If(_, cond, texp, fexp) => {
                let mut res = cond.unbound();
                res.extend(texp.unbound());
                res.extend(fexp.unbound());
                res
            },
            Exp::FnExp(_, arg, exp) => {
                let mut res = exp.unbound();
                for next in arg.unbound() {
                    res.remove(&next);
                }
                res
            },
            Exp::App(_, func, arg) => {
                let mut res = func.unbound();
                res.extend(arg.unbound());
                res
            },
            Exp::Block(_, stmts, exp) => {
                let mut res =
                    match exp {
                        Some(exp) => exp.unbound(),
                        None => HashSet::new()
                    };
                for next in stmts.iter().rev() {
                    res.extend(next.unbound());
                    for next2 in next.defining() {
                        res.remove(&next2);
                    }
                }
                res
            },
            Exp::Scope(scope, exp) => {
                let mut res = exp.unbound();
                for next in scope.keys() {
                    res.remove(next);
                }
                res
            }
        }
    }
}

impl Partial {
    fn into_value(self) -> Rc<Value> {
        match self {
            Partial::Value(val) => val,
            _ => panic!("Partial::into_value on non-value")
        }
    }

    fn into_exp(self) -> Rc<Exp> {
        match self {
            Partial::Value(val) => Rc::new(Exp::Value(val)),
            Partial::Exp(exp) => exp
        }
    }
}



impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Exp::ID(_, id) =>
                write!(f, "{}", id),
            Exp::BoolConst(_, b) =>
                write!(f, "{}", b),
            Exp::IntConst(_, num) =>
                write!(f, "{}", num),
            Exp::RealConst(_, num) =>
                write!(f, "{}", num),
            Exp::StrConst(_, s) =>
                write!(f, "{:?}", s),
            Exp::CharConst(_, c) =>
                write!(f, "{:?}", c),
            Exp::Tuple(_, exps) => {
                if exps.len() == 1 {
                    write!(f, "({},)", &exps[0])
                } else {
                    write!(f, "(")?; for (i, next) in exps.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", next)?;
                    }
                    write!(f, ")")
                }
            },
            Exp::Unary(_, op, rhs) =>
                write!(f, "({}{})", op, rhs),
            Exp::Binary(_, op, lhs, rhs) =>
                write!(f, "({} {} {})", lhs, op, rhs),
            Exp::If(_, cond, texp, fexp) =>
                write!(f, "(if {} then {} else {})", cond, texp, fexp),
            Exp::FnExp(_, arg, exp) =>
                write!(f, "(fn {} => {})", arg, exp),
            Exp::App(_, func, arg) =>
                write!(f, "({} {})", func, arg),
            Exp::Block(_, stmts, exp) => {
                write!(f, "{{ ")?;
                for next in stmts {
                    write!(f, "{} ", next)?;
                }
                match exp {
                    Some(exp) => write!(f, "{} }}", exp),
                    None => write!(f, "}}")
                }
            },
            Exp::Value(val) =>
                write!(f, "$( {} )", val),
            Exp::Scope(_, exp) =>
                write!(f, "${{ {} }}", exp)
        }
    }
}
