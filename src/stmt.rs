use super::*;

use std::collections::HashSet;

#[derive(Debug, Clone)]
pub enum Stmt {
    Empty,
    Exp(TokenInfo, Rc<Exp>),
    Let(TokenInfo, Rc<Exp>, Rc<Exp>),
    Fun(TokenInfo, String, Vec<Rc<Exp>>, Rc<Exp>)
}

impl Stmt {
    pub fn eval(&self, scope: &mut Scope) -> Result<Option<Rc<Value>>, EvalError> {
        match self {
            Stmt::Empty =>
                Ok(None),
            Stmt::Exp(_, exp) =>
                Ok(Some(exp.eval(scope)?)),
            Stmt::Let(token, arg, exp) => {
                let val = exp.eval(scope)?;
                match assign_arg(scope, arg, &val) {
                    Ok(()) => Ok(None),
                    Err(err) => Err(err.with_token(token))
                }
            },
            Stmt::Fun(_, id, args, exp) => {
                let mut scope2 = Scope::new();
                for next in self.unbound() {
                    if let Some(val) = scope.get(&next) {
                        scope2.insert(next, Rc::clone(val));
                    }
                }
                scope.insert(id.clone(), Rc::new(
                    Value::FnVal(scope2, Some(id.clone()), args.clone(), Rc::clone(exp))));
                Ok(None)
            }
        }
    }

    pub fn unbound(&self) -> HashSet<String> {
        match self {
            Stmt::Empty =>
                HashSet::new(),
            Stmt::Exp(_, exp) =>
                exp.unbound(),
            Stmt::Let(_, arg, exp) => {
                let mut res = exp.unbound();
                for next in arg.unbound() {
                    res.remove(&next);
                }
                res
            },
            Stmt::Fun(_, id, args, exp) => {
                let mut res = exp.unbound();
                for arg in args {
                    for next in arg.unbound() {
                        res.remove(&next);
                    }
                }
                res.remove(id);
                res
            }
        }
    }

    pub fn defining(&self) -> HashSet<String> {
        match self {
            Stmt::Empty
          | Stmt::Exp(_, _) =>
                HashSet::new(),
            Stmt::Let(_, arg, _) =>
                arg.unbound(),
            Stmt::Fun(_, id, _, _) => {
                let mut res = HashSet::new();
                res.insert(id.clone());
                res
            }
        }
    }
}



impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Empty =>
                write!(f, ";"),
            Stmt::Exp(_, exp) =>
                write!(f, "{};", exp),
            Stmt::Let(_, arg, exp) =>
                write!(f, "let {} = {};", arg, exp),
            Stmt::Fun(_, name, args, exp) => {
                write!(f, "fun {}", name)?;
                for next in args {
                    write!(f, " {}", next)?;
                }
                write!(f, " = {};", exp)
            }
        }
    }
}
