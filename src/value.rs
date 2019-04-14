use super::*;

pub type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Real(f64),
    Char(char),
    Str(String, i64),
    Tuple(Vec<Rc<Value>>),
    FnVal(Scope, Option<String>, Vec<Rc<Exp>>, Rc<Exp>),
    Intrinsic(IntrinsicValue)
}

#[derive(Clone)]
pub struct IntrinsicValue {
    pub f: Rc<dyn Fn(&Scope, Rc<Value>) -> Result<Rc<Value>>>
}

impl Value {
    pub fn eval_unary(&self, token: &Token) -> Result<Rc<Value>> {
        Ok(match (&token, self) {
            (Token::PLUS, Value::Int(num)) =>
                Rc::new(Value::Int(*num)),
            (Token::PLUS, Value::Real(num)) =>
                Rc::new(Value::Real(*num)),
            (Token::MINUS, Value::Int(num)) =>
                Rc::new(Value::Int(-num)),
            (Token::MINUS, Value::Real(num)) =>
                Rc::new(Value::Real(-num)),
            (Token::BANG, Value::Bool(b)) =>
                Rc::new(Value::Bool(!b)),

            (Token::PLUS, Value::Tuple(l))
          | (Token::MINUS, Value::Tuple(l))
          | (Token::BANG, Value::Tuple(l)) => {
                let mut res = Vec::new();
                for l in l {
                    res.push(l.eval_unary(token)?);
                }
                Rc::new(Value::Tuple(res))
            },
            (_, val) =>
                return Err(format!("invalid value for unary {}: {}", token, val))
        })
    }

    pub fn eval_binary(&self, token: &Token, rval: &Value) -> Result<Rc<Value>> {
        Ok(match (&token, self, rval) {
            (Token::PLUS, Value::Int(l), Value::Int(r)) =>
                match l.checked_add(*r) {
                    Some(res) =>
                        Rc::new(Value::Int(res)),
                    None => 
                        return Err("integer overflow".to_string())
                },
            (Token::PLUS, Value::Real(l), Value::Real(r)) =>
                Rc::new(Value::Real(l + r)),
            (Token::PLUS, Value::Char(l), Value::Int(r)) => {
                let res_i = match (*l as i64).checked_add(*r) {
                    Some(res) => res,
                    None =>
                        return Err("integer overflow".to_string())
                };
                if res_i < 0 || res_i > u32::max_value() as i64 {
                    return Err("integer overflow".to_string())
                }
                match std::char::from_u32(res_i as u32) {
                    Some(res) =>
                        Rc::new(Value::Char(res)),
                    None =>
                        return Err("integer overflow".to_string())
                }
            },
            (Token::PLUS, Value::Str(l, llen), Value::Str(r, rlen)) =>
                Rc::new(Value::Str(l.clone() + &r, llen + rlen)),
            (Token::PLUS, Value::Str(l, llen), Value::Char(c)) => {
                let mut s2 = l.clone();
                s2.push(*c);
                Rc::new(Value::Str(s2, llen + 1))
            },

            (Token::MINUS, Value::Int(l), Value::Int(r)) =>
                match l.checked_sub(*r) {
                    Some(res) =>
                        Rc::new(Value::Int(res)),
                    None =>
                        return Err("integer overflow".to_string())
                },
            (Token::MINUS, Value::Char(l), Value::Int(r)) => {
                let res_i = match (*l as i64).checked_sub(*r) {
                    Some(res) => res,
                    None =>
                        return Err("integer overflow".to_string())
                };
                if res_i < 0 || res_i > u32::max_value() as i64 {
                    return Err("integer overflow".to_string())
                }
                match std::char::from_u32(res_i as u32) {
                    Some(res) =>
                        Rc::new(Value::Char(res)),
                    None => 
                        return Err("integer overflow".to_string())
                }
            },
            (Token::MINUS, Value::Char(l), Value::Char(r)) =>
                Rc::new(Value::Int(*l as i64 - *r as i64)),
            (Token::MINUS, Value::Real(l), Value::Real(r)) =>
                Rc::new(Value::Real(l - r)),

            (Token::PLUS, Value::Tuple(l), Value::Tuple(r))
          | (Token::MINUS, Value::Tuple(l), Value::Tuple(r)) if l.len() == r.len() => {
                let mut res = Vec::new();
                for (l, r) in l.iter().zip(r.iter()) {
                    res.push(l.eval_binary(token, r)?);
                }
                Rc::new(Value::Tuple(res))
            },

            (Token::STAR, Value::Int(l), Value::Int(r)) =>
                match l.checked_mul(*r) {
                    Some(res) =>
                        Rc::new(Value::Int(res)),
                    None => 
                        return Err("integer overflow".to_string())
                }
            (Token::STAR, Value::Real(l), Value::Real(r)) =>
                Rc::new(Value::Real(l * r)),

            (Token::SLASH, Value::Int(l), Value::Int(r)) =>
                if *r == 0 {
                    return Err("division by zero".to_string())
                } else {
                    match l.checked_div(*r) {
                        Some(res) =>
                            Rc::new(Value::Int(res)),
                        None => 
                            return Err("integer overflow".to_string())
                    }
                },
            (Token::SLASH, Value::Real(l), Value::Real(r)) =>
                Rc::new(Value::Real(l / r)),

            (Token::PERCENT, Value::Int(l), Value::Int(r)) =>
                if *r == 0 {
                    return Err("division by zero".to_string())
                } else {
                    Rc::new(Value::Int(l % r))
                },

            (Token::AND, Value::Int(l), Value::Int(r)) =>
                Rc::new(Value::Int(l & r)),
            (Token::PIPE, Value::Int(l), Value::Int(r)) =>
                Rc::new(Value::Int(l | r)),
            (Token::HAT, Value::Int(l), Value::Int(r)) =>
                Rc::new(Value::Int(l ^ r)),

            (Token::LESS, Value::Int(l), Value::Int(r)) =>
                Rc::new(Value::Bool(l < r)),
            (Token::LESSEQ, Value::Int(l), Value::Int(r)) =>
                Rc::new(Value::Bool(l <= r)),
            (Token::GREATER, Value::Int(l), Value::Int(r)) =>
                Rc::new(Value::Bool(l > r)),
            (Token::GREATEREQ, Value::Int(l), Value::Int(r)) =>
                Rc::new(Value::Bool(l >= r)),

            (Token::LESS, Value::Real(l), Value::Real(r)) =>
                Rc::new(Value::Bool(l < r)),
            (Token::LESSEQ, Value::Real(l), Value::Real(r)) =>
                Rc::new(Value::Bool(l <= r)),
            (Token::GREATER, Value::Real(l), Value::Real(r)) =>
                Rc::new(Value::Bool(l > r)),
            (Token::GREATEREQ, Value::Real(l), Value::Real(r)) =>
                Rc::new(Value::Bool(l >= r)),

            (Token::LESS, Value::Char(l), Value::Char(r)) =>
                Rc::new(Value::Bool(l < r)),
            (Token::LESSEQ, Value::Char(l), Value::Char(r)) =>
                Rc::new(Value::Bool(l <= r)),
            (Token::GREATER, Value::Char(l), Value::Char(r)) =>
                Rc::new(Value::Bool(l > r)),
            (Token::GREATEREQ, Value::Char(l), Value::Char(r)) =>
                Rc::new(Value::Bool(l >= r)),

            (Token::LESS, Value::Str(l, _), Value::Str(r, _)) =>
                Rc::new(Value::Bool(l < r)),
            (Token::LESSEQ, Value::Str(l, _), Value::Str(r, _)) =>
                Rc::new(Value::Bool(l <= r)),
            (Token::GREATER, Value::Str(l, _), Value::Str(r, _)) =>
                Rc::new(Value::Bool(l > r)),
            (Token::GREATEREQ, Value::Str(l, _), Value::Str(r, _)) =>
                Rc::new(Value::Bool(l >= r)),

            (Token::EQ, Value::Int(l), Value::Int(r)) =>
                Rc::new(Value::Bool(l == r)),
            (Token::EQ, Value::Bool(l), Value::Bool(r)) =>
                Rc::new(Value::Bool(l == r)),
            (Token::EQ, Value::Char(l), Value::Char(r)) =>
                Rc::new(Value::Bool(l == r)),
            (Token::EQ, Value::Str(l, _), Value::Str(r, _)) =>
                Rc::new(Value::Bool(l == r)),
            (Token::BANGEQ, Value::Int(l), Value::Int(r)) =>
                Rc::new(Value::Bool(l != r)),
            (Token::BANGEQ, Value::Bool(l), Value::Bool(r)) =>
                Rc::new(Value::Bool(l != r)),
            (Token::BANGEQ, Value::Char(l), Value::Char(r)) =>
                Rc::new(Value::Bool(l != r)),
            (Token::BANGEQ, Value::Str(l, _), Value::Str(r, _)) =>
                Rc::new(Value::Bool(l != r)),

            (Token::EQ, Value::Tuple(l), Value::Tuple(r)) => {
                if l.len() == r.len() {
                    let mut res = true;
                    for (l, r) in l.iter().zip(r.iter()) {
                        if let Value::Bool(false) = l.eval_binary(token, r)?.as_ref() {
                            res = false;
                            break;
                        }
                    }
                    Rc::new(Value::Bool(res))
                } else {
                    Rc::new(Value::Bool(false))
                }
            },
            (Token::BANGEQ, Value::Tuple(l), Value::Tuple(r)) => {
                if l.len() == r.len() {
                    let mut res = false;
                    for (l, r) in l.iter().zip(r.iter()) {
                        if let Value::Bool(true) = l.eval_binary(token, r)?.as_ref() {
                            res = true;
                            break;
                        }
                    }
                    Rc::new(Value::Bool(res))
                } else {
                    Rc::new(Value::Bool(true))
                }
            },

            (_, lval, rval) =>
                return Err(format!("invalid values for binary {}: {}, {}", token, lval, rval))
        })
    }
}

impl IntrinsicValue {
    pub fn new<F>(f: F) -> IntrinsicValue
        where F: Fn(&Scope, Rc<Value>) -> Result<Rc<Value>> + 'static
    {
        IntrinsicValue { f: Rc::new(f) }
    }
}

pub fn make_intrinsic<F>(f: F) -> Rc<Value>
    where F: Fn(&Scope, Rc<Value>) -> Result<Rc<Value>> + 'static
{
    Rc::new(Value::Intrinsic(IntrinsicValue::new(f)))
}

#[macro_export]
macro_rules! replace_expr {
    ($_t:tt $sub:expr) => {$sub};
}

#[macro_export]
macro_rules! count_tts {
    ($($tts:tt)*) => {0usize $(+ $crate::replace_expr!($tts 1usize))*};
}

#[macro_export]
macro_rules! intrinsic_fn_assign {
    (($it:expr) [Value $($types:ident)*] [$(($args:expr))*] [$(($argtypes:expr))*] $ret:tt ($scope:expr) ($f:expr)) => {{
        let next = std::rc::Rc::clone($it.next().unwrap());
        $crate::intrinsic_fn_assign!(
            ($it) [$($types)*] [$(($args))* (next)] [$(($argtypes))* (std::rc::Rc::new($crate::Value::Bool(false)))] $ret ($scope) ($f))
    }};
    (($it:expr) [Bool $($types:ident)*] [$(($args:expr))*] [$(($argtypes:expr))*] $ret:tt ($scope:expr) ($f:expr)) => {{
        let next = $it.next().unwrap();
        if let $crate::Value::Bool(b) = next.as_ref() {
            $crate::intrinsic_fn_assign!(
                ($it) [$($types)*] [$(($args))* (*b)] [$(($argtypes))* (false)] $ret ($scope) ($f))
        } else {
            Err(format!("invalid value for intrinsic function call: {}", next))
        }
    }};
    (($it:expr) [Int $($types:ident)*] [$(($args:expr))*] [$(($argtypes:expr))*] $ret:tt ($scope:expr) ($f:expr)) => {{
        let next = $it.next().unwrap();
        if let $crate::Value::Int(n) = next.as_ref() {
            $crate::intrinsic_fn_assign!(
                ($it) [$($types)*] [$(($args))* (*n)] [$(($argtypes))* (0i64)] $ret ($scope) ($f))
        } else {
            Err(format!("invalid value for intrinsic function call: {}", next))
        }
    }};
    (($it:expr) [Real $($types:ident)*] [$(($args:expr))*] [$(($argtypes:expr))*] $ret:tt ($scope:expr) ($f:expr)) => {{
        let next = $it.next().unwrap();
        if let $crate::Value::Real(x) = next.as_ref() {
            $crate::intrinsic_fn_assign!(
                ($it) [$($types)*] [$(($args))* (*x)] [$(($argtypes))* (std::f64::MAX)] $ret ($scope) ($f))
        } else {
            Err(format!("invalid value for intrinsic function call: {}", next))
        }
    }};
    (($it:expr) [Char $($types:ident)*] [$(($args:expr))*] [$(($argtypes:expr))*] $ret:tt ($scope:expr) ($f:expr)) => {{
        let next = $it.next().unwrap();
        if let $crate::Value::Char(c) = next.as_ref() {
            $crate::intrinsic_fn_assign!(
                ($it) [$($types)*] [$(($args))* (*c)] [$(($argtypes))* ('0')] $ret ($scope) ($f))
        } else {
            Err(format!("invalid value for intrinsic function call: {}", next))
        }
    }};
    (($it:expr) [Str $($types:ident)*] [$(($args:expr))*] [$(($argtypes:expr))*] $ret:tt ($scope:expr) ($f:expr)) => {{
        let next = $it.next().unwrap();
        if let $crate::Value::Str(s, len) = next.as_ref() {
            $crate::intrinsic_fn_assign!(
                ($it) [$($types)*] [$(($args))* ((&s[..], *len))] [$(($argtypes))* (("", 0i64))] $ret ($scope) ($f))
        } else {
            Err(format!("invalid value for intrinsic function call: {}", next))
        }
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] Value ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: $crate::value::Result<std::rc::Rc<$crate::Value>> = f($scope, $($argtypes),*); }
        f($scope, $($args),*)
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] Bool ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: $crate::value::Result<bool> = f($scope, $($argtypes),*); }
        Ok(std::rc::Rc::new($crate::Value::Bool(f($scope, $($args),*)?)))
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] Int ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: $crate::value::Result<i64> = f($scope, $($argtypes),*); }
        Ok(std::rc::Rc::new($crate::Value::Int(f($scope, $($args),*)?)))
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] Real ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: $crate::value::Result<f64> = f($scope, $($argtypes),*); }
        Ok(std::rc::Rc::new($crate::Value::Real(f($scope, $($args),*)?)))
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] Char ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: $crate::value::Result<char> = f($scope, $($argtypes),*); }
        Ok(std::rc::Rc::new($crate::Value::Char(f($scope, $($args),*)?)))
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] Str ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: $crate::value::Result<String> = f($scope, $($argtypes),*); }
        let s = f($scope, $($args),*)?;
        Ok(std::rc::Rc::new($crate::Value::String(s, s.chars().count())))
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] () ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: $crate::value::Result<()> = f($scope, $($argtypes),*); }
        f($scope, $($args),*)?;
        Ok(std::rc::Rc::new($crate::Value::Tuple(vec![])))
    }};
}

#[macro_export]
macro_rules! intrinsic_fn_closure {
    (($scope:ident, $val:ident) ($type:ident) -> $ret:tt : $fn:expr) => {{
        let vals = vec![$val];
        $crate::intrinsic_fn_assign!((vals.iter()) [$type] [] [] $ret ($scope) ($fn))
    }};
    (($scope:ident, $val:ident) ($($types:ident),*) -> $ret:tt : $fn:expr) => {{
        if let $crate::Value::Tuple(vals) = $val.as_ref() {
            if vals.len() == $crate::count_tts!($($types)*) {
                let mut iter = vals.iter();
                $crate::intrinsic_fn_assign!((iter) [$($types)*] [] [] $ret ($scope) ($fn))
            } else {
                Err(format!("invalid value for intrinsic function call: {}", $val))
            }
        } else {
            Err(format!("invalid value for intrinsic function call: {}", $val))
        }
    }};
}

#[macro_export]
macro_rules! intrinsic_fn {
    ($($t:tt)*) => {
        $crate::make_intrinsic(|scope, val|
            $crate::intrinsic_fn_closure!((scope, val) $($t)*))
    };
}

#[macro_export]
macro_rules! intrinsic_fn_move {
    ($($t:tt)*) => {
        $crate::make_intrinsic(move |scope, val|
            $crate::intrinsic_fn_closure!((scope, val) $($t)*))
    };
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Bool(b) =>
                write!(f, "{}", b),
            Value::Int(num) =>
                write!(f, "{}", num),
            Value::Real(num) => {
                if num.is_infinite() {
                    if num.is_sign_positive() {
                        write!(f, "Inf")
                    } else {
                        write!(f, "-Inf")
                    }
                } else if num.is_nan() {
                    write!(f, "NaN")
                } else if num.is_finite() {
                    let res = format!("{}", num);
                    if res.contains('.') {
                        write!(f, "{}", res)
                    } else {
                        write!(f, "{}.0", res)
                    }
                } else {
                    unreachable!();
                }
            },
            Value::Char(c) =>
                write!(f, "{:?}", c),
            Value::Str(s, _) =>
                write!(f, "{:?}", s),
            Value::Tuple(exps) => {
                if exps.len() == 1 {
                    write!(f, "({},)", &exps[0])
                } else {
                    write!(f, "(")?;
                    for (i, next) in exps.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", next)?;
                    }
                    write!(f, ")")
                }
            },
            Value::FnVal(_, _, args, _) => {
                write!(f, "fn")?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                Ok(())
            },
            Value::Intrinsic(_) =>
                write!(f, "$fn"),
        }
    }
}

impl fmt::Debug for IntrinsicValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "IntrinsicValue")
    }
}
