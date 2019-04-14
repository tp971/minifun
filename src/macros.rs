#[doc(hidden)]
#[macro_export]
macro_rules! replace_expr {
    ($_t:tt $sub:expr) => {$sub};
}

#[doc(hidden)]
#[macro_export]
macro_rules! count_tts {
    ($($tts:tt)*) => {0usize $(+ $crate::replace_expr!($tts 1usize))*};
}

#[doc(hidden)]
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
        if false { let _: ::std::result::Result<std::rc::Rc<$crate::Value>, String> = f($scope, $($argtypes),*); }
        f($scope, $($args),*)
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] Bool ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: ::std::result::Result<bool, String> = f($scope, $($argtypes),*); }
        Ok(std::rc::Rc::new($crate::Value::Bool(f($scope, $($args),*)?)))
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] Int ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: ::std::result::Result<i64, String> = f($scope, $($argtypes),*); }
        Ok(std::rc::Rc::new($crate::Value::Int(f($scope, $($args),*)?)))
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] Real ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: ::std::result::Result<f64, String> = f($scope, $($argtypes),*); }
        Ok(std::rc::Rc::new($crate::Value::Real(f($scope, $($args),*)?)))
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] Char ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: ::std::result::Result<char, String> = f($scope, $($argtypes),*); }
        Ok(std::rc::Rc::new($crate::Value::Char(f($scope, $($args),*)?)))
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] Str ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: ::std::result::Result<String, String> = f($scope, $($argtypes),*); }
        let s = f($scope, $($args),*)?;
        Ok(std::rc::Rc::new($crate::Value::String(s, s.chars().count())))
    }};
    (($_it:expr) [] [$(($args:expr))*] [$(($argtypes:expr))*] () ($scope:expr) ($f:expr)) => {{
        let f = $f;
        if false { let _: ::std::result::Result<(), String> = f($scope, $($argtypes),*); }
        f($scope, $($args),*)?;
        Ok(std::rc::Rc::new($crate::Value::Tuple(vec![])))
    }};
}

#[doc(hidden)]
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
