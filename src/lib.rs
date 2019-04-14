pub mod parser;

mod stmt;
pub use stmt::*;

mod exp;
pub use exp::*;

mod value;
pub use value::*;

mod macros;

use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct EvalError {
    token: TokenInfo,
    desc: String
}

impl EvalError {
    pub fn new(token: &TokenInfo, desc: String) -> EvalError {
        EvalError { token: token.clone(), desc }
    }

    pub fn with_token(self, token: &TokenInfo) -> EvalError {
        EvalError { token: token.clone(), desc: self.desc }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}: {}", self.token.source, self.token.row, self.token.col, self.desc)
    }
}

pub type Scope = std::collections::HashMap<String, Rc<Value>>;

#[derive(Debug, Clone)]
pub enum Token {
    EOF,
    ID(String), TRUE, FALSE,
    INT(i64), REAL(f64), CHAR(char), STRING(String),
    LPAR, RPAR, COMMA, LBRACE, RBRACE,
    FN, RIGHTARROW, IF, THEN, ELSE,
    SEMICOLON, LET, FUN,
    PLUS, MINUS, BANG, STAR, SLASH, PERCENT,
    AND, PIPE, HAT,
    LESS, LESSEQ, GREATER, GREATEREQ,
    EQ, BANGEQ, ANDAND, PIPEPIPE
}

impl Token {
    pub fn prec(&self) -> (i16, i16) {
        match self {
            Token::PIPEPIPE =>
                (0, 1),
            Token::ANDAND =>
                (2, 3),
            Token::EQ | Token::BANGEQ =>
                (4, 5),
            Token::LESS | Token::LESSEQ | Token::GREATER | Token::GREATEREQ =>
                (6, 7),
            Token::PIPE =>
                (8, 9),
            Token::HAT =>
                (10, 11),
            Token::AND =>
                (12, 13),
            Token::PLUS | Token::MINUS =>
                (14, 15),
            Token::STAR | Token::SLASH | Token::PERCENT =>
                (16, 17),
            _ =>
                (-1, -1)
        }
    }

    pub fn max_prec() -> i16 {
        18
    }

    fn fmt_esc(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Token::EOF => 
                write!(f, "end of file"),
            Token::ID(id) if id == "" => 
                write!(f, "identifier"),
            Token::ID(id) => 
                write!(f, "identifier `{}`", id),
            Token::STRING(_) => 
                write!(f, "string constant"),
            _ =>
                write!(f, "`{}`", self)
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub source: String,
    pub row: usize,
    pub col: usize,
    pub token: Token
}

pub fn assign_arg(scope: &mut Scope, arg: &Rc<Exp>, val: &Rc<Value>) -> Result<(), EvalError> {
    match (arg.as_ref(), val.as_ref()) {
        (Exp::ID(_, id), _) => {
            scope.insert(id.clone(), Rc::clone(val));
            Ok(())
        },
        (Exp::Tuple(token, args), Value::Tuple(vals)) => {
            if args.len() != vals.len() {
                return Err(EvalError::new(token,
                    format!("cannot assign {} = {}: lengths do not match", *arg, *val)));
            }
            for (arg, val) in args.iter().zip(vals.iter()) {
                assign_arg(scope, &arg, &val)?;
            }
            Ok(())
        },
        (Exp::Tuple(token, _), val) => {
            Err(EvalError::new(token,
                format!("cannot assign {} = {}: rhs is not tuple", *arg, *val)))
        }
        (_, _) => {
            panic!("This should not happen: assign_arg: arg is not ID or Tuple")
        }
    }
}

pub fn insert_stdlib(scope: &mut Scope) {
    scope.insert("Inf".to_string(), Rc::new(Value::Real(std::f64::INFINITY)));
    scope.insert("NaN".to_string(), Rc::new(Value::Real(std::f64::NAN)));
    scope.insert("NaN".to_string(), Rc::new(Value::Real(std::f64::NAN)));
    scope.insert("PI".to_string(), Rc::new(Value::Real(std::f64::consts::PI)));
    scope.insert("E".to_string(), Rc::new(Value::Real(std::f64::consts::E)));

    scope.insert("int".to_string(), intrinsic_fn! {
        (Real) -> Int:
            |_, x| Ok(x as i64)
    });
    scope.insert("real".to_string(), intrinsic_fn! {
        (Int) -> Real:
            |_, n| Ok(n as f64)
    });
    scope.insert("floor".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.floor())
    });
    scope.insert("ceil".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.ceil())
    });
    scope.insert("round".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.round())
    });
    scope.insert("abs".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.abs())
    });

    scope.insert("sqrt".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.sqrt())
    });
    scope.insert("cbrt".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.cbrt())
    });
    scope.insert("exp".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.exp())
    });
    scope.insert("ln".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.ln())
    });
    scope.insert("pow".to_string(), intrinsic_fn! {
        (Real, Real) -> Real:
            |_, x: f64, y: f64| Ok(x.powf(y))
    });
    scope.insert("log".to_string(), intrinsic_fn! {
        (Real, Real) -> Real:
            |_, x: f64, y: f64| Ok(x.log(y))
    });

    scope.insert("sin".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.sin())
    });
    scope.insert("cos".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.cos())
    });
    scope.insert("tan".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.tan())
    });
    scope.insert("asin".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.asin())
    });
    scope.insert("acos".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.acos())
    });
    scope.insert("atan".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.atan())
    });

    scope.insert("sinh".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.sinh())
    });
    scope.insert("cosh".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.cosh())
    });
    scope.insert("tanh".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.tanh())
    });
    scope.insert("asinh".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.asinh())
    });
    scope.insert("acosh".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.acosh())
    });
    scope.insert("atanh".to_string(), intrinsic_fn! {
        (Real) -> Real:
            |_, x: f64| Ok(x.atanh())
    });

    scope.insert("is_nan".to_string(), intrinsic_fn! {
        (Real) -> Bool:
            |_, x: f64| Ok(x.is_nan())
    });
    scope.insert("is_infinite".to_string(), intrinsic_fn! {
        (Real) -> Bool:
            |_, x: f64| Ok(x.is_infinite())
    });
}



impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::EOF => write!(f, "end of file"),
            Token::ID(id) => write!(f, "{}", id),
            Token::TRUE => write!(f, "true"),
            Token::FALSE => write!(f, "false"),
            Token::INT(num) => write!(f, "{}", num),
            Token::REAL(num) => write!(f, "{}", num),
            Token::CHAR(c) => write!(f, "{:?}", c),
            Token::STRING(s) => write!(f, "{:?}", s),
            Token::LPAR => write!(f, "("),
            Token::RPAR => write!(f, ")"),
            Token::COMMA => write!(f, ","),
            Token::LBRACE => write!(f, "{{"),
            Token::RBRACE => write!(f, "}}"),
            Token::FN => write!(f, "fn"),
            Token::RIGHTARROW => write!(f, "=>"),
            Token::IF => write!(f, "if"),
            Token::THEN => write!(f, "then"),
            Token::ELSE => write!(f, "else"),
            Token::SEMICOLON => write!(f, ";"),
            Token::LET => write!(f, "let"),
            Token::FUN => write!(f, "fun"),
            Token::PLUS => write!(f, "+"),
            Token::MINUS => write!(f, "-"),
            Token::BANG => write!(f, "!"),
            Token::STAR => write!(f, "*"),
            Token::SLASH => write!(f, "/"),
            Token::PERCENT => write!(f, "%"),
            Token::AND => write!(f, "&"),
            Token::PIPE => write!(f, "|"),
            Token::HAT => write!(f, "^"),
            Token::LESS => write!(f, "<"),
            Token::LESSEQ => write!(f, "<="),
            Token::GREATER => write!(f, ">"),
            Token::GREATEREQ => write!(f, ">="),
            Token::EQ => write!(f, "="),
            Token::BANGEQ => write!(f, "!="),
            Token::ANDAND => write!(f, "&&"),
            Token::PIPEPIPE => write!(f, "||")
        }
    }
}

impl fmt::Display for TokenInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.token.fmt_esc(f)
    }
}
