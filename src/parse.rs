use std::{
    collections::HashMap,
    fmt,
    ops::{Add, Div, Mul, Sub},
};

use crate::{
    builtin::{builtin_constant, BUILTINS},
    lex::*,
    node::*,
    value::Value,
    vector::Vector,
};

pub struct Cube {
    pub root: NodeBox,
    pub initial_pos: Vector,
    pub initial_dir: Vector,
    pub tempo: f64,
}

#[derive(Debug)]
pub enum ParseError {
    InvalidCharacter(char),
    Expected(&'static str),
    InvalidNumber(String),
    UnknownIdent(String),
    InvalidUnaryOperation {
        op: &'static str,
        operand: &'static str,
    },
    InvalidBinaryOperation {
        a: &'static str,
        b: &'static str,
        op: &'static str,
    },
    CannotCall(&'static str),
    WrongNumberOfArguments {
        name: String,
        found: usize,
    },
    ExpectedNumber(&'static str),
    ExpectedVector(&'static str),
    IndexOutOfBounds {
        index: usize,
        len: usize,
    },
    CannotSet(&'static str),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::InvalidCharacter(c) => write!(f, "Invalid character: {}", c),
            ParseError::Expected(expectation) => write!(f, "Expected {}", expectation),
            ParseError::InvalidNumber(s) => write!(f, "Invalid number: {}", s),
            ParseError::UnknownIdent(s) => write!(f, "Unknown identifier: {}", s),
            ParseError::InvalidUnaryOperation { op, operand } => {
                write!(f, "Invalid operation: {} {}", op, operand)
            }
            ParseError::InvalidBinaryOperation { a, b, op } => {
                write!(f, "Invalid operation: {} {} {}", a, op, b)
            }
            ParseError::CannotCall(name) => write!(f, "Cannot call {name}"),
            ParseError::WrongNumberOfArguments { name, found } => {
                write!(f, "No variant of {name} takes {found} arguments")
            }
            ParseError::ExpectedNumber(name) => write!(f, "Expected {name} to be a number"),
            ParseError::ExpectedVector(name) => write!(f, "Expected {name} to be a vector"),
            ParseError::IndexOutOfBounds { index, len } => {
                write!(f, "Index {index} out of bounds for length {len}")
            }
            ParseError::CannotSet(name) => write!(f, "Cannot set {name}"),
        }
    }
}

pub type ParseResult<T = ()> = Result<T, Sp<ParseError>>;

pub fn parse(input: &str) -> ParseResult<Cube> {
    let tokens = lex(input).map_err(|e| e.map(ParseError::InvalidCharacter))?;
    let mut parser = Parser {
        tokens,
        curr: 0,
        scopes: vec![Scope {
            bindings: HashMap::new(),
        }],
        args_stack: Vec::new(),
    };
    while parser.try_exact(Token::Newline).is_some() {}
    while parser.item()? {
        while parser.try_exact(Token::Newline).is_some() {}
    }
    let root = parser
        .try_expr()?
        .map(|val| val.value.into_node())
        .unwrap_or_else(|| NodeBox::new(constant_scalar_node(0.0)));
    while parser.try_exact(Token::Newline).is_some() {}
    let initial_pos = parser
        .find_binding("initial_pos")
        .map(|val| val.value.expect_vector("initial_pos", val.span))
        .transpose()?
        .unwrap_or(Vector::ZERO);
    let initial_dir = parser
        .find_binding("initial_dir")
        .map(|val| val.value.expect_vector("initial_dir", val.span))
        .transpose()?
        .unwrap_or(Vector::X);
    let tempo = parser
        .find_binding("tempo")
        .map(|val| val.value.expect_number("tempo", val.span))
        .transpose()?
        .unwrap_or(120.0);
    if parser.curr < parser.tokens.len() {
        return Err(parser.tokens[parser.curr]
            .span
            .sp(ParseError::Expected("end of file")));
    }
    Ok(Cube {
        root,
        initial_pos,
        initial_dir,
        tempo,
    })
}

struct Parser {
    tokens: Vec<Sp<Token>>,
    curr: usize,
    scopes: Vec<Scope>,
    args_stack: Vec<Vec<Sp<Value>>>,
}

struct Scope {
    bindings: HashMap<String, Sp<Value>>,
}

impl Parser {
    fn next_token_map<T>(&mut self, f: impl FnOnce(Token) -> Option<T>) -> Option<Sp<T>> {
        let token = self
            .tokens
            .get(self.curr)
            .cloned()
            .and_then(|sp| f(sp.value).map(|val| sp.span.sp(val)))?;
        self.curr += 1;
        Some(token)
    }
    fn try_exact(&mut self, token: impl Into<Token>) -> Option<Span> {
        self.next_token_map(|t| if t == token.into() { Some(()) } else { None })
            .map(|sp| sp.span)
    }
    fn last_span(&self) -> Span {
        if let Some(token) = self
            .tokens
            .get(self.curr.saturating_sub(1))
            .or_else(|| self.tokens.last())
        {
            token.span
        } else {
            Span::default()
        }
    }
    fn expect(&mut self, token: impl Into<Token>, expectation: &'static str) -> ParseResult {
        if self.try_exact(token.into()).is_some() {
            Ok(())
        } else {
            Err(self.expected(expectation))
        }
    }
    fn expected(&self, expectation: &'static str) -> Sp<ParseError> {
        self.last_span().sp(ParseError::Expected(expectation))
    }
    fn find_binding(&self, name: &str) -> Option<Sp<&Value>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.bindings.get(name))
            .map(|val| val.span.sp(&val.value))
    }
    fn item(&mut self) -> ParseResult<bool> {
        self.binding()
    }
    fn binding(&mut self) -> ParseResult<bool> {
        let start = self.curr;
        let Some(name) = self.ident() else {
            return Ok(false);
        };
        if self.expect(Token::Equals, "equals").is_err() {
            self.curr = start;
            return Ok(false);
        }
        let value = self
            .try_expr()?
            .ok_or_else(|| self.expected("expression"))?;
        self.scopes
            .last_mut()
            .unwrap()
            .bindings
            .insert(name.value, value);
        Ok(true)
    }
    fn try_expr(&mut self) -> ParseResult<Option<Sp<Value>>> {
        self.try_as_expr()
    }
    fn try_as_expr(&mut self) -> ParseResult<Option<Sp<Value>>> {
        let Some(mut left) = self.try_md_expr()? else {
            return Ok(None);
        };
        while let Some(op) = self.next_token_map(|token| match token {
            Token::BinOp(BinOp::Add) => Some(BinOp::Add),
            Token::BinOp(BinOp::Sub) => Some(BinOp::Sub),
            _ => None,
        }) {
            let right = self.try_md_expr()?.ok_or_else(|| self.expected("term"))?;
            let span = left.span.union(right.span);
            left = span.sp(match op.value {
                BinOp::Add => left
                    .value
                    .bin_scalar_op(right.value, "+", op.span, Add::add)?,
                BinOp::Sub => left
                    .value
                    .bin_scalar_op(right.value, "-", op.span, Sub::sub)?,
                _ => unreachable!(),
            });
        }
        Ok(Some(left))
    }
    fn try_md_expr(&mut self) -> ParseResult<Option<Sp<Value>>> {
        let Some(mut left) = self.try_set()? else {
            return Ok(None);
        };

        while let Some(op) = self.next_token_map(|token| match token {
            Token::BinOp(BinOp::Mul) => Some(BinOp::Mul),
            Token::BinOp(BinOp::Div) => Some(BinOp::Div),
            _ => None,
        }) {
            let right = self.try_set()?.ok_or_else(|| self.expected("term"))?;
            let span = left.span.union(right.span);
            left = span.sp(match op.value {
                BinOp::Mul => left
                    .value
                    .bin_scalar_op(right.value, "*", op.span, Mul::mul)?,
                BinOp::Div => left
                    .value
                    .bin_scalar_op(right.value, "/", op.span, Div::div)?,
                _ => unreachable!(),
            });
        }
        Ok(Some(left))
    }
    fn try_set(&mut self) -> ParseResult<Option<Sp<Value>>> {
        let Some(mut left) = self.try_call()? else {
            return Ok(None);
        };
        loop {
            if let Some(set_span) = self.try_exact(Token::Colon) {
                // Set
                if !matches!(left.value, Value::Args) {
                    return Err(left
                        .span
                        .union(set_span)
                        .sp(ParseError::CannotSet(left.value.type_name())));
                }
                let mut indices = self.args()?;
                let Some(last_arg) = indices.last() else {
                    return Err(self.expected("arguments"));
                };
                let full_span = left.span.union(last_arg.span);
                let value = indices.remove(0);
                let mut args = self.args_stack.pop().expect("stack empty when setting");
                for nval in indices {
                    let n = nval.value.expect_number("index", nval.span)?.abs() as usize;
                    if let Some(spot) = args.get_mut(n) {
                        *spot = value.clone();
                    } else {
                        return Err(nval.span.sp(ParseError::IndexOutOfBounds {
                            index: n,
                            len: args.len(),
                        }));
                    }
                }
                self.args_stack.push(args);
                left = full_span.sp(Value::Args);
            } else if let Some(mod_span) = self.try_exact(Token::DoubleColon) {
                // Modify
                if !matches!(left.value, Value::Args) {
                    return Err(left
                        .span
                        .union(mod_span)
                        .sp(ParseError::CannotSet(left.value.type_name())));
                }
                let mut indices = self.args()?;
                let Some(last_arg) = indices.last() else {
                    return Err(self.expected("arguments"));
                };
                let full_span = left.span.union(last_arg.span);
                let f_val = indices.remove(0);
                let mut args = self.args_stack.pop().expect("stack empty when modifying");
                for nval in indices {
                    let n = nval.value.expect_number("index", nval.span)?.abs() as usize;
                    if let Some(value) = args.get_mut(n) {
                        *value = call(f_val.clone(), vec![value.clone()], &mut self.args_stack)?;
                    } else {
                        return Err(nval.span.sp(ParseError::IndexOutOfBounds {
                            index: n,
                            len: args.len(),
                        }));
                    }
                }
                self.args_stack.push(args);
                left = full_span.sp(Value::Args);
            } else {
                break;
            }
        }
        Ok(Some(left))
    }
    fn try_call(&mut self) -> ParseResult<Option<Sp<Value>>> {
        let Some(term) = self.try_term()? else {
            return Ok(None);
        };
        let args = self.args()?;
        call(term, args, &mut self.args_stack).map(Some)
    }
    fn args(&mut self) -> ParseResult<Vec<Sp<Value>>> {
        let mut args = Vec::new();
        while let Some(arg) = self.try_term()? {
            match arg.value {
                Value::Args => {
                    args.extend(self.args_stack.pop().unwrap());
                }
                _ => args.push(arg),
            }
        }
        Ok(args)
    }
    fn try_term(&mut self) -> ParseResult<Option<Sp<Value>>> {
        Ok(Some(if let Some(ident) = self.ident() {
            if let Some(value) = self.find_binding(&ident.value) {
                value.map(Clone::clone)
            } else if BUILTINS.contains_key(&ident.value) {
                ident.map(Value::BuiltinFn)
            } else if let Some(val) = builtin_constant(&ident.value) {
                ident.span.sp(val)
            } else {
                return Err(ident.map(ParseError::UnknownIdent));
            }
        } else if let Some(num) = self.number()? {
            num.map(Value::Number)
        } else if self.try_exact(Token::OpenParen).is_some() {
            let res = self
                .try_expr()?
                .ok_or_else(|| self.expected("expression"))?;
            self.expect(Token::CloseParen, "`)`")?;
            res
        } else if self.try_exact(Token::Dollar).is_some() {
            self.try_expr()?
                .ok_or_else(|| self.expected("expression"))?
        } else {
            return Ok(None);
        }))
    }
    fn ident(&mut self) -> Option<Sp<String>> {
        self.next_token_map(|token| match token {
            Token::Ident(ident) => Some(ident),
            _ => None,
        })
    }
    fn number(&mut self) -> ParseResult<Option<Sp<f64>>> {
        self.next_token_map(|token| match token {
            Token::Number(num) => Some(num),
            _ => None,
        })
        .map(|num| {
            num.value
                .parse()
                .map(|n| num.span.sp(n))
                .map_err(|_| num.span.sp(ParseError::InvalidNumber(num.value)))
        })
        .transpose()
    }
}

fn call(
    f_val: Sp<Value>,
    args: Vec<Sp<Value>>,
    stack: &mut Vec<Vec<Sp<Value>>>,
) -> ParseResult<Sp<Value>> {
    let f_name = match f_val.value {
        Value::BuiltinFn(name) => name,
        _ if args.is_empty() => return Ok(f_val),
        value => return Err(f_val.span.sp(ParseError::CannotCall(value.type_name()))),
    };
    let (arg_count, f) = &BUILTINS[&f_name];
    if !arg_count.matches(args.len()) {
        return Err(f_val.span.sp(ParseError::WrongNumberOfArguments {
            name: f_name,
            found: args.len(),
        }));
    }
    Ok(f_val.span.sp(f(args, f_val.span, stack)?))
}
