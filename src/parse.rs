use std::{
    collections::HashMap,
    fmt,
    ops::{Add, Div, Mul, Sub},
};

use crate::{builtin::BUILTINS, lex::*, node::*, vector::Vector};

#[derive(Debug)]
pub enum ParseError {
    InvalidCharacter(char),
    Expected(&'static str),
    InvalidNumber(String),
    UnknownIdent(String),
    InvalidOperation {
        a: &'static str,
        b: &'static str,
        op: &'static str,
    },
    CannotCall(&'static str),
    WrongNumberOfArguments {
        name: String,
        found: usize,
    },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::InvalidCharacter(c) => write!(f, "Invalid character: {}", c),
            ParseError::Expected(expectation) => write!(f, "Expected {}", expectation),
            ParseError::InvalidNumber(s) => write!(f, "Invalid number: {}", s),
            ParseError::UnknownIdent(s) => write!(f, "Unknown identifier: {}", s),
            ParseError::InvalidOperation { a, b, op } => {
                write!(f, "Invalid operation: {} {} {}", a, op, b)
            }
            ParseError::CannotCall(name) => write!(f, "Cannot call {name}"),
            ParseError::WrongNumberOfArguments { name, found } => {
                write!(f, "No variant of {name} takes {found} arguments")
            }
        }
    }
}

pub type ParseResult<T = ()> = Result<T, ParseError>;

pub fn parse(input: &str) -> ParseResult<Option<NodeBox>> {
    let tokens = lex(input).map_err(ParseError::InvalidCharacter)?;
    let mut parser = Parser {
        tokens,
        curr: 0,
        scopes: vec![Scope {
            bindings: HashMap::new(),
        }],
    };
    while parser.item()? {}
    Ok(parser.try_expr()?.map(Value::into_node))
}

struct Parser {
    tokens: Vec<Token>,
    curr: usize,
    scopes: Vec<Scope>,
}

struct Scope {
    bindings: HashMap<String, Value>,
}

impl Parser {
    fn next_token_map<T>(&mut self, f: impl FnOnce(Token) -> Option<T>) -> Option<T> {
        let token = self.tokens.get(self.curr).cloned().and_then(f)?;
        self.curr += 1;
        Some(token)
    }
    fn try_exact(&mut self, token: impl Into<Token>) -> bool {
        self.next_token_map(|t| if t == token.into() { Some(()) } else { None })
            .is_some()
    }
    fn expect(&mut self, token: impl Into<Token>, expectation: &'static str) -> ParseResult {
        if self.try_exact(token.into()) {
            Ok(())
        } else {
            Err(ParseError::Expected(expectation))
        }
    }
    fn find_binding(&self, name: &str) -> Option<&Value> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.bindings.get(name))
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
        let value = self.try_expr()?.ok_or(ParseError::Expected("expression"))?;
        self.scopes.last_mut().unwrap().bindings.insert(name, value);
        Ok(true)
    }
    fn try_expr(&mut self) -> ParseResult<Option<Value>> {
        self.try_as_expr()
    }
    fn try_as_expr(&mut self) -> ParseResult<Option<Value>> {
        let Some(left) = self.try_md_expr()? else {
            return Ok(None);
        };
        Ok(Some(
            if let Some(op) = self.next_token_map(|token| match token {
                Token::BinOp(BinOp::Add) => Some(BinOp::Add),
                Token::BinOp(BinOp::Sub) => Some(BinOp::Sub),
                _ => None,
            }) {
                let right = self.try_as_expr()?.ok_or(ParseError::Expected("term"))?;
                match op {
                    BinOp::Add => left.bin_op(right, "+", Add::add)?,
                    BinOp::Sub => left.bin_op(right, "-", Sub::sub)?,
                    _ => unreachable!(),
                }
            } else {
                left
            },
        ))
    }
    fn try_md_expr(&mut self) -> ParseResult<Option<Value>> {
        let Some(left) = self.try_call()? else {
            return Ok(None);
        };
        Ok(Some(
            if let Some(op) = self.next_token_map(|token| match token {
                Token::BinOp(BinOp::Mul) => Some(BinOp::Mul),
                Token::BinOp(BinOp::Div) => Some(BinOp::Div),
                _ => None,
            }) {
                let right = self.try_md_expr()?.ok_or(ParseError::Expected("term"))?;
                match op {
                    BinOp::Mul => left.bin_op(right, "*", Mul::mul)?,
                    BinOp::Div => left.bin_op(right, "/", Div::div)?,
                    _ => unreachable!(),
                }
            } else {
                left
            },
        ))
    }
    fn try_call(&mut self) -> ParseResult<Option<Value>> {
        let Some(term) = self.try_term()? else {
            return Ok(None);
        };
        let mut args = Vec::new();
        while let Some(arg) = self.try_term()? {
            args.push(arg);
        }
        Ok(Some(if args.is_empty() {
            term
        } else {
            let f_name = match term {
                Value::BuiltinFn(name) => name,
                value => return Err(ParseError::CannotCall(value.type_name())),
            };
            let f_overrides = &BUILTINS[&f_name];
            let f = f_overrides
                .get(&args.len())
                .ok_or(ParseError::WrongNumberOfArguments {
                    name: f_name,
                    found: args.len(),
                })?;
            f(args)?
        }))
    }
    fn try_term(&mut self) -> ParseResult<Option<Value>> {
        Ok(Some(if let Some(ident) = self.ident() {
            if let Some(value) = self.find_binding(&ident) {
                value.clone()
            } else if BUILTINS.contains_key(&ident) {
                Value::BuiltinFn(ident)
            } else {
                return Err(ParseError::UnknownIdent(ident));
            }
        } else if let Some(num) = self.number()? {
            Value::Number(num)
        } else if self.try_exact(Token::OpenParen) {
            let res = self.try_expr()?.ok_or(ParseError::Expected("expression"))?;
            self.expect(Token::CloseParen, "`)`")?;
            res
        } else {
            return Ok(None);
        }))
    }
    fn ident(&mut self) -> Option<String> {
        self.next_token_map(|token| match token {
            Token::Ident(ident) => Some(ident),
            _ => None,
        })
    }
    fn number(&mut self) -> ParseResult<Option<f64>> {
        self.next_token_map(|token| match token {
            Token::Number(num) => Some(num),
            _ => None,
        })
        .map(|num| num.parse().map_err(|_| ParseError::InvalidNumber(num)))
        .transpose()
    }
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    #[allow(dead_code)]
    Node(NodeBox),
    BuiltinFn(String),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Node(node) => write!(f, "{node:?}"),
            Value::BuiltinFn(name) => write!(f, "{name}"),
        }
    }
}

impl Node for Value {
    fn boxed(&self) -> NodeBox {
        match self {
            Value::Number(n) => NodeBox::new(constant_scalar_node(*n)),
            Value::Node(node) => node.clone(),
            Value::BuiltinFn(_) => NodeBox::new(constant_scalar_node(0.0)),
        }
    }
    fn sample(&mut self, sample_rate: f64, pos: Vector, dir: Vector) -> Vector {
        match self {
            Value::Number(n) => Vector::splat(*n),
            Value::Node(node) => node.sample(sample_rate, pos, dir),
            Value::BuiltinFn(_) => Vector::ZERO,
        }
    }
}

impl Value {
    fn into_node(self) -> NodeBox {
        match self {
            Value::Node(node) => node,
            _ => self.boxed(),
        }
    }
    fn type_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::Node(_) => "node",
            Value::BuiltinFn(_) => "builtin function",
        }
    }
    pub fn bin_op(
        self,
        other: Self,
        op_name: &'static str,
        f: impl Fn(f64, f64) -> f64 + Clone + Send + Sync + 'static,
    ) -> ParseResult<Self> {
        Ok(match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(f(a, b)),
            (Value::Number(a), Value::Node(b)) => Value::Node(NodeBox::new(state_node(
                format!("{a} {op_name} {b:?}"),
                b,
                move |b, sample_rate, pos, dir| b.sample(sample_rate, pos, dir).map(|b| f(a, b)),
            ))),
            (Value::Node(a), Value::Number(b)) => Value::Node(NodeBox::new(state_node(
                format!("{a:?} {op_name} {b}"),
                a,
                move |a, sample_rate, pos, dir| a.sample(sample_rate, pos, dir).map(|a| f(a, b)),
            ))),
            #[allow(clippy::redundant_closure)]
            (Value::Node(a), Value::Node(b)) => Value::Node(NodeBox::new(state_node(
                format!("{a:?} {op_name} {b:?}"),
                (a, b),
                move |(a, b), sample_rate, pos, dir| {
                    a.sample(sample_rate, pos, dir)
                        .with(b.sample(sample_rate, pos, dir), |a, b| f(a, b))
                },
            ))),
            (a, b) => {
                return Err(ParseError::InvalidOperation {
                    a: a.type_name(),
                    b: b.type_name(),
                    op: op_name,
                })
            }
        })
    }
}

impl From<Wave3> for Value {
    fn from(wave: Wave3) -> Self {
        Value::Node(NodeBox::new(wave))
    }
}

impl<F, S> From<GenericNode<F, S>> for Value
where
    GenericNode<F, S>: Node,
{
    fn from(node: GenericNode<F, S>) -> Self {
        Value::Node(NodeBox::new(node))
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Number(n)
    }
}
