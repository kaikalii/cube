use std::{
    collections::HashMap,
    ops::{Add, Div, Mul, Sub},
};

use crate::{
    lex::*,
    node::{state_node, Node, NodeBox},
};

pub enum ParseError {
    InvalidCharacter(char),
    Expected(&'static str),
    InvalidNumber(String),
    UnknownIdent(String),
}

pub type ParseResult<T = ()> = Result<T, ParseError>;

pub fn parse(input: &str) -> ParseResult {
    let tokens = lex(input).map_err(ParseError::InvalidCharacter)?;
    let mut parser = Parser {
        tokens,
        curr: 0,
        scopes: vec![Scope {
            bindings: HashMap::new(),
        }],
    };
    while parser.item()? {}
    Ok(())
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
    fn expect(&mut self, token: impl Into<Token>, expectation: &'static str) -> ParseResult {
        let token = token.into();
        if self
            .next_token_map(|t| if t == token { Some(()) } else { None })
            .is_some()
        {
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
        let Some(name) = self.ident() else {
            return Ok(false);
        };
        self.expect(Token::Equals, "equals")?;
        let value = self.expr()?.ok_or(ParseError::Expected("expression"))?;
        self.scopes.last_mut().unwrap().bindings.insert(name, value);
        Ok(true)
    }
    fn expr(&mut self) -> ParseResult<Option<Value>> {
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
                    BinOp::Add => left.bin_op(right, "+", Add::add),
                    BinOp::Sub => left.bin_op(right, "-", Sub::sub),
                    _ => unreachable!(),
                }
            } else {
                left
            },
        ))
    }
    fn try_md_expr(&mut self) -> ParseResult<Option<Value>> {
        let Some(left) = self.try_term()? else {
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
                    BinOp::Mul => left.bin_op(right, "*", Mul::mul),
                    BinOp::Div => left.bin_op(right, "/", Div::div),
                    _ => unreachable!(),
                }
            } else {
                left
            },
        ))
    }
    fn try_term(&mut self) -> ParseResult<Option<Value>> {
        Ok(Some(if let Some(ident) = self.ident() {
            if let Some(value) = self.find_binding(&ident) {
                value.clone()
            } else {
                return Err(ParseError::UnknownIdent(ident));
            }
        } else if let Some(num) = self.number()? {
            Value::Number(num)
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
enum Value {
    Number(f64),
    Node(NodeBox),
}

impl Value {
    fn bin_op(
        self,
        other: Self,
        op_name: &'static str,
        f: impl Fn(f64, f64) -> f64 + Clone + Send + Sync + 'static,
    ) -> Self {
        match (self, other) {
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
        }
    }
}
