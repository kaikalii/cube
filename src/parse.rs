use std::collections::HashMap;

use crate::{lex::*, node::Node};

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
    todo!()
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
    fn item(&mut self) -> ParseResult {
        Ok(())
    }
    fn binding(&mut self) -> ParseResult {
        let Some(name) = self.ident() else {
            return Ok(())
        };
        self.expect(Token::Equals, "equals")?;
        Ok(())
    }
    fn expr(&mut self) -> ParseResult<Option<Value>> {
        todo!()
    }
    fn as_expr(&mut self) -> ParseResult<Option<Value>> {
        todo!()
    }
    fn md_expr(&mut self) -> ParseResult<Option<Value>> {
        let Some(left) = self.try_term()? else {
            return Ok(None);
        };
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
    Node(Node),
}

impl Value {
    fn bin_op(self, op: BinOp, other: Self) -> Self {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(op.eval(a, b)),
            (Value::Number(a), Value::Node(b)) => todo!(),
            (Value::Node(_), Value::Number(_)) => todo!(),
            (Value::Node(_), Value::Node(_)) => todo!(),
        }
    }
}

impl BinOp {
    fn eval(self, left: f64, right: f64) -> f64 {
        match self {
            BinOp::Add => left + right,
            BinOp::Sub => left - right,
            BinOp::Mul => left * right,
            BinOp::Div => left / right,
        }
    }
}
