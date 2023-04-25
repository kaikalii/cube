use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt,
    ops::{Add, Div, Mul, Sub},
};

use crate::{
    builtin::{builtin_constant, BUILTINS},
    lex::*,
    node::*,
    value::Value,
};

pub struct Cube {
    pub root: NodeBox,
    pub initial_pos: f64,
    pub tempo: f64,
    pub octave: i8,
}

#[derive(Debug)]
pub enum CompileError {
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
    WrongNumberOfArguments {
        name: String,
        found: usize,
    },
    ExpectedNatural(&'static str),
    ExpectedNumber(&'static str),
    ExpectedVector(&'static str),
    IndexOutOfBounds {
        index: usize,
        len: usize,
    },
    CannotSet(&'static str),
    MistmatchedLengths {
        a: usize,
        b: usize,
    },
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::InvalidCharacter(c) => write!(f, "Invalid character: {}", c),
            CompileError::Expected(expectation) => write!(f, "Expected {}", expectation),
            CompileError::InvalidNumber(s) => write!(f, "Invalid number: {}", s),
            CompileError::UnknownIdent(s) => write!(f, "Unknown identifier: {}", s),
            CompileError::InvalidUnaryOperation { op, operand } => {
                write!(f, "Invalid operation: {} {}", op, operand)
            }
            CompileError::InvalidBinaryOperation { a, b, op } => {
                write!(f, "Invalid operation: {} {} {}", a, op, b)
            }
            CompileError::WrongNumberOfArguments { name, found } => {
                write!(f, "No variant of {name} takes {found} arguments")
            }
            CompileError::ExpectedNatural(name) => {
                write!(f, "Expected {name} to be a natural number")
            }
            CompileError::ExpectedNumber(name) => write!(f, "Expected {name} to be a number"),
            CompileError::ExpectedVector(name) => write!(f, "Expected {name} to be a vector"),
            CompileError::IndexOutOfBounds { index, len } => {
                write!(f, "Index {index} out of bounds for length {len}")
            }
            CompileError::CannotSet(name) => write!(f, "Cannot set {name}"),
            CompileError::MistmatchedLengths { a, b } => {
                write!(f, "Mismatched lengths: {a} and {b}")
            }
        }
    }
}

pub type CompileResult<T = ()> = Result<T, Sp<CompileError>>;

pub fn compile(input: &str) -> CompileResult<Cube> {
    let tokens = lex(input).map_err(|e| e.map(CompileError::InvalidCharacter))?;
    let mut compiler = Compiler {
        tokens,
        curr: 0,
        scopes: vec![Scope {
            bindings: HashMap::new(),
        }],
    };
    while compiler.try_exact(Token::Newline).is_some() {}
    while compiler.item()? {
        while compiler.try_exact(Token::Newline).is_some() {}
    }
    let root = compiler
        .try_expr()?
        .map(|val| val.value.into_node())
        .unwrap_or_else(|| NodeBox::new(0.0));
    while compiler.try_exact(Token::Newline).is_some() {}
    let initial_time = compiler
        .find_binding("initial_time")
        .map(|val| val.value.expect_number("initial_time", val.span))
        .transpose()?
        .unwrap_or(0.0);
    let tempo = compiler
        .find_binding("tempo")
        .map(|val| val.value.expect_number("tempo", val.span))
        .transpose()?
        .unwrap_or(120.0);
    if compiler.curr < compiler.tokens.len() {
        return Err(compiler.tokens[compiler.curr]
            .span
            .sp(CompileError::Expected("end of file")));
    }
    Ok(Cube {
        root,
        initial_pos: initial_time,
        tempo,
        octave: 3,
    })
}

struct Compiler {
    tokens: Vec<Sp<Token>>,
    curr: usize,
    scopes: Vec<Scope>,
}

struct Scope {
    bindings: HashMap<String, Sp<Value>>,
}

impl Compiler {
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
    fn expect(&mut self, token: impl Into<Token>, expectation: &'static str) -> CompileResult {
        if self.try_exact(token.into()).is_some() {
            Ok(())
        } else {
            Err(self.expected(expectation))
        }
    }
    fn expected(&self, expectation: &'static str) -> Sp<CompileError> {
        self.last_span().sp(CompileError::Expected(expectation))
    }
    fn find_binding(&self, name: &str) -> Option<Sp<&Value>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.bindings.get(name))
            .map(|val| val.span.sp(&val.value))
    }
    fn item(&mut self) -> CompileResult<bool> {
        self.binding()
    }
    fn binding(&mut self) -> CompileResult<bool> {
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
    fn try_expr(&mut self) -> CompileResult<Option<Sp<Value>>> {
        self.try_as_expr()
    }
    fn try_as_expr(&mut self) -> CompileResult<Option<Sp<Value>>> {
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
    fn try_md_expr(&mut self) -> CompileResult<Option<Sp<Value>>> {
        let Some(mut left) = self.try_cmp_op()? else {
            return Ok(None);
        };

        while let Some(op) = self.next_token_map(|token| match token {
            Token::BinOp(BinOp::Mul) => Some(BinOp::Mul),
            Token::BinOp(BinOp::Div) => Some(BinOp::Div),
            _ => None,
        }) {
            let right = self.try_cmp_op()?.ok_or_else(|| self.expected("term"))?;
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
    fn try_cmp_op(&mut self) -> CompileResult<Option<Sp<Value>>> {
        let Some(mut left) = self.try_set()? else {
            return Ok(None);
        };

        while let Some(op) = self.next_token_map(|token| match token {
            Token::BinOp(BinOp::Le) => Some(BinOp::Le),
            Token::BinOp(BinOp::Lt) => Some(BinOp::Lt),
            Token::BinOp(BinOp::Ge) => Some(BinOp::Ge),
            Token::BinOp(BinOp::Gt) => Some(BinOp::Gt),
            _ => None,
        }) {
            let right = self.try_set()?.ok_or_else(|| self.expected("term"))?;
            let span = left.span.union(right.span);
            fn cmp(a: f64, b: f64) -> Ordering {
                a.partial_cmp(&b)
                    .unwrap_or_else(|| a.is_nan().cmp(&b.is_nan()))
            }
            left = span.sp(match op.value {
                BinOp::Le => left
                    .value
                    .bin_scalar_op(right.value, "<=", op.span, |a, b| {
                        (cmp(a, b) == Ordering::Less) as u8 as f64
                    })?,
                BinOp::Lt => left
                    .value
                    .bin_scalar_op(right.value, "<", op.span, |a, b| {
                        (cmp(a, b) != Ordering::Greater) as u8 as f64
                    })?,
                BinOp::Ge => left
                    .value
                    .bin_scalar_op(right.value, ">=", op.span, |a, b| {
                        (cmp(a, b) == Ordering::Greater) as u8 as f64
                    })?,
                BinOp::Gt => left
                    .value
                    .bin_scalar_op(right.value, ">", op.span, |a, b| {
                        (cmp(a, b) != Ordering::Less) as u8 as f64
                    })?,
                _ => unreachable!(),
            });
        }
        Ok(Some(left))
    }
    fn try_set(&mut self) -> CompileResult<Option<Sp<Value>>> {
        let Some(mut left) = self.try_call()? else {
            return Ok(None);
        };
        loop {
            if self.try_exact(Token::DoubleColon).is_some() {
                // Fill
                let value = self
                    .try_call()?
                    .ok_or_else(|| self.expected("expression"))?;
                let full_span = left.span.union(value.span);
                let n = left.value.expect_natural("fill count", left.span)?;
                let args = vec![value.value; n];
                left = full_span.sp(Value::List(args));
            } else if let Some(set_span) = self.try_exact(Token::Colon) {
                // Set
                let Value::List(mut args) = left.value else {
                     return Err(left
                        .span
                        .union(set_span)
                        .sp(CompileError::CannotSet(left.value.type_name())));
                };
                let mut indices = self.args()?;
                let Some(last_arg) = indices.last() else {
                    return Err(self.expected("arguments"));
                };
                let full_span = left.span.union(last_arg.span);
                let value = indices.remove(0).value;
                for nval in indices {
                    let n = nval.value.expect_natural("index", nval.span)?;
                    if let Some(spot) = args.get_mut(n) {
                        *spot = value.clone();
                    } else {
                        return Err(nval.span.sp(CompileError::IndexOutOfBounds {
                            index: n,
                            len: args.len(),
                        }));
                    }
                }
                left = full_span.sp(Value::List(args));
            } else if let Some(mod_span) = self.try_exact(Token::SemiColon) {
                // Modify
                let Value::List(mut args) = left.value else {
                     return Err(left
                        .span
                        .union(mod_span)
                        .sp(CompileError::CannotSet(left.value.type_name())));
                };
                let mut indices = self.args()?;
                let Some(last_arg) = indices.last() else {
                    return Err(self.expected("arguments"));
                };
                let full_span = left.span.union(last_arg.span);
                let f_val = indices.remove(0);
                for nval in indices {
                    let n = nval.value.expect_natural("index", nval.span)?;
                    if let Some(value) = args.get_mut(n) {
                        *value = call(f_val.clone(), vec![left.span.sp(value.clone())])?.value;
                    } else {
                        return Err(nval.span.sp(CompileError::IndexOutOfBounds {
                            index: n,
                            len: args.len(),
                        }));
                    }
                }
                left = full_span.sp(Value::List(args));
            } else {
                break;
            }
        }
        Ok(Some(left))
    }
    fn try_call(&mut self) -> CompileResult<Option<Sp<Value>>> {
        let Some(term) = self.try_bracket_list()? else {
            return Ok(None);
        };
        let bind = self.try_exact(Token::At).is_some();
        let args = self.args()?;
        if bind {
            Ok(Some(term.span.sp(Value::Bind(term.into(), args))))
        } else {
            call(term, args).map(Some)
        }
    }
    fn args(&mut self) -> CompileResult<Vec<Sp<Value>>> {
        let mut args = Vec::new();
        while let Some(arg) = self.try_bracket_list()? {
            args.push(arg);
        }
        Ok(args)
    }
    fn try_bracket_list(&mut self) -> CompileResult<Option<Sp<Value>>> {
        if let Some(span) = self.try_exact(Token::OpenBracket) {
            let start = span;
            let mut end = span;
            let mut items = Vec::new();
            while let Some(item) = self.try_bar_list()? {
                items.push(item.value);
                end = item.span;
            }
            self.expect(Token::CloseBracket, "`]`")?;
            Ok(Some(start.union(end).sp(Value::List(items))))
        } else {
            self.try_bar_list()
        }
    }
    fn try_bar_list(&mut self) -> CompileResult<Option<Sp<Value>>> {
        if let Some(span) = self.try_exact(Token::Bar) {
            let start = span;
            let mut end = span;
            let mut items = Vec::new();
            while let Some(item) = self.try_term()? {
                items.push(item.value);
                end = item.span;
            }
            Ok(Some(start.union(end).sp(Value::List(items))))
        } else {
            self.try_term()
        }
    }
    fn try_term(&mut self) -> CompileResult<Option<Sp<Value>>> {
        Ok(Some(if let Some(ident) = self.ident() {
            if let Some(value) = self.find_binding(&ident.value) {
                value.map(Clone::clone)
            } else if BUILTINS.contains_key(&ident.value) {
                ident.map(Value::BuiltinFn)
            } else if let Some(val) = builtin_constant(&ident.value) {
                ident.span.sp(val)
            } else {
                return Err(ident.map(CompileError::UnknownIdent));
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
    fn number(&mut self) -> CompileResult<Option<Sp<f64>>> {
        self.next_token_map(|token| match token {
            Token::Number(num) => Some(num),
            _ => None,
        })
        .map(|num| {
            num.value
                .parse()
                .map(|n| num.span.sp(n))
                .map_err(|_| num.span.sp(CompileError::InvalidNumber(num.value)))
        })
        .transpose()
    }
}

pub fn call(f_val: Sp<Value>, mut args: Vec<Sp<Value>>) -> CompileResult<Sp<Value>> {
    let f_name = match f_val.value {
        Value::BuiltinFn(name) => name,
        Value::Bind(f, mut bind_args) => {
            bind_args.append(&mut args);
            return call(*f, bind_args);
        }
        _ => {
            let mut value = f_val.value;
            let mut span = f_val.span;
            if !args.is_empty() {
                let b = args.remove(0);
                span = span.union(b.span);
                let b = call(b, args)?;
                value = value.bin_scalar_op(b.value, "*", b.span, |a, b| a * b)?;
            }
            return Ok(span.sp(value));
        }
    };
    let (arg_count, f) = &BUILTINS[&f_name];
    if !arg_count.matches(args.len()) {
        return Err(f_val.span.sp(CompileError::WrongNumberOfArguments {
            name: f_name,
            found: args.len(),
        }));
    }
    Ok(f_val.span.sp(f(args, f_val.span)?))
}
