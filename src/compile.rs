use std::{collections::HashMap, fmt};

use hodaun::{Letter, Octave, Stereo};

use crate::{
    builtin::{builtin_constant, BUILTINS},
    lex::*,
    node::*,
    value::Value,
};

pub struct Compiled {
    pub root: NodeBox,
    pub initial_time: f64,
    pub tempo: f64,
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
    IndexOutOfBounds {
        index: usize,
        len: usize,
    },
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
            CompileError::IndexOutOfBounds { index, len } => {
                write!(f, "Index {index} out of bounds for length {len}")
            }
            CompileError::MistmatchedLengths { a, b } => {
                write!(f, "Mismatched lengths: {a} and {b}")
            }
        }
    }
}

pub type CompileResult<T = ()> = Result<T, Sp<CompileError>>;

pub fn compile(input: &str) -> CompileResult<Compiled> {
    let tokens = lex(input).map_err(|e| e.map(CompileError::InvalidCharacter))?;
    let mut compiler = Compiler {
        tokens,
        curr: 0,
        scopes: vec![Scope {
            bindings: HashMap::new(),
        }],
        last_octave: 3,
        last_val: 0.0.into(),
        outputs: Vec::new(),
    };
    while compiler.exact(Token::Newline).is_some() {}
    while compiler.item()? {
        while compiler.exact(Token::Newline).is_some() {}
    }
    let initial_time = compiler
        .find_binding("initial_time")
        .map(|val| val.val.expect_number("initial_time", val.span))
        .transpose()?
        .unwrap_or(0.0);
    let tempo = compiler
        .find_binding("tempo")
        .map(|val| val.val.expect_number("tempo", val.span))
        .transpose()?
        .unwrap_or(120.0);
    let root = NodeBox::new(state_node(
        format!("{:?}", compiler.outputs),
        compiler.outputs,
        |outputs, env| {
            outputs
                .iter_mut()
                .fold(Stereo::ZERO, |acc, node| acc + node.sample(env))
        },
    ));
    if compiler.curr < compiler.tokens.len() {
        return Err(compiler.tokens[compiler.curr]
            .span
            .sp(CompileError::Expected("end of file")));
    }
    Ok(Compiled {
        root,
        initial_time,
        tempo,
    })
}

struct Compiler {
    tokens: Vec<Sp<Token>>,
    curr: usize,
    scopes: Vec<Scope>,
    last_octave: Octave,
    last_val: Value,
    outputs: Vec<NodeBox>,
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
            .and_then(|sp| f(sp.val).map(|val| sp.span.sp(val)))?;
        self.curr += 1;
        Some(token)
    }
    fn exact(&mut self, token: impl Into<Token>) -> Option<Span> {
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
        if self.exact(token.into()).is_some() {
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
            .map(|val| val.span.sp(&val.val))
    }
    fn item(&mut self) -> CompileResult<bool> {
        Ok(if self.binding()? {
            true
        } else if let Some(value) = self.expr()? {
            self.outputs.push(value.val.into_node());
            true
        } else {
            false
        })
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
        let value = self.expr()?.ok_or_else(|| self.expected("expression"))?;
        self.scopes
            .last_mut()
            .unwrap()
            .bindings
            .insert(name.val, value);
        Ok(true)
    }
    fn expr(&mut self) -> CompileResult<Option<Sp<Value>>> {
        self.as_expr()
    }
    fn as_expr(&mut self) -> CompileResult<Option<Sp<Value>>> {
        let Some(mut left) = self.md_expr()? else {
            return Ok(None);
        };
        while let Some(op) = self.next_token_map(|token| match token {
            Token::BinOp(op @ (BinOp::Add | BinOp::Sub)) => Some(op),
            _ => None,
        }) {
            let right = self.md_expr()?.ok_or_else(|| self.expected("term"))?;
            let span = left.span.union(right.span);
            left = span.sp(match op.val {
                BinOp::Add => left.val.add(right.val, op.span)?,
                BinOp::Sub => left.val.sub(right.val, op.span)?,
                _ => unreachable!(),
            });
        }
        Ok(Some(left))
    }
    fn md_expr(&mut self) -> CompileResult<Option<Sp<Value>>> {
        let Some(mut left) = self.cmp_op()? else {
            return Ok(None);
        };

        while let Some(op) = self.next_token_map(|token| match token {
            Token::BinOp(op @ (BinOp::Mul | BinOp::Div)) => Some(op),
            _ => None,
        }) {
            let right = self.cmp_op()?.ok_or_else(|| self.expected("term"))?;
            let span = left.span.union(right.span);
            left = span.sp(match op.val {
                BinOp::Mul => left.val.mul(right.val, op.span)?,
                BinOp::Div => left.val.div(right.val, op.span)?,
                _ => unreachable!(),
            });
        }
        Ok(Some(left))
    }
    fn cmp_op(&mut self) -> CompileResult<Option<Sp<Value>>> {
        let Some(mut left) = self.call()? else {
            return Ok(None);
        };

        while let Some(op) = self.next_token_map(|token| match token {
            Token::BinOp(op @ (BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge)) => Some(op),
            _ => None,
        }) {
            let right = self.call()?.ok_or_else(|| self.expected("term"))?;
            let span = left.span.union(right.span);
            left = span.sp(match op.val {
                BinOp::Lt => left.val.lt(right.val, op.span)?,
                BinOp::Le => left.val.le(right.val, op.span)?,
                BinOp::Gt => left.val.gt(right.val, op.span)?,
                BinOp::Ge => left.val.ge(right.val, op.span)?,
                _ => unreachable!(),
            });
        }
        Ok(Some(left))
    }
    fn call(&mut self) -> CompileResult<Option<Sp<Value>>> {
        let Some(term) = self.bracket_list()? else {
            return Ok(None);
        };
        let args = self.args()?;
        call(term, args).map(Some)
    }
    fn args(&mut self) -> CompileResult<Vec<Sp<Value>>> {
        let mut args = Vec::new();
        while let Some(arg) = self.bracket_list()? {
            args.push(arg);
        }
        Ok(args)
    }
    fn bracket_list(&mut self) -> CompileResult<Option<Sp<Value>>> {
        if let Some(span) = self.exact(Token::OpenBracket) {
            let start = span;
            let mut end = span;
            let mut items = Vec::new();
            while let Some(item) = self.bar_list()? {
                items.push(item.val);
                end = item.span;
            }
            self.expect(Token::CloseBracket, "`]`")?;
            Ok(Some(start.union(end).sp(Value::List(items))))
        } else {
            self.bar_list()
        }
    }
    fn bar_list(&mut self) -> CompileResult<Option<Sp<Value>>> {
        if let Some(span) = self.exact(Token::Bar) {
            let start = span;
            let mut end = span;
            let mut items = Vec::new();
            while let Some(item) = self.term()? {
                items.push(item.val);
                end = item.span;
            }
            Ok(Some(start.union(end).sp(Value::List(items))))
        } else {
            self.term()
        }
    }
    fn term(&mut self) -> CompileResult<Option<Sp<Value>>> {
        let val = if let Some(ident) = self.ident() {
            if let Some(value) = self.find_binding(&ident.val) {
                value.map(Clone::clone)
            } else if BUILTINS.contains_key(&ident.val) {
                ident.map(Value::BuiltinFn)
            } else if let Some(val) = builtin_constant(&ident.val) {
                ident.span.sp(val)
            } else if let Some((letter, oct)) = parse_note(&ident.val) {
                let freq = letter.frequency(oct);
                self.last_octave = oct;
                ident.span.sp(Value::Number(freq))
            } else if let Some(letter) = parse_letter(&ident.val) {
                let freq = letter.frequency(self.last_octave);
                ident.span.sp(Value::Number(freq))
            } else if let Some(value) = parse_hex(&ident.val) {
                ident.span.sp(value)
            } else {
                return Err(ident.map(CompileError::UnknownIdent));
            }
        } else if let Some(num) = self.number()? {
            num.map(Value::Number)
        } else if self.exact(Token::OpenParen).is_some() {
            let res = self.expr()?.ok_or_else(|| self.expected("expression"))?;
            self.expect(Token::CloseParen, "`)`")?;
            res
        } else if let Some(span) = self.exact(Token::Tilde) {
            span.sp(self.last_val.clone())
        } else if self.exact(Token::Period).is_some() {
            self.call()?.ok_or_else(|| self.expected("expression"))?
        } else {
            return Ok(None);
        };
        self.last_val = val.val.clone();
        Ok(Some(val))
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
            num.val
                .parse()
                .map(|n| num.span.sp(n))
                .map_err(|_| num.span.sp(CompileError::InvalidNumber(num.val)))
        })
        .transpose()
    }
}

pub fn call(f_val: Sp<Value>, mut args: Vec<Sp<Value>>) -> CompileResult<Sp<Value>> {
    let f_name = match f_val.val {
        Value::BuiltinFn(name) => name,
        Value::Bind(f, mut bind_args) => {
            bind_args.append(&mut args);
            return call(*f, bind_args);
        }
        _ => {
            let mut value = f_val.val;
            let mut span = f_val.span;
            if !args.is_empty() {
                let b = args.remove(0);
                span = span.union(b.span);
                let b = call(b, args)?;
                value = value.bin_scalar_op(b.val, "*", b.span, |a, b| a * b)?;
            }
            return Ok(span.sp(value));
        }
    };
    let (arg_count, f) = &BUILTINS[&f_name];
    if args.len() < arg_count.min {
        return Ok(f_val.span.sp(Value::Bind(
            f_val.span.sp(Value::BuiltinFn(f_name)).into(),
            args,
        )));
    }
    if !arg_count.matches(args.len()) {
        return Err(f_val.span.sp(CompileError::WrongNumberOfArguments {
            name: f_name,
            found: args.len(),
        }));
    }
    Ok(f_val.span.sp(f(args, f_val.span)?))
}

fn parse_letter(name: &str) -> Option<Letter> {
    Some(match name {
        "A" => Letter::A,
        "Ab" => Letter::Ab,
        "A#" => Letter::Ash,
        "B" => Letter::B,
        "Bb" => Letter::Bb,
        "C" => Letter::C,
        "C#" => Letter::Csh,
        "D" => Letter::D,
        "Db" => Letter::Db,
        "D#" => Letter::Dsh,
        "E" => Letter::E,
        "Eb" => Letter::Eb,
        "F" => Letter::F,
        "F#" => Letter::Fsh,
        "G" => Letter::G,
        "Gb" => Letter::Gb,
        "G#" => Letter::Gsh,
        _ => return None,
    })
}

fn parse_note(name: &str) -> Option<(Letter, Octave)> {
    if !name.ends_with(|c: char| c.is_ascii_digit()) {
        return None;
    };
    let octave: Octave = name[name.len() - 1..].parse().unwrap();
    let letter = &name[..name.len() - 1];
    let letter = parse_letter(letter)?;
    Some((letter, octave))
}

fn parse_hex(name: &str) -> Option<Value> {
    let name = name.strip_prefix('x')?;
    let mut values = Vec::new();
    for c in name.chars() {
        let n = if c.is_ascii_digit() {
            c as u8 - b'0'
        } else if ('a'..='f').contains(&c) {
            c as u8 - b'a' + 10
        } else if ('A'..='F').contains(&c) {
            c as u8 - b'A' + 10
        } else if c == '_' {
            continue;
        } else {
            return None;
        };
        for i in (0..4).rev() {
            values.push(Value::Number(((n >> (3 - i)) & 1) as f64));
        }
    }
    Some(Value::List(values))
}
