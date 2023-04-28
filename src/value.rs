#![allow(clippy::redundant_closure)]

use std::{cmp::Ordering, fmt, ops::*};

use hodaun::Stereo;

use crate::{
    compile::{CompileError, CompileResult},
    lex::{Sp, Span},
    node::*,
};

#[derive(Clone)]
pub enum Value {
    Number(f64),
    #[allow(dead_code)]
    Node(NodeBox),
    BuiltinFn(String),
    Bind(Box<Sp<Self>>, Vec<Sp<Self>>),
    List(Vec<Self>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Node(node) => write!(f, "{node:?}"),
            Value::BuiltinFn(name) => write!(f, "{name}"),
            Value::Bind(func, args) => write!(f, "{func:?}{args:?}"),
            Value::List(_) => write!(f, "list"),
        }
    }
}

impl Node for Value {
    fn boxed(&self) -> NodeBox {
        match self {
            Value::Number(n) => NodeBox::new(*n),
            Value::Node(node) => node.clone(),
            Value::BuiltinFn(_) | Value::Bind(..) => NodeBox::new(0.0),
            Value::List(items) => NodeBox::new(state_node("list", items.clone(), |items, env| {
                items
                    .iter_mut()
                    .fold(Stereo::ZERO, |acc, item| acc + item.sample(env))
            })),
        }
    }
    fn sample(&mut self, env: &mut Env) -> Stereo {
        match self {
            Value::Number(n) => Stereo::both(*n),
            Value::Node(node) => node.sample(env),
            Value::BuiltinFn(_) | Value::Bind(..) => Stereo::ZERO,
            Value::List(items) => items
                .iter_mut()
                .fold(Stereo::ZERO, |acc, item| acc + item.sample(env)),
        }
    }
}

impl Value {
    pub fn into_node(self) -> NodeBox {
        match self {
            Value::Node(node) => node,
            _ => self.boxed(),
        }
    }
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::Node(_) => "node",
            Value::BuiltinFn(_) => "builtin function",
            Value::Bind(_, _) => "bound function",
            Value::List(_) => "list",
        }
    }
    pub fn into_list(self) -> Vec<Self> {
        match self {
            Value::List(list) => list.into_iter().collect(),
            value => vec![value],
        }
    }
    pub fn expect_natural(&self, name: &'static str, span: Span) -> CompileResult<usize> {
        match self {
            Value::Number(n) if *n >= 0.0 && n.fract() == 0.0 => Ok(*n as usize),
            _ => Err(span.sp(CompileError::ExpectedNatural(name))),
        }
    }
    pub fn expect_number(&self, name: &'static str, span: Span) -> CompileResult<f64> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err(span.sp(CompileError::ExpectedNumber(name))),
        }
    }
    pub fn distribute<F, V>(self, f: F) -> Self
    where
        F: Fn(Self) -> V + Clone,
        V: Into<Self>,
    {
        match self {
            Value::List(list) => {
                Value::List(list.into_iter().map(|v| v.distribute(f.clone())).collect())
            }
            value => f(value).into(),
        }
    }
    pub fn un_scalar_op(
        self,
        op_name: &'static str,
        span: Span,
        f: impl Fn(f64) -> f64 + Clone + Send + Sync + 'static,
    ) -> CompileResult<Self> {
        Ok(match self {
            Value::Number(n) => Value::Number(f(n)),
            Value::Node(node) => Value::Node(NodeBox::new(state_node(
                format!("{op_name} {node:?}"),
                node,
                move |node, env| node.sample(env).map(|v| f(v)),
            ))),
            Value::BuiltinFn(_) | Value::Bind(..) => {
                return Err(span.sp(CompileError::InvalidUnaryOperation {
                    op: op_name,
                    operand: self.type_name(),
                }))
            }
            Value::List(items) => Value::List(
                items
                    .into_iter()
                    .map(|v| v.un_scalar_op(op_name, span, f.clone()))
                    .collect::<CompileResult<_>>()?,
            ),
        })
    }
    pub fn bin_scalar_op(
        self,
        other: Self,
        op_name: &'static str,
        span: Span,
        f: impl Fn(f64, f64) -> f64 + Clone + Send + Sync + 'static,
    ) -> CompileResult<Self> {
        Ok(match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(f(a, b)),
            (Value::Number(a), Value::Node(b)) => Value::Node(NodeBox::new(state_node(
                format!("({a} {op_name} {b:?})"),
                b,
                move |b, env| b.sample(env).map(|b| f(a, b)),
            ))),
            (Value::Node(a), Value::Number(b)) => Value::Node(NodeBox::new(state_node(
                format!("({a:?} {op_name} {b})"),
                a,
                move |a, env| a.sample(env).map(|a| f(a, b)),
            ))),
            (Value::Node(a), Value::Node(b)) => Value::Node(NodeBox::new(state_node(
                format!("({a:?} {op_name} {b:?})"),
                (a, b),
                move |(a, b), env| a.sample(env).with(b.sample(env), |a, b| f(a, b)),
            ))),
            (Value::List(a), Value::List(b)) => {
                if a.len() != b.len() {
                    return Err(span.sp(CompileError::MistmatchedLengths {
                        a: a.len(),
                        b: b.len(),
                    }));
                }
                Value::List(
                    a.into_iter()
                        .zip(b)
                        .map(|(a, b)| a.bin_scalar_op(b, op_name, span, f.clone()))
                        .collect::<CompileResult<_>>()?,
                )
            }
            (Value::List(a), b) => Value::List(
                a.into_iter()
                    .map(|a| a.bin_scalar_op(b.clone(), op_name, span, f.clone()))
                    .collect::<CompileResult<_>>()?,
            ),
            (a, Value::List(b)) => Value::List(
                b.into_iter()
                    .map(|b| a.clone().bin_scalar_op(b, op_name, span, f.clone()))
                    .collect::<CompileResult<_>>()?,
            ),
            (a, b) => {
                return Err(span.sp(CompileError::InvalidBinaryOperation {
                    a: a.type_name(),
                    b: b.type_name(),
                    op: op_name,
                }))
            }
        })
    }
    pub fn add(self, other: Self, span: Span) -> CompileResult<Self> {
        self.bin_scalar_op(other, "+", span, Add::add)
    }
    pub fn sub(self, other: Self, span: Span) -> CompileResult<Self> {
        self.bin_scalar_op(other, "-", span, Sub::sub)
    }
    pub fn mul(self, other: Self, span: Span) -> CompileResult<Self> {
        self.bin_scalar_op(other, "*", span, Mul::mul)
    }
    pub fn div(self, other: Self, span: Span) -> CompileResult<Self> {
        self.bin_scalar_op(other, "/", span, Div::div)
    }
    pub fn lt(self, other: Self, span: Span) -> CompileResult<Self> {
        self.bin_scalar_op(other, "<", span, |a, b| {
            (cmp(a, b) == Ordering::Less) as u8 as f64
        })
    }
    pub fn le(self, other: Self, span: Span) -> CompileResult<Self> {
        self.bin_scalar_op(other, "<=", span, |a, b| {
            (cmp(a, b) != Ordering::Greater) as u8 as f64
        })
    }
    pub fn gt(self, other: Self, span: Span) -> CompileResult<Self> {
        self.bin_scalar_op(other, ">", span, |a, b| {
            (cmp(a, b) == Ordering::Greater) as u8 as f64
        })
    }
    pub fn ge(self, other: Self, span: Span) -> CompileResult<Self> {
        self.bin_scalar_op(other, ">=", span, |a, b| {
            (cmp(a, b) != Ordering::Less) as u8 as f64
        })
    }
}

fn cmp(a: f64, b: f64) -> Ordering {
    a.partial_cmp(&b)
        .unwrap_or_else(|| a.is_nan().cmp(&b.is_nan()))
}

impl From<i64> for Value {
    fn from(n: i64) -> Self {
        Value::Number(n as f64)
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Number(n)
    }
}

impl From<NodeBox> for Value {
    fn from(node: NodeBox) -> Self {
        Value::Node(node)
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
