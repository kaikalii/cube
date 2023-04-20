#![allow(clippy::redundant_closure)]

use std::fmt;

use crate::{
    node::*,
    parse::{ParseError, ParseResult},
    vector::Vector,
};

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Vector(Vector),
    #[allow(dead_code)]
    Node(NodeBox),
    BuiltinFn(String),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Vector(v) => write!(f, "{v}"),
            Value::Node(node) => write!(f, "{node:?}"),
            Value::BuiltinFn(name) => write!(f, "{name}"),
        }
    }
}

impl Node for Value {
    fn boxed(&self) -> NodeBox {
        match self {
            Value::Number(n) => NodeBox::new(constant_scalar_node(*n)),
            Value::Vector(v) => NodeBox::new(constant_vector_node(*v)),
            Value::Node(node) => node.clone(),
            Value::BuiltinFn(_) => NodeBox::new(constant_scalar_node(0.0)),
        }
    }
    fn sample(&mut self, sample_rate: f64, pos: Vector, dir: Vector) -> Vector {
        match self {
            Value::Number(n) => Vector::splat(*n),
            Value::Vector(v) => *v,
            Value::Node(node) => node.sample(sample_rate, pos, dir),
            Value::BuiltinFn(_) => Vector::ZERO,
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
            Value::Vector(_) => "vector",
            Value::Node(_) => "node",
            Value::BuiltinFn(_) => "builtin function",
        }
    }
    pub fn un_op(
        self,
        op_name: &'static str,
        f: impl Fn(f64) -> f64 + Clone + Send + Sync + 'static,
    ) -> ParseResult<Self> {
        Ok(match self {
            Value::Number(n) => Value::Number(f(n)),
            Value::Vector(v) => Value::Vector(v.map(f)),
            Value::Node(node) => Value::Node(NodeBox::new(state_node(
                format!("{op_name} {node:?}"),
                node,
                move |node, sample_rate, pos, dir| node.sample(sample_rate, pos, dir).map(|v| f(v)),
            ))),
            Value::BuiltinFn(_) => {
                return Err(ParseError::InvalidUnaryOperation {
                    op: op_name,
                    operand: self.type_name(),
                })
            }
        })
    }
    pub fn bin_scalar_op(
        self,
        other: Self,
        op_name: &'static str,
        f: impl Fn(f64, f64) -> f64 + Clone + Send + Sync + 'static,
    ) -> ParseResult<Self> {
        Ok(match (self, other) {
            (Value::Number(a), Value::Number(b)) => Value::Number(f(a, b)),
            (Value::Vector(a), Value::Vector(b)) => Value::Vector(a.with(b, f)),
            (Value::Vector(a), Value::Number(b)) => Value::Vector(a.map(|a| f(a, b))),
            (Value::Number(a), Value::Vector(b)) => Value::Vector(b.map(|b| f(a, b))),
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
            (Value::Vector(a), Value::Node(b)) => Value::Node(NodeBox::new(state_node(
                format!("{a:?} {op_name} {b:?}"),
                b,
                move |b, sample_rate, pos, dir| {
                    b.sample(sample_rate, pos, dir).with(a, |b, a| f(a, b))
                },
            ))),
            (Value::Node(a), Value::Vector(b)) => Value::Node(NodeBox::new(state_node(
                format!("{a:?} {op_name} {b:?}"),
                a,
                move |a, sample_rate, pos, dir| {
                    a.sample(sample_rate, pos, dir).with(b, |a, b| f(a, b))
                },
            ))),
            (Value::Node(a), Value::Node(b)) => Value::Node(NodeBox::new(state_node(
                format!("{a:?} {op_name} {b:?}"),
                (a, b),
                move |(a, b), sample_rate, pos, dir| {
                    a.sample(sample_rate, pos, dir)
                        .with(b.sample(sample_rate, pos, dir), |a, b| f(a, b))
                },
            ))),
            (a, b) => {
                return Err(ParseError::InvalidBinaryOperation {
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
