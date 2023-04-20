use std::{
    collections::{BTreeMap, HashMap},
    f64::consts::{E, PI, TAU},
};

use once_cell::sync::Lazy;

use crate::{lex::Span, node::*, parse::ParseResult, value::Value, vector::Vector};

pub fn builtin_constant(name: &str) -> Option<Value> {
    Some(match name {
        "ZERO" => Vector::ZERO.into(),
        "X" => Vector::X.into(),
        "Y" => Vector::Y.into(),
        "Z" => Vector::Z.into(),
        "PI" => PI.into(),
        "TAU" => TAU.into(),
        "E" => E.into(),
        _ => return None,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ArgCount {
    Exact(usize),
    AtLeast(usize),
}

impl ArgCount {
    pub fn matches(&self, n: usize) -> bool {
        match self {
            ArgCount::Exact(n2) => n == *n2,
            ArgCount::AtLeast(n2) => n >= *n2,
        }
    }
}

pub type BuiltinFn = dyn Fn(Vec<Value>, Span) -> ParseResult<Value> + Send + Sync;

type BuiltinFnMap = HashMap<String, BTreeMap<ArgCount, Box<BuiltinFn>>>;

macro_rules! build_map {
    ($(($name:ident, $($span:ident,)? |$($arg:ident),* $(,($varargs:ident))? $(,)?| $body:expr)),* $(,)*) => {{
        let mut map = BuiltinFnMap::new();
        $(
            let arg_count_n = 0 $(+ { stringify!($arg); 1 })*;
            #[allow(unused_variables)]
            let arg_count = ArgCount::Exact(arg_count_n);
            $(let arg_count = {
                stringify!($varargs);
                ArgCount::AtLeast(arg_count_n)
            };)?
            map.entry(stringify!($name).to_string()).or_default().insert(arg_count, Box::new(|args: Vec<Value>, _span: Span| {
                let mut args = args.into_iter();
                $(let $arg = args.next().unwrap();)*
                $(let $span = _span;)?
                $(let $varargs: Vec<_> = args.collect();)?
                Ok($body.into())
            }));
        )*
        map
    }};
}

pub static BUILTINS: Lazy<BuiltinFnMap> = Lazy::new(|| {
    build_map!(
        (sin, |freq| Wave3::new("sine", freq, |pos| {
            pos.map(|x| (x * TAU).sin())
        })),
        (square, |freq| Wave3::new("square", freq, |pos| {
            pos.map(|x| true_square_wave(x, 50))
        })),
        (saw, |freq| Wave3::new("saw", freq, |pos| {
            pos.map(|x| true_saw_wave(x, 50))
        })),
        (tri, |freq| Wave3::new("triangle", freq, |pos| {
            pos.map(|x| true_triangle_wave(x, 50))
        })),
        (min, span, |a, (rest)| {
            let mut min = a;
            for b in rest {
                min = min.bin_scalar_op(b, "min", span, f64::min)?;
            }
            min
        }),
        (max, span, |a, (rest)| {
            let mut max = a;
            for b in rest {
                max = max.bin_scalar_op(b, "max", span, f64::max)?;
            }
            max
        }),
        (pow, span, |a, b| a.bin_scalar_op(
            b,
            "pow",
            span,
            f64::powf
        )?),
        (log, span, |a, b| a.bin_scalar_op(
            b,
            "log",
            span,
            f64::log
        )?),
        (neg, span, |x| x.un_scalar_op("neg", span, |x| -x)?),
        (abs, span, |x| x.un_scalar_op("abs", span, f64::abs)?),
        (sqrt, span, |x| x.un_scalar_op("sqrt", span, f64::sqrt)?),
        (exp, span, |x| x.un_scalar_op("exp", span, f64::exp)?),
        (x, span, |v| v
            .un_vector_op("x", span, |v| Vector::X * v.x)?),
        (y, span, |v| v
            .un_vector_op("y", span, |v| Vector::Y * v.y)?),
        (z, span, |v| v
            .un_vector_op("z", span, |v| Vector::Z * v.z)?),
        (len, span, |v| v
            .un_vector_op("len", span, |v| Vector::X * v.length())?),
        (vec, span, |v| v.un_scalar_to_vector_op(
            "vec",
            span,
            Vector::splat
        )?),
        (vec, span, |x, y| {
            let x = x.un_vector_op("x", span, |v| Vector::X * v.x)?;
            let y = y.un_vector_op("y", span, |v| Vector::Y * v.y)?;
            x.bin_vector_op(y, "vec", span, |x, y| x + y)?
        }),
        (vec, span, |x, y, z| {
            let x = x.un_vector_op("x", span, |v| Vector::X * v.x)?;
            let y = y.un_vector_op("y", span, |v| Vector::Y * v.y)?;
            let z = z.un_vector_op("z", span, |v| Vector::Z * v.z)?;
            x.bin_vector_op(y, "vec", span, |x, y| x + y)?
                .bin_vector_op(z, "vec", span, |xy, z| xy + z)?
        }),
    )
});
