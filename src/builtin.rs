use std::{collections::HashMap, f64::consts::TAU, ops::Add};

use once_cell::sync::Lazy;

use crate::{node::*, parse::ParseResult, value::Value};

pub type BuiltinFn = dyn Fn(Vec<Value>) -> ParseResult<Value> + Send + Sync;

type BuiltinFnMap = HashMap<String, HashMap<usize, Box<BuiltinFn>>>;

macro_rules! build_map {
    ($(($name:ident, |$($arg:ident),* $(,)?| $body:expr)),* $(,)*) => {{
        let mut map: BuiltinFnMap = HashMap::new();
        $(
            let arg_count = 0 $(+ { stringify!($arg); 1 })*;
            map.entry(stringify!($name).to_string()).or_default().insert(arg_count, Box::new(|args: Vec<Value>| {
                let mut args = args.into_iter();
                $(let $arg = args.next().unwrap();)*
                Ok($body.into())
            }));
        )*
        map
    }};
}

pub static BUILTINS: Lazy<BuiltinFnMap> = Lazy::new(|| {
    build_map!(
        (sin, |freq| Wave3::new("sine", freq, |pos| {
            pos.map(|x| (x * TAU).sin()).reduce(Add::add)
        })),
        (square, |freq| Wave3::new("square", freq, |pos| {
            pos.map(|x| true_square_wave(x, 50)).reduce(Add::add)
        })),
        (saw, |freq| Wave3::new("saw", freq, |pos| {
            pos.map(|x| true_saw_wave(x, 50)).reduce(Add::add)
        })),
        (tri, |freq| Wave3::new("triangle", freq, |pos| {
            pos.map(|x| true_triangle_wave(x, 50)).reduce(Add::add)
        })),
        (min, |a, b| a.bin_op(b, "min", f64::min)?),
        (max, |a, b| a.bin_op(b, "max", f64::max)?),
    )
});
