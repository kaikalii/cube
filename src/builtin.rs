use std::{collections::HashMap, f64::consts::TAU};

use once_cell::sync::Lazy;

use crate::{node::*, parse::ParseResult, value::Value, vector::Vector};

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
        (min, |a, b| a.bin_scalar_op(b, "min", f64::min)?),
        (max, |a, b| a.bin_scalar_op(b, "max", f64::max)?),
        (neg, |x| x.un_scalar_op("neg", |x| -x)?),
        (abs, |x| x.un_scalar_op("abs", f64::abs)?),
        (x, |v| v.un_vector_op("x", |v| Vector::X * v.x)?),
        (y, |v| v.un_vector_op("y", |v| Vector::Y * v.y)?),
        (z, |v| v.un_vector_op("z", |v| Vector::Z * v.z)?),
        (len, |v| v
            .un_vector_op("len", |v| Vector::X * v.length())?),
        (vec, |v| v.un_scalar_to_vector_op("vec", Vector::splat)?),
        (vec, |x, y| {
            let x = x.un_vector_op("x", |v| Vector::X * v.x)?;
            let y = y.un_vector_op("y", |v| Vector::Y * v.y)?;
            x.bin_vector_op(y, "vec", |x, y| x + y)?
        }),
        (vec, |x, y, z| {
            let x = x.un_vector_op("x", |v| Vector::X * v.x)?;
            let y = y.un_vector_op("y", |v| Vector::Y * v.y)?;
            let z = z.un_vector_op("z", |v| Vector::Z * v.z)?;
            x.bin_vector_op(y, "vec", |x, y| x + y)?
                .bin_vector_op(z, "vec", |xy, z| xy + z)?
        }),
    )
});
