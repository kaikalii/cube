use std::{
    collections::{BTreeMap, HashMap},
    f64::consts::{E, PI, TAU},
};

use once_cell::sync::Lazy;

use crate::{
    lex::Span,
    node::*,
    parse::ParseResult,
    value::Value,
    vector::{modulus, Vector},
};

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

macro_rules! make_builtin_fns {
    ($(
        $(#[doc = $doc:literal])*
        ($name:ident, $($span:ident,)? |$($arg:ident),* $(,($varargs:ident))? $(,)?| $body:expr)),*
    $(,)*) => {
        fn builtin_fns() -> BuiltinFnMap {
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
        }
        #[cfg(test)]
        #[allow(path_statements)]
        fn builtin_docs() -> Vec<BuiltinDocs> {
            let mut docs: Vec<BuiltinDocs> = Vec::new();
            $(
                let doc_lines: &[&str] = &[$($doc.trim()),*];
                if !doc_lines.is_empty() {
                    let doc = doc_lines.join("\n");
                    let name = stringify!($name);
                    if let Some(docs) = docs.iter_mut().find(|docs| docs.name == name) {
                        docs.doc.push('\n');
                        docs.doc.push_str(&doc);
                    } else {
                        docs.push(BuiltinDocs {
                            name,
                            doc,
                            args: &[$(stringify!($arg)),*],
                            varargs: { None::<&str> $(; Some(stringify!($varargs)))?},
                        });
                    }
                }
            )*
            docs
        }
    };
}
make_builtin_fns!(
    /// Generate a sine wave from a frequency
    (sin, |freq| Wave3::new("sine", freq, |pos| {
        pos.map(|x| (x * TAU).sin())
    })),
    /// Generate a square wave from a frequency
    (square, |freq| Wave3::new("square", freq, |pos| {
        pos.map(|x| true_square_wave(x, 50))
    })),
    /// Generate a saw wave from a frequency
    (saw, |freq| Wave3::new("saw", freq, |pos| {
        pos.map(|x| true_saw_wave(x, 50))
    })),
    /// Generate a triangle wave from a frequency
    (tri, |freq| Wave3::new("triangle", freq, |pos| {
        pos.map(|x| true_triangle_wave(x, 50))
    })),
    /// Get the minimum of two or more values
    (min, span, |a, (rest)| {
        let mut min = a;
        for b in rest {
            min = min.bin_scalar_op(b, "min", span, f64::min)?;
        }
        min
    }),
    /// Get the maximum of two or more values
    (max, span, |a, (rest)| {
        let mut max = a;
        for b in rest {
            max = max.bin_scalar_op(b, "max", span, f64::max)?;
        }
        max
    }),
    /// Raise a value to a power
    (pow, span, |a, b| a.bin_scalar_op(
        b,
        "pow",
        span,
        f64::powf
    )?),
    /// Get the logarithm of a value
    (log, span, |a, b| a.bin_scalar_op(
        b,
        "log",
        span,
        f64::log
    )?),
    /// Negate a value
    (neg, span, |x| x.un_scalar_op("neg", span, |x| -x)?),
    /// Get the absolute value of a value
    (abs, span, |x| x.un_scalar_op("abs", span, f64::abs)?),
    /// Get the square root of a value
    (sqrt, span, |x| x.un_scalar_op("sqrt", span, f64::sqrt)?),
    /// Get e raised to a value
    (exp, span, |x| x.un_scalar_op("exp", span, f64::exp)?),
    /// Get the x component of a vector
    (x, span, |v| v
        .un_vector_op("x", span, |v| Vector::X * v.x)?),
    /// Get the y component of a vector
    (y, span, |v| v
        .un_vector_op("y", span, |v| Vector::Y * v.y)?),
    /// Get the z component of a vector
    (z, span, |v| v
        .un_vector_op("z", span, |v| Vector::Z * v.z)?),
    /// Get the length of a vector
    (len, span, |v| v
        .un_vector_op("len", span, |v| Vector::X * v.length())?),
    /// Create a new vector
    ///
    /// Passing a single value will create a vector with all components equal to that value.
    /// Passing two values will create a vector with the x and y components equal to those values.
    /// Passing three values will create a vector with the x, y, and z components equal to those values.
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
    /// Create looping sections from some values
    ///
    /// With an offset at `offset`, each section will be played for the `period`.
    ///
    /// ## Example
    /// ```
    /// square 110 * max 0 (saw (sec 0 1 2 8))
    /// ```
    (sec, span, |offset, period, first, (rest)| {
        let offset = offset.expect_vector("offset", span)?;
        let period = period.expect_vector("period", span)?;
        let mut nodes: Vec<NodeBox> = vec![first.into_node()];
        for value in rest {
            nodes.push(value.into_node());
        }
        state_node("sections", nodes, move |nodes, sr, pos, dir| {
            let i = offset.zip(period).zip(pos).map(|((offset, period), pos)| {
                (modulus(pos - offset, period * nodes.len() as f64) / period) as usize
            });
            let x = nodes[i.x].sample(sr, pos, dir).x;
            let y = if i.x == i.y {
                x
            } else {
                nodes[i.y].sample(sr, pos, dir).y
            };
            let z = if i.x == i.z {
                x
            } else if i.y == i.z {
                y
            } else {
                nodes[i.z].sample(sr, pos, dir).z
            };
            Vector::new(x, y, z)
        })
    })
);

pub static BUILTINS: Lazy<BuiltinFnMap> = Lazy::new(builtin_fns);

#[cfg(test)]
struct BuiltinDocs {
    name: &'static str,
    args: &'static [&'static str],
    varargs: Option<&'static str>,
    doc: String,
}

#[cfg(test)]
#[test]
fn generate_docs() -> std::io::Result<()> {
    use std::{fs::File, io::Write};
    let mut file = File::create("builtins.md")?;
    for docs in builtin_docs() {
        writeln!(file, "# `{}`", docs.name)?;
        write!(file, "**`{}`**", docs.name)?;
        if let Some(varargs) = docs.varargs {
            writeln!(file, " `{} ...{}`", docs.args.join(" "), varargs)?;
        } else {
            writeln!(file, " `{}`", docs.args.join(" "))?;
        }
        writeln!(file)?;
        writeln!(file, "{}", docs.doc)?;
        writeln!(file)?;
    }
    Ok(())
}
