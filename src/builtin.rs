use std::{
    collections::HashMap,
    f64::consts::{E, PI, TAU},
};

use hodaun::{Letter, Octave};
use once_cell::sync::Lazy;

use crate::{
    compile::{CompileError, CompileResult},
    lex::{Sp, Span},
    node::*,
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
        "noise" => noise_node().into(),
        name => {
            let (letter, octave) = parse_note(name)?;
            letter.frequency(octave).into()
        }
    })
}

fn parse_note(name: &str) -> Option<(Letter, Octave)> {
    if !name.ends_with(|c: char| c.is_ascii_digit()) {
        return None;
    };
    let octave: Octave = name[name.len() - 1..].parse().unwrap();
    let letter = &name[..name.len() - 1];
    let letter = match letter {
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
    };
    Some((letter, octave))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArgCount {
    min: usize,
    max: Option<usize>,
}

impl ArgCount {
    pub fn matches(&self, n: usize) -> bool {
        if n < self.min {
            return false;
        }
        if let Some(max) = self.max {
            n <= max
        } else {
            true
        }
    }
}

pub type BuiltinFn = dyn Fn(Vec<Sp<Value>>, Span) -> CompileResult<Value> + Send + Sync;

type BuiltinFnMap = HashMap<String, (ArgCount, Box<BuiltinFn>)>;

macro_rules! make_builtin_fns {
    ($(
        $(#[doc = $doc:literal])*
        ($name:ident, $($span:ident,)? |$($(#[default($default:expr)])? $arg:ident),* $($(,)?($varargs:ident))? $(,)?| $body:expr)),*
    $(,)*) => {
        #[allow(unused_assignments, unreachable_code, unused_mut)]
        fn builtin_fns() -> BuiltinFnMap {
            let mut map = BuiltinFnMap::new();
            $(
                let mut min = 0;
                let mut max = Some(0);
                $(
                    stringify!($arg);
                    min += 1;
                    max = Some(max.unwrap() + 1);
                    $(
                        stringify!($default);
                        min -= 1;
                    )*
                )*
                $(
                    stringify!($varargs);
                    max = None;
                )*
                let args = ArgCount { min, max };
                map.insert(stringify!($name).into(), (args, Box::new(|args: Vec<Sp<Value>>, _span: Span| {
                    let mut args = args.into_iter();
                    $(let mut $arg = args.next().map(|arg| arg.value).unwrap_or_else(|| {
                        $(return $default.into();)?
                        unreachable!()
                    });)*
                    $(let $span = _span;)?
                    $(let mut $varargs: Vec<_> = args.collect();)?
                    Ok($body.into())
                })));
            )*
            map
        }
        #[cfg(test)]
        #[allow(path_statements, unused_mut)]
        fn builtin_docs() -> Vec<BuiltinDocs> {
            let mut docs: Vec<BuiltinDocs> = Vec::new();
            $(
                let doc_lines: &[&str] = &[$($doc.trim()),*];
                if !doc_lines.is_empty() {
                    let doc = doc_lines.join("\n");
                    let name = stringify!($name);
                    docs.push(BuiltinDocs {
                        name,
                        doc,
                        args: vec![
                            $({
                                let mut name = stringify!($arg).to_string();
                                $(
                                    name.insert_str(0, "[");
                                    name.push_str(" = ");
                                    let default = stringify!($default);
                                    let default = default.strip_suffix(".clone()").unwrap_or(default);
                                    name.push_str(default);
                                    name.push(']');
                                )*
                                name
                            }),*
                        ],
                        varargs: { None::<&str> $(; Some(stringify!($varargs)))?},
                    });
                }
            )*
            docs
        }
    };
}

make_builtin_fns!(
    (add, sp, |a, b| a
        .bin_scalar_op(b, "add", sp, |a, b| a + b)?),
    (sub, sp, |a, b| a
        .bin_scalar_op(b, "sub", sp, |a, b| a - b)?),
    (mul, sp, |a, b| a
        .bin_scalar_op(b, "mul", sp, |a, b| a * b)?),
    (div, sp, |a, b| a
        .bin_scalar_op(b, "div", sp, |a, b| a / b)?),
    /// Generate a sine wave from a frequency
    (sin, |freq| Wave3::new("sine", freq, |pos| {
        pos.map(|x| (x * TAU).sin())
    })),
    /// Generate a square wave from a frequency
    (square, |freq| Wave3::new("square", freq, |pos| pos
        .map(square_wave))),
    /// Generate a saw wave from a frequency
    (saw, |freq| Wave3::new("saw", freq, |pos| pos.map(saw_wave))),
    /// Generate a triangle wave from a frequency
    (tri, |freq| Wave3::new("triangle", freq, |pos| pos
        .map(triangle_wave))),
    /// Generate an additive square wave from a frequency and number of harmonics
    (hsquare, span, |n, freq| {
        let n = n.expect_number("n", span)? as usize;
        Wave3::new("hsquare", freq, move |pos| {
            pos.map(|x| true_square_wave(x, n))
        })
    }),
    /// Generate an additive saw wave from a frequency and number of harmonics
    (hsaw, span, |n, freq| {
        let n = n.expect_number("n", span)? as usize;
        Wave3::new("hsaw", freq, move |pos| pos.map(|x| true_saw_wave(x, n)))
    }),
    /// Generate an additive triangle wave from a frequency and number of harmonics
    (htri, span, |n, freq| {
        let n = n.expect_number("n", span)? as usize;
        Wave3::new("htriangle", freq, move |pos| {
            pos.map(|x| true_triangle_wave(x, n))
        })
    }),
    /// Generate kick drum sound
    (
        kick,
        |freq, #[default(40)] high, #[default(0.5)] falloff| state_node(
            "kick",
            (freq, high, falloff),
            |(freq, high, falloff), env| {
                let period = 1.0 / freq.sample(env);
                let high = high.sample(env);
                let falloff = falloff.sample(env);
                period
                    .zip(high)
                    .zip(falloff)
                    .with(env.pos, |((period, high), falloff), pos| {
                        kick_wave(pos, period, high, falloff)
                    })
            }
        )
    ),
    /// Get the minimum of two or more values or a vector
    (min, span, |a, (rest)| {
        let mut min = a;
        if rest.is_empty() {
            min = min.un_vector_op("min", span, |v| Vector::splat(v.x.min(v.y).min(v.z)))?;
        } else {
            for b in rest {
                min = min.bin_scalar_op(b.value, "min", span, f64::min)?;
            }
        }
        min
    }),
    /// Get the maximum of two or more values or a vector
    (max, span, |a, (rest)| {
        let mut max = a;
        if rest.is_empty() {
            max = max.un_vector_op("max", span, |v| Vector::splat(v.x.max(v.y).max(v.z)))?;
        } else {
            for b in rest {
                max = max.bin_scalar_op(b.value, "max", span, f64::max)?;
            }
        }
        max
    }),
    /// Raise a value to a power
    (pow, sp, |a, b| a.bin_scalar_op(b, "pow", sp, f64::powf)?),
    /// Get the logarithm of a value
    (log, sp, |a, b| a.bin_scalar_op(b, "log", sp, f64::log)?),
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
    /// Passing three values will create a vector with the x, y, and z components equal to those values.
    (
        vec,
        span,
        |x, #[default(x.clone())] y, #[default(x.clone())] z| {
            let x = x.un_vector_op("x", span, |v| Vector::X * v.x)?;
            let y = y.un_vector_op("y", span, |v| Vector::Y * v.y)?;
            let z = z.un_vector_op("z", span, |v| Vector::Z * v.z)?;
            x.bin_vector_op(y, "vec", span, |x, y| x + y)?
                .bin_vector_op(z, "vec", span, |xy, z| xy + z)?
        }
    ),
    /// Get the frequency of a beat subdivided into `n` parts at the current tempo
    (perbeat, |n| {
        state_node("perbeat", n, move |n, env| n.sample(env) * env.beat_freq())
    }),
    /// Get the period that is an `n`th of a beat at the current tempo
    (beat, |n| {
        state_node("beat", n, move |n, env| {
            1.0 / env.beat_freq() / n.sample(env)
        })
    }),
    /// Get the period of `n` beats at the current tempo
    (beats, |n| {
        state_node("beats", n, move |n, env| n.sample(env) / env.beat_freq())
    }),
    (m2, sp, |x| x
        .un_scalar_op("m2", sp, |x| x * 2f64.powf(2.0 / 12.0))?),
    (b3, sp, |x| x
        .un_scalar_op("b3", sp, |x| x * 2f64.powf(3.0 / 12.0))?),
    (m3, sp, |x| x
        .un_scalar_op("m3", sp, |x| x * 2f64.powf(4.0 / 12.0))?),
    (p4, sp, |x| x
        .un_scalar_op("p4", sp, |x| x * 2f64.powf(5.0 / 12.0))?),
    (p5, sp, |x| x
        .un_scalar_op("p5", sp, |x| x * 2f64.powf(7.0 / 12.0))?),
    (m6, sp, |x| x
        .un_scalar_op("m6", sp, |x| x * 2f64.powf(9.0 / 12.0))?),
    (b7, sp, |x| x
        .un_scalar_op("b7", sp, |x| x * 2f64.powf(10.0 / 12.0))?),
    (m7, sp, |x| x
        .un_scalar_op("m7", sp, |x| x * 2f64.powf(11.0 / 12.0))?),
    (p8, sp, |x| x.un_scalar_op("p8", sp, |x| x * 2.0)?),
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
        let mut nodes: Vec<NodeBox> = vec![first.into_node()];
        for value in rest {
            nodes.push(value.value.into_node());
        }
        state_node("sections", (nodes, period), move |(nodes, period), env| {
            let period = period.sample(env);
            let i = offset
                .zip(period)
                .zip(env.pos)
                .map(|((offset, period), pos)| {
                    (modulus(pos - offset, period * nodes.len() as f64) / period) as usize
                });
            let x = nodes[i.x].sample(env).x;
            let y = if i.x == i.y {
                x
            } else {
                nodes[i.y].sample(env).y
            };
            let z = if i.x == i.z {
                x
            } else if i.y == i.z {
                y
            } else {
                nodes[i.z].sample(env).z
            };
            Vector::new(x, y, z)
        })
    }),
    (sel, span, |(args)| {
        let mut args = args.into_iter();
        let indices: Vec<Sp<Value>> = args
            .by_ref()
            .take_while(|v| !matches!(v.value, Value::Sep))
            .collect();
        let values: Vec<Sp<Value>> = args.collect();
        let mut selected = Vec::with_capacity(indices.len());
        for index in indices {
            let index = index.value.expect_natural("index", index.span)?;
            let value = values.get(index).cloned().ok_or_else(|| {
                span.sp(CompileError::IndexOutOfBounds {
                    index,
                    len: values.len(),
                })
            })?;
            selected.push(value);
        }
        Value::Args(selected)
    }),
    (args, |(args)| { Value::Args(args) })
);

pub static BUILTINS: Lazy<BuiltinFnMap> = Lazy::new(builtin_fns);

#[cfg(test)]
struct BuiltinDocs {
    name: &'static str,
    args: Vec<String>,
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
        write!(file, "<code><b>{}</b>", docs.name)?;
        if let Some(varargs) = docs.varargs {
            writeln!(file, " {} ...{}</code>", docs.args.join(" "), varargs)?;
        } else {
            writeln!(file, " {}</code>", docs.args.join(" "))?;
        }
        writeln!(file)?;
        writeln!(file, "{}", docs.doc)?;
        writeln!(file)?;
    }
    Ok(())
}
