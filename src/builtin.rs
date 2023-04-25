use std::{
    collections::HashMap,
    f64::consts::{E, PI, TAU},
};

use hodaun::{Letter, Octave, Stereo};
use once_cell::sync::Lazy;

use crate::{
    compile::{call, CompileError, CompileResult},
    lex::{Sp, Span},
    modulus,
    node::*,
    value::Value,
};

pub fn builtin_constant(name: &str) -> Option<Value> {
    Some(match name {
        "PI" => PI.into(),
        "TAU" => TAU.into(),
        "E" => E.into(),
        "INF" => f64::INFINITY.into(),
        "noise" => noise_node().into(),
        "m2" => 2f64.powf(2.0 / 12.0).into(),
        "b3" => 2f64.powf(3.0 / 12.0).into(),
        "m3" => 2f64.powf(4.0 / 12.0).into(),
        "p4" => 2f64.powf(5.0 / 12.0).into(),
        "p5" => 2f64.powf(7.0 / 12.0).into(),
        "m6" => 2f64.powf(9.0 / 12.0).into(),
        "b7" => 2f64.powf(10.0 / 12.0).into(),
        "m7" => 2f64.powf(11.0 / 12.0).into(),
        "p8" => 2.0.into(),
        "time" => pure_node("time", |env| Stereo::both(env.time)).into(),
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
        ($name:ident, $($span:ident,)?
        |
            $($(#[default($default:expr)])? $arg:ident),*
            $($(,)?[$varargs:ident])?
            $(,($arg2:ident))*
            $(,)?
        | $body:expr)),*
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
    /// Generate a sine wave from a frequency
    (sin, |freqs| freqs.distribute(|freq| Wave3::new(
        "sine",
        freq,
        |time| { (time * TAU).sin() }
    ))),
    /// Generate a square wave from a frequency
    (square, |freqs| freqs.distribute(|freq| Wave3::new(
        "square",
        freq,
        square_wave
    ))),
    /// Generate a saw wave from a frequency
    (saw, |freqs| freqs
        .distribute(|freq| Wave3::new("saw", freq, saw_wave))),
    /// Generate a triangle wave from a frequency
    (tri, |freqs| freqs.distribute(|freq| Wave3::new(
        "triangle",
        freq,
        triangle_wave
    ))),
    /// Generate an additive square wave from a frequency and number of harmonics
    (hsquare, span, |n, freqs| {
        let n = n.expect_number("n", span)? as usize;
        freqs.distribute(|freq| Wave3::new("hsquare", freq, move |time| true_square_wave(time, n)))
    }),
    /// Generate an additive saw wave from a frequency and number of harmonics
    (hsaw, span, |n, freqs| {
        let n = n.expect_number("n", span)? as usize;
        freqs.distribute(|freq| Wave3::new("hsaw", freq, move |time| true_saw_wave(time, n)))
    }),
    /// Generate an additive triangle wave from a frequency and number of harmonics
    (htri, span, |n, freqs| {
        let n = n.expect_number("n", span)? as usize;
        freqs.distribute(|freq| {
            Wave3::new("htriangle", freq, move |time| true_triangle_wave(time, n))
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
                period.zip(high).with(falloff, |(period, high), falloff| {
                    kick_wave(env.time, period, high, falloff)
                })
            }
        )
    ),
    /// Get the minimum of two or more values
    (min, span, |a, [rest]| {
        let mut min = a;
        for b in rest {
            min = min.bin_scalar_op(b.value, "min", span, f64::min)?;
        }
        min
    }),
    /// Get the maximum of two or more values
    (max, span, |a, [rest]| {
        let mut max = a;
        for b in rest {
            max = max.bin_scalar_op(b.value, "max", span, f64::max)?;
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
    /// Pan
    (pan, |pan, value| {
        state_node("pan", (pan, value), |(pan, value), env| {
            let pan = pan.sample(env).average();
            let value = value.sample(env).average();
            Stereo::pan(value, pan)
        })
    }),
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
    /// Create looping sections from some values
    ///
    /// With an offset at `offset`, each section will be played for the `period`.
    ///
    /// ## Example
    /// ```
    /// square 110 * max 0 (saw (sec 0 1 2 8))
    /// ```
    (sec, span, |offset, period, values| {
        let offset = offset.expect_vector("offset", span)?;
        let nodes: Vec<NodeBox> = values
            .into_list()
            .into_iter()
            .map(|v| v.into_node())
            .collect();
        state_node("sections", (nodes, period), move |(nodes, period), env| {
            let period = period.sample(env);
            let i = offset.with(period, |offset, period| {
                (modulus(env.time - offset, period * nodes.len() as f64) / period) as usize
            });
            let left = nodes[i.left].sample(env).left;
            let right = if i.left == i.right {
                left
            } else {
                nodes[i.right].sample(env).right
            };
            Stereo::new(left, right)
        })
    }),
    (sel, span, |indices, values| {
        let indices = indices.into_list();
        let values = values.into_list();
        let mut selected = Vec::with_capacity(indices.len());
        for index in indices {
            let index = index.expect_natural("index", span)?;
            let value = values.get(index).cloned().ok_or_else(|| {
                span.sp(CompileError::IndexOutOfBounds {
                    index,
                    len: values.len(),
                })
            })?;
            selected.push(value);
        }
        Value::List(selected)
    }),
    (join, |[values]| {
        let mut joined = Vec::new();
        for value in values {
            joined.extend(value.value.into_list());
        }
        Value::List(joined)
    }),
    (flip, span, |function, [args]| {
        args.reverse();
        call(span.sp(function), args)?.value
    }),
    (phi, span, |f, g, h, x, [y]| {
        let gx = call(span.sp(g), vec![span.sp(x)])?;
        let hy = call(span.sp(h), y)?;
        call(span.sp(f), vec![gx, hy])?.value
    }),
    (lhook, span, |f, g, x, [y]| {
        let gx = call(span.sp(g), vec![span.sp(x)])?;
        y.insert(0, gx);
        call(span.sp(f), y)?.value
    }),
    (rhook, span, |f, g, x, [y]| {
        let gy = call(span.sp(g), y)?;
        call(span.sp(f), vec![span.sp(x), gy])?.value
    }),
    (comp, span, |f, g, [x]| {
        let gx = call(span.sp(g), x)?;
        call(span.sp(f), vec![gx])?.value
    })
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
