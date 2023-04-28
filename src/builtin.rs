use std::{
    collections::HashMap,
    f64::consts::{PI, TAU},
};

use hodaun::Stereo;
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
        "_" => 0.0.into(),
        "PI" => PI.into(),
        "TAU" => TAU.into(),
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
        _ => return None,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArgCount {
    pub min: usize,
    pub variadic: bool,
}

impl ArgCount {
    pub fn matches(&self, n: usize) -> bool {
        if self.variadic {
            n >= self.min
        } else {
            n == self.min
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
            $(,)?
        | $body:expr)),*
    $(,)*) => {
        #[allow(unused_assignments, unreachable_code, unused_mut)]
        fn builtin_fns() -> BuiltinFnMap {
            let mut map = BuiltinFnMap::new();
            $(
                let mut min = 0;
                let mut variadic = false;
                $(
                    stringify!($arg);
                    min += 1;
                    $(
                        stringify!($default);
                        min -= 1;
                    )*
                )*
                $(
                    stringify!($varargs);
                    variadic = true;
                )*
                let args = ArgCount { min, variadic };
                map.insert(stringify!($name).into(), (args, Box::new(|args: Vec<Sp<Value>>, _span: Span| {
                    let mut args = args.into_iter();
                    $(let mut $arg = args.next().unwrap_or_else(|| {
                        $(return _span.sp($default.into());)?
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
    /// Named alias for `+`
    (add, span, |a, b| a.val.add(b.val, span)?),
    /// Named alias for `-`
    (sub, span, |a, b| a.val.sub(b.val, span)?),
    /// Named alias for `*`
    (mul, span, |a, b| a.val.mul(b.val, span)?),
    /// Named alias for `/`
    (div, span, |a, b| a.val.div(b.val, span)?),
    /// Named alias for `<`
    (lt, span, |a, b| a.val.lt(b.val, span)?),
    /// Named alias for `<=`
    (le, span, |a, b| a.val.le(b.val, span)?),
    /// Named alias for `>`
    (gt, span, |a, b| a.val.gt(b.val, span)?),
    /// Named alias for `>=`
    (ge, span, |a, b| a.val.ge(b.val, span)?),
    /// Generate a sine wave from a frequency
    (sine, |freqs| freqs.val.distribute(|freq| wave_node(
        "sine",
        freq,
        |time| (time * TAU).sin()
    ))),
    /// Generate a square wave from a frequency
    (square, |freqs| freqs.val.distribute(|freq| wave_node(
        "square",
        freq,
        square_wave
    ))),
    /// Generate a saw wave from a frequency
    (saw, |freqs| freqs
        .val
        .distribute(|freq| wave_node("saw", freq, saw_wave))),
    /// Generate a triangle wave from a frequency
    (tri, |freqs| freqs.val.distribute(|freq| wave_node(
        "triangle",
        freq,
        triangle_wave
    ))),
    /// Generate an additive square wave from a frequency and number of harmonics
    (hsquare, |n, freqs| {
        freqs.val.distribute(|freq| {
            harmonic_wave_node("hsquare", n.val.clone(), freq, harmonic_square_wave)
        })
    }),
    /// Generate an additive saw wave from a frequency and number of harmonics
    (hsaw, |n, freqs| {
        freqs
            .val
            .distribute(|freq| harmonic_wave_node("hsaw", n.val.clone(), freq, harmonic_saw_wave))
    }),
    /// Generate an additive triangle wave from a frequency and number of harmonics
    (htri, |n, freqs| {
        freqs.val.distribute(|freq| {
            harmonic_wave_node("htriangle", n.val.clone(), freq, harmonic_triangle_wave)
        })
    }),
    /// Generate kick drum sound
    (
        kick,
        |freq, #[default(40)] high, #[default(0.5)] falloff| state_node(
            "kick",
            (
                freq.val.into_node(),
                high.val.into_node(),
                falloff.val.into_node()
            ),
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
    (min, |a, [rest]| {
        let mut min = a.val;
        for b in rest {
            min = min.bin_scalar_op(b.val, "min", b.span, f64::min)?;
        }
        min
    }),
    /// Get the maximum of two or more values
    (max, |a, [rest]| {
        let mut max = a.val;
        for b in rest {
            max = max.bin_scalar_op(b.val, "max", b.span, f64::max)?;
        }
        max
    }),
    /// Raise a value to a power
    (pow, sp, |a, b| a.val.bin_scalar_op(
        b.val,
        "pow",
        sp,
        f64::powf
    )?),
    /// Get the logarithm of a value
    (log, sp, |a, b| a.val.bin_scalar_op(
        b.val,
        "log",
        sp,
        f64::log
    )?),
    /// Negate a value
    (neg, |x| x.val.un_scalar_op("neg", x.span, |x| -x)?),
    /// Map a value from the range [-1, 1] to [0, 1]
    (pos, |x| x
        .val
        .un_scalar_op("pos", x.span, |x| x * 0.5 + 0.5)?),
    /// Get the absolute value of a value
    (abs, |x| x.val.un_scalar_op("abs", x.span, f64::abs)?),
    /// Get the square root of a value
    (sqrt, |x| x.val.un_scalar_op("sqrt", x.span, f64::sqrt)?),
    /// Get e raised to a value
    (exp, |x| x.val.un_scalar_op("exp", x.span, f64::exp)?),
    /// Pan
    (pan, |pan, value| {
        state_node(
            "pan",
            (pan.val.into_node(), value.val.into_node()),
            |(pan, value), env| {
                let pan = pan.sample(env).average();
                let value = value.sample(env).average();
                Stereo::pan(value, pan)
            },
        )
    }),
    /// Get the frequency of a beat subdivided into `n` parts at the current tempo
    (perbeat, |n| {
        state_node("perbeat", n.val.into_node(), move |n, env| {
            n.sample(env) * env.beat_freq()
        })
    }),
    /// Get the period that is an `n`th of a beat at the current tempo
    (beat, |n| {
        state_node("beat", n.val.into_node(), move |n, env| {
            1.0 / env.beat_freq() / n.sample(env)
        })
    }),
    /// Get the period of `n` beats at the current tempo
    (beats, |n| {
        state_node("beats", n.val.into_node(), move |n, env| {
            n.sample(env) / env.beat_freq()
        })
    }),
    /// Alias for `sec (perbeat n) values`
    (sperbeat, |n, values| {
        let perbeat = state_node("perbeat", n.val.into_node(), move |n, env| {
            n.sample(env) * env.beat_freq()
        });
        section(perbeat, values.val)?
    }),
    /// Alias for `sec (beat n) values`
    (sbeat, |n, values| {
        let beat = state_node("beat", n.val.into_node(), move |n, env| {
            1.0 / env.beat_freq() / n.sample(env)
        });
        section(beat, values.val)?
    }),
    /// Alias for `sec (beats n) values`
    (sbeats, |n, values| {
        let beats = state_node("beats", n.val.into_node(), move |n, env| {
            n.sample(env) / env.beat_freq()
        });
        section(beats, values.val)?
    }),
    /// Create looping sections from some values
    ///
    /// Each value will be played for the `period`.
    ///
    /// ## Example
    /// ```
    /// square 110 * max 0 (saw (sec (beat 4) |2 8))
    /// ```
    (sec, |period, values| section(
        period.val.into_node(),
        values.val
    )?),
    /// Select from `values` using `indices`
    (sel, span, |indices, values| {
        let indices = indices.val.into_list();
        let values = values.val.into_list();
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
    /// Evaluate the `value` with the `offset` added to the current time
    (offset, |offset, value| {
        state_node(
            "offset",
            (offset.val.into_node(), value.val.into_node()),
            |(offset, value), env| {
                let offset = offset.sample(env).average();
                env.time += offset;
                let value = value.sample(env);
                env.time -= offset;
                value
            },
        )
    }),
    /// Apply a low-pass filter to a `value` with the given `cutoff` frequency
    (lowpass, |cutoff, value| NodeBox::new(LowPass::new(
        cutoff.val.into_node(),
        value.val.into_node(),
    ))),
    /// Apply a basic reverb effect to a `value` with the given `period` and `n` reflections
    (reverb, |period, n, value| NodeBox::new(Reverb::new(
        period.val.into_node(),
        n.val.into_node(),
        value.val.into_node(),
    ))),
    /// Join all arguments into a single list
    (join, |[values]| {
        let mut joined = Vec::new();
        for value in values {
            joined.extend(value.val.into_list());
        }
        Value::List(joined)
    }),
    /// Flatten a list of lists into a single list
    (flatten, |values| {
        Value::List(
            values
                .val
                .into_list()
                .into_iter()
                .flat_map(Value::into_list)
                .collect(),
        )
    }),
    /// Call a function with the arguments reversed
    (flip, |function, [args]| {
        args.reverse();
        call(function, args)?.val
    }),
    /// Call `f(g(x ...xs))`
    (atop, |f, g, x, [xs]| {
        xs.insert(0, x);
        let gx = call(g, xs)?;
        call(f, vec![gx])?.val
    }),
    /// Call `f(g(x) g(y ...ys))`
    (over, |f, g, x, y, [ys]| {
        ys.insert(0, y);
        let gx = call(g.clone(), vec![x])?;
        let gy = call(g, ys)?;
        call(f, vec![gx, gy])?.val
    }),
    /// Call `f(g(x) h(y ...ys))`
    (fork, |f, g, h, x, y, [ys]| {
        ys.insert(0, y);
        let gx = call(g, vec![x])?;
        let hy = call(h, ys)?;
        call(f, vec![gx, hy])?.val
    }),
    /// Call `f(g(x ...xs) y)`
    (lhook, |f, g, x, y, [ys]| {
        ys.insert(0, y);
        let gx = call(g, vec![x])?;
        ys.insert(0, gx);
        call(f, ys)?.val
    }),
    /// Call `f(x g(y ...ys))`
    (rhook, |f, g, x, y, [ys]| {
        ys.insert(0, y);
        let gy = call(g, ys)?;
        call(f, vec![x, gy])?.val
    }),
    /// Apply a functions to each item in a list
    (map, span, |f, xs| {
        let mut mapped = Vec::new();
        for x in xs.val.into_list() {
            mapped.push(call(f.clone(), vec![span.sp(x)])?.val);
        }
        Value::List(mapped)
    }),
    /// Bind a function to some arguments
    (bind, |f, [args]| Value::Bind(f.into(), args)),
);

fn section(period: impl Node + Clone, values: Value) -> CompileResult<Value> {
    let nodes: Vec<NodeBox> = values
        .into_list()
        .into_iter()
        .map(|v| v.into_node())
        .collect();
    Ok(
        state_node("sections", (nodes, period), move |(nodes, period), env| {
            let period = period.sample(env).average();
            let i = (modulus(env.time, period * nodes.len() as f64) / period) as usize;
            nodes[i].sample(env)
        })
        .into(),
    )
}

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
