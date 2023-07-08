# `add`
<code><b>add</b> a b</code>

Named alias for `+`

# `sub`
<code><b>sub</b> a b</code>

Named alias for `-`

# `mul`
<code><b>mul</b> a b</code>

Named alias for `*`

# `div`
<code><b>div</b> a b</code>

Named alias for `/`

# `lt`
<code><b>lt</b> a b</code>

Named alias for `<`

# `le`
<code><b>le</b> a b</code>

Named alias for `<=`

# `gt`
<code><b>gt</b> a b</code>

Named alias for `>`

# `ge`
<code><b>ge</b> a b</code>

Named alias for `>=`

# `sine`
<code><b>sine</b> freqs</code>

Generate a sine wave from a frequency

# `square`
<code><b>square</b> freqs</code>

Generate a square wave from a frequency

# `saw`
<code><b>saw</b> freqs</code>

Generate a saw wave from a frequency

# `tri`
<code><b>tri</b> freqs</code>

Generate a triangle wave from a frequency

# `curve`
<code><b>curve</b> falloff freqs</code>

A curved saw wave

`curve 1` is equivalent to `saw`

# `hsquare`
<code><b>hsquare</b> n freqs</code>

Generate an additive square wave from a frequency and number of harmonics

# `hsaw`
<code><b>hsaw</b> n freqs</code>

Generate an additive saw wave from a frequency and number of harmonics

# `htri`
<code><b>htri</b> n freqs</code>

Generate an additive triangle wave from a frequency and number of harmonics

# `kick`
<code><b>kick</b> period [falloff = 0.4] [high = 40]</code>

Generate kick drum sound

# `min`
<code><b>min</b> a ...rest</code>

Get the minimum of two or more values

# `max`
<code><b>max</b> a ...rest</code>

Get the maximum of two or more values

# `pow`
<code><b>pow</b> a b</code>

Raise a value to a power

# `log`
<code><b>log</b> a b</code>

Get the logarithm of a value

# `neg`
<code><b>neg</b> x</code>

Negate a value

# `pos`
<code><b>pos</b> x</code>

Map a value from the range [-1, 1] to [0, 1]

# `abs`
<code><b>abs</b> x</code>

Get the absolute value of a value

# `sqrt`
<code><b>sqrt</b> x</code>

Get the square root of a value

# `exp`
<code><b>exp</b> x</code>

Get e raised to a value

# `pan`
<code><b>pan</b> pan value</code>

Pan

# `perbeat`
<code><b>perbeat</b> n</code>

Get the frequency of a beat subdivided into `n` parts at the current tempo

# `beat`
<code><b>beat</b> n</code>

Get the period that is an `n`th of a beat at the current tempo

# `beats`
<code><b>beats</b> n</code>

Get the period of `n` beats at the current tempo

# `sec`
<code><b>sec</b> period values</code>

Create looping sections from some values

Each value will be played for the `period`.

## Example
```
0.5 square . sec (beat 2) |C2~ D E F G
```

# `sperbeat`
<code><b>sperbeat</b> n values</code>

Alias for `sec (perbeat n) values`

# `sbeat`
<code><b>sbeat</b> n values</code>

Alias for `sec (beat n) values`

# `sbeats`
<code><b>sbeats</b> n values</code>

Alias for `sec (beats n) values`

# `sel`
<code><b>sel</b> indices values</code>

Select from `values` using `indices`

# `offset`
<code><b>offset</b> offset value</code>

Evaluate the `value` with the `offset` added to the current time

# `lowpass`
<code><b>lowpass</b> cutoff value</code>

Apply a low-pass filter to a `value` with the given `cutoff` frequency

# `reverb`
<code><b>reverb</b> period n value</code>

Apply a basic reverb effect to a `value` with the given `period` and `n` reflections

# `join`
<code><b>join</b>  ...values</code>

Join all arguments into a single list

# `flatten`
<code><b>flatten</b> values</code>

Flatten a list of lists into a single list

# `flip`
<code><b>flip</b> function ...args</code>

Call a function with the arguments reversed

# `atop`
<code><b>atop</b> f g x ...xs</code>

Call `f(g(x ...xs))`

# `over`
<code><b>over</b> f g x y ...ys</code>

Call `f(g(x) g(y ...ys))`

# `fork`
<code><b>fork</b> f g h x y ...ys</code>

Call `f(g(x) h(y ...ys))`

# `lhook`
<code><b>lhook</b> f g x y ...ys</code>

Call `f(g(x ...xs) y)`

# `rhook`
<code><b>rhook</b> f g x y ...ys</code>

Call `f(x g(y ...ys))`

# `map`
<code><b>map</b> f xs</code>

Apply a functions to each item in a list

# `bind`
<code><b>bind</b> f ...args</code>

Bind a function to some arguments

