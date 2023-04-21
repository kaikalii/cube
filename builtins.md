# `sin`
<code><b>sin</b>  ...freqs</code>

Generate a sine wave from a frequency

# `square`
<code><b>square</b>  ...freqs</code>

Generate a square wave from a frequency

# `saw`
<code><b>saw</b>  ...freqs</code>

Generate a saw wave from a frequency

# `tri`
<code><b>tri</b>  ...freqs</code>

Generate a triangle wave from a frequency

# `hsquare`
<code><b>hsquare</b> n ...freqs</code>

Generate an additive square wave from a frequency and number of harmonics

# `hsaw`
<code><b>hsaw</b> n ...freqs</code>

Generate an additive saw wave from a frequency and number of harmonics

# `htri`
<code><b>htri</b> n ...freqs</code>

Generate an additive triangle wave from a frequency and number of harmonics

# `kick`
<code><b>kick</b> freq [high = 40] [falloff = 0.5]</code>

Generate kick drum sound

# `min`
<code><b>min</b> a ...rest</code>

Get the minimum of two or more values or a vector

# `max`
<code><b>max</b> a ...rest</code>

Get the maximum of two or more values or a vector

# `pow`
<code><b>pow</b> a b</code>

Raise a value to a power

# `log`
<code><b>log</b> a b</code>

Get the logarithm of a value

# `neg`
<code><b>neg</b> x</code>

Negate a value

# `abs`
<code><b>abs</b> x</code>

Get the absolute value of a value

# `sqrt`
<code><b>sqrt</b> x</code>

Get the square root of a value

# `exp`
<code><b>exp</b> x</code>

Get e raised to a value

# `x`
<code><b>x</b> v</code>

Get the x component of a vector

# `y`
<code><b>y</b> v</code>

Get the y component of a vector

# `z`
<code><b>z</b> v</code>

Get the z component of a vector

# `len`
<code><b>len</b> v</code>

Get the length of a vector

# `vec`
<code><b>vec</b> x [y = x] [z = x]</code>

Create a new vector

Passing a single value will create a vector with all components equal to that value.
Passing three values will create a vector with the x, y, and z components equal to those values.

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
<code><b>sec</b> offset period first ...rest</code>

Create looping sections from some values

With an offset at `offset`, each section will be played for the `period`.

## Example
```
square 110 * max 0 (saw (sec 0 1 2 8))
```

# `args`
<code><b>args</b>  ...args</code>

Collect args into an args list that can be bound to a name

# `gtx`
<code><b>gtx</b> x</code>

Check if the current x position is greater than a value

# `gty`
<code><b>gty</b> y</code>

Check if the current y position is greater than a value

# `gtz`
<code><b>gtz</b> z</code>

Check if the current z position is greater than a value

# `ltx`
<code><b>ltx</b> x</code>

Check if the current x position is less than a value

# `lty`
<code><b>lty</b> y</code>

Check if the current y position is less than a value

# `ltz`
<code><b>ltz</b> z</code>

Check if the current z position is less than a value

