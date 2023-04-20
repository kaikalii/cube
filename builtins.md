# `sin`
<code><b>sin</b> freq</code>

Generate a sine wave from a frequency

# `square`
<code><b>square</b> freq</code>

Generate a square wave from a frequency

# `saw`
<code><b>saw</b> freq</code>

Generate a saw wave from a frequency

# `tri`
<code><b>tri</b> freq</code>

Generate a triangle wave from a frequency

# `kick`
<code><b>kick</b> freq [high = 40] [falloff = 0.5]</code>

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

# `beat`
<code><b>beat</b> n</code>

Get the frequency of a beat subdivided into `n` parts at the current tempo

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

