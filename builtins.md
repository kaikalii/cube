# `sin`
**`sin`** `freq`

Generate a sine wave from a frequency

# `square`
**`square`** `freq`

Generate a square wave from a frequency

# `saw`
**`saw`** `freq`

Generate a saw wave from a frequency

# `tri`
**`tri`** `freq`

Generate a triangle wave from a frequency

# `min`
**`min`** `a ...rest`

Get the minimum of two or more values

# `max`
**`max`** `a ...rest`

Get the maximum of two or more values

# `pow`
**`pow`** `a b`

Raise a value to a power

# `log`
**`log`** `a b`

Get the logarithm of a value

# `neg`
**`neg`** `x`

Negate a value

# `abs`
**`abs`** `x`

Get the absolute value of a value

# `sqrt`
**`sqrt`** `x`

Get the square root of a value

# `exp`
**`exp`** `x`

Get e raised to a value

# `x`
**`x`** `v`

Get the x component of a vector

# `y`
**`y`** `v`

Get the y component of a vector

# `z`
**`z`** `v`

Get the z component of a vector

# `len`
**`len`** `v`

Get the length of a vector

# `vec`
**`vec`** `v`

Create a new vector

Passing a single value will create a vector with all components equal to that value.
Passing two values will create a vector with the x and y components equal to those values.
Passing three values will create a vector with the x, y, and z components equal to those values.

# `sec`
**`sec`** `offset period first ...rest`

Create looping sections from some values

With an offset at `offset`, each section will be played for the `period`.

## Example
```
square 110 * max 0 (saw (sec 0 1 2 8))
```

