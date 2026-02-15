# Get Starting Values from a Pooled NLS Model (Internal Helper)

Fits a simpler, pooled NLS model (ignoring random effects and fixed
effect factors) to derive initial estimates for global Q0 and alpha
parameters. These are then used as starting values for the main NLME
model intercepts.

## Usage

``` r
get_pooled_nls_starts(data, y_var, x_var, equation_form)
```

## Arguments

- data:

  The input data frame.

- y_var:

  Name of the y-variable.

- x_var:

  Name of the x-variable.

- equation_form:

  The equation form ("zben" or "simplified").

## Value

A named list with \`Q0\` and \`alpha\` starting values if successful,
else \`NULL\`.
