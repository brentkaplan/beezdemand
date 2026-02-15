# Print Method for beezdemand_nlme Objects

Provides a concise summary of a \`beezdemand_nlme\` object, typically
displaying the call, model specifications, and key results from the
\`nlme\` fit if successful.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
print(x, digits = max(3L, getOption("digits") - 3L), ...)
```

## Arguments

- x:

  An object of class \`beezdemand_nlme\`.

- digits:

  Minimal number of significant digits, see \`print.default\`.

- ...:

  Additional arguments passed to \`print.nlme\` if the model exists.

## Value

Invisibly returns the original object \`x\`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'fit_two_factor_no_interaction' is a beezdemand_nlme object
print(fit_two_factor_no_interaction)

# If fitting failed:
# fit_failed <- fit_demand_mixed(..., nlme_control=list(maxIter=1)) # To force a failure
# print(fit_failed)
} # }
```
