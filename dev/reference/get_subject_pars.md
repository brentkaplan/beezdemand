# Get Subject-Specific Parameters

Convenience function to extract subject-specific demand parameters from
a fitted hurdle demand model. Equivalent to accessing
`object$subject_pars`.

## Usage

``` r
get_subject_pars(object)
```

## Arguments

- object:

  A fitted `beezdemand_hurdle` object.

## Value

Data frame with subject-specific parameters including:

- id:

  Subject identifier

- a_i:

  Random effect for Part I (zeros)

- b_i:

  Random effect for Part II (Q0)

- c_i:

  Random effect for alpha (3-RE model only)

- Q0:

  Subject-specific intensity (consumption at price 0)

- alpha:

  Subject-specific elasticity

- breakpoint:

  Price where P(quit) = 0.5

- Pmax:

  Price at maximum expenditure

- Omax:

  Maximum expenditure

## See also

[`fit_demand_hurdle`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
pars <- get_subject_pars(fit)
head(pars)
} # }
```
