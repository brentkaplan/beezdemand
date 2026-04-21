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
# \donttest{
data(apt)
fit <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
#> Sample size may be too small for reliable estimation.
#>   Subjects: 10, Parameters: 12, Recommended minimum: 60 subjects.
#>   Consider using more subjects or the simpler 2-RE model.
#> Fitting HurdleDemand3RE model...
#>   Part II: zhao_exponential
#>   Subjects: 10, Observations: 160
#>   Fixed parameters: 12, Random effects per subject: 3
#>   Optimizing...
#>   Converged in 81 iterations
#>   Computing standard errors...
#> Done. Log-likelihood: 32.81
pars <- get_subject_pars(fit)
head(pars)
#>    id       a_i        b_i        c_i        Q0      alpha breakpoint      Pmax
#> 1  19 -24.18572  0.4327276 -0.1848782 10.023473 0.01460071  21.256530 13.292153
#> 2  30 -51.52766 -0.8325815 -0.3938517  2.828144 0.01184726  27.643280 16.381413
#> 3  38  18.03621 -0.3566930 -0.2240901  4.551737 0.01403927  14.167670 13.823717
#> 4  60  41.85352  0.3917919  0.4987260  9.621440 0.02892408  11.269526  6.709803
#> 5  68  23.55110  0.4511080  0.1328454 10.209411 0.02006133  13.436440  9.674080
#> 6 106  82.94144 -0.1452772  0.3451208  5.623335 0.02480560   7.593382  7.823835
#>       Omax Pmax_unconditional Omax_unconditional
#> 1 44.18881          13.292153           44.18881
#> 2 15.36567          16.381413           15.36567
#> 3 20.86896          13.137907           20.83931
#> 4 21.41158           6.709803           21.41158
#> 5 32.75738           9.674081           32.75738
#> 6 14.59192           7.085743           14.52454
# }
```
