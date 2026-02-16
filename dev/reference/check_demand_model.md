# Check Demand Model Diagnostics

Performs diagnostic checks on fitted demand models, returning
information about convergence, boundary conditions, and residual
patterns.

## Usage

``` r
check_demand_model(object, ...)

# S3 method for class 'beezdemand_hurdle'
check_demand_model(object, ...)

# S3 method for class 'beezdemand_nlme'
check_demand_model(object, ...)

# S3 method for class 'beezdemand_fixed'
check_demand_model(object, ...)
```

## Arguments

- object:

  A fitted model object of class `beezdemand_hurdle`, `beezdemand_nlme`,
  or `beezdemand_fixed`.

- ...:

  Additional arguments passed to methods.

## Value

An object of class `beezdemand_diagnostics` containing:

- convergence:

  List with convergence status and messages

- boundary:

  List with boundary condition warnings

- residuals:

  Summary statistics for residuals

- random_effects:

  Summary of random effects (if applicable)

- issues:

  Character vector of identified issues

- recommendations:

  Character vector of recommendations

## Details

The function checks for:

- Convergence status and optimization messages

- Parameters at or near boundaries

- Residual patterns (heteroscedasticity, outliers)

- Random effect variance estimates near zero

- Correlation matrices near singularity

## Note

This function is named `check_demand_model()` to avoid potential
conflicts with
[`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html)
from the performance package.

## See also

[`plot_residuals()`](https://brentkaplan.github.io/beezdemand/reference/plot_residuals.md),
[`plot_qq()`](https://brentkaplan.github.io/beezdemand/reference/plot_qq.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")
diagnostics <- check_demand_model(fit)
print(diagnostics)
} # }
```
