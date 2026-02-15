# Full alcohol purchase task dataset

A larger dataset containing alcohol purchase task data with demographic
covariates. Suitable for testing hurdle models and mixed-effects models
with covariates.

## Usage

``` r
apt_full
```

## Format

A data frame with 18,700 rows and 8 columns:

- id:

  Unique participant identifier (1-1100)

- gender:

  Participant gender (Male/Female)

- age:

  Participant age in years

- binges:

  Number of binge drinking episodes

- totdrinks:

  Total number of drinks consumed

- tothours:

  Total hours spent drinking

- x:

  Price point for the purchase task

- y:

  Number of drinks participant would purchase at price x

## Examples

``` r
if (FALSE) { # \dontrun{
data(apt_full)

# Fit hurdle model
fit <- fit_demand_hurdle(apt_full, y_var = "y", x_var = "x", id_var = "id")

# Fit mixed model with gender as factor
fit_mixed <- fit_demand_mixed(apt_full, y_var = "y", x_var = "x", id_var = "id",
                              factors = "gender")
} # }
```
