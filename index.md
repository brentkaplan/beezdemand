# beezdemand

Behavioral economic demand analysis for purchase task data: exponential
and modified exponential curves, mixed-effects models, and hurdle
models.

## Install

From CRAN:

``` r

install.packages("beezdemand")
```

Development version:

``` r

# install.packages("pak")
pak::pak("brentkaplan/beezdemand")
```

## 1-minute example

``` r

library(beezdemand)

fit <- fit_demand_fixed(apt, id_var = "id", x_var = "price", y_var = "consumption")
summary(fit)
```

## Next

- Get started:
  [`vignette("beezdemand")`](https://brentkaplan.github.io/beezdemand/articles/beezdemand.md)
  or the online article: [Getting
  Started](https://brentkaplan.github.io/beezdemand/articles/beezdemand.md)
- Explore models:
  [Hurdle](https://brentkaplan.github.io/beezdemand/articles/hurdle-demand-models.md),
  [Mixed-Effects](https://brentkaplan.github.io/beezdemand/articles/mixed-demand.md),
  [Cross-Price](https://brentkaplan.github.io/beezdemand/articles/cross-price-models.md)
- Browse functions:
  [Reference](https://brentkaplan.github.io/beezdemand/reference/index.md)
