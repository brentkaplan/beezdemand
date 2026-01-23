# beezdemand <img src="reference/figures/logo.png" alt="beezdemand logo" align="right" height="140" />

Behavioral economic demand analysis for purchase task data: exponential and modified exponential curves, mixed-effects models, and hurdle models.

## Install

From CRAN:

```r
install.packages("beezdemand")
```

Development version:

```r
# install.packages("pak")
pak::pak("brentkaplan/beezdemand")
```

## 1-minute example

```r
library(beezdemand)

fit <- fit_demand_fixed(apt, id_var = "id", x_var = "price", y_var = "consumption")
summary(fit)
```

## Next

- Get started: `vignette("beezdemand")` or the online article: [Getting Started](articles/beezdemand.html)
- Explore models: [Hurdle](articles/hurdle-demand-models.html), [Mixed-Effects](articles/mixed-demand.html), [Cross-Price](articles/cross-price-models.html)
- Browse functions: [Reference](reference/index.html)

