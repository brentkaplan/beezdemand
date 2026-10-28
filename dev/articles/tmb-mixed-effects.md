# TMB Mixed-Effects Demand Models

## Introduction

[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
fits continuous mixed-effects demand models using [Template Model
Builder](https://github.com/kaskr/adcomp) (TMB). It is the modern
alternative to
[`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
(which uses `nlme`) and provides several advantages:

- **Automatic differentiation**: exact gradients via compiled C++,
  replacing the numerical finite-difference approximations used by
  `nlme`
- **Laplace approximation**: integrates over random effects analytically
  rather than relying on iterative linearization
- **Multi-start optimization**: automatically tries multiple
  starting-value sets and keeps the best fit
- **Four equation forms**: exponential, exponentiated, simplified, and
  zero-bounded exponential (zben)
- **Factor and covariate support**: design matrices for group
  comparisons with estimated marginal means (EMMs) and pairwise
  contrasts

This vignette covers
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
for continuous consumption data. For **two-part hurdle models** that
explicitly model zero consumption, see
[`vignette("hurdle-demand-models")`](https://brentkaplan.github.io/beezdemand/articles/hurdle-demand-models.md).
For **individual NLS curves**, see
[`vignette("fixed-demand")`](https://brentkaplan.github.io/beezdemand/articles/fixed-demand.md).

## Quick Start

``` r

fit <- fit_demand_tmb(
  dat,
  y_var = "y", x_var = "x", id_var = "id",
  equation = "exponential",
  random_effects = c("q0", "alpha"),
  verbose = 0
)
fit
#> 
#> TMB Mixed-Effects Demand Model
#> 
#> Call:
#> fit_demand_tmb(data = dat, y_var = "y", x_var = "x", id_var = "id", 
#>     equation = "exponential", random_effects = c("q0", "alpha"), 
#>     verbose = 0)
#> 
#> Equation: exponential 
#> Convergence: Yes 
#> Number of subjects: 99 
#> Number of observations: 1131 
#> Observations dropped (zeros): 569 
#> Random effects: 2 (q0, alpha) 
#> Log-likelihood: -297.55 
#> AIC: 607.1 
#> 
#> Fixed Effects:
#>       Q0.0    alpha.0   logsigma   logsigma logsigma_e    rho_raw 
#>     1.6611    -4.9277    -0.3234    -0.1093    -1.4600    -0.6190 
#> 
#> Use summary() for full results.
```

``` r

summary(fit)
#> 
#> TMB Mixed-Effects Demand Model Summary
#> ================================================== 
#> 
#> Equation: exponential 
#> Backend: TMB_mixed 
#> Convergence: Yes 
#> Subjects: 99  Observations: 1131 
#> 
#> --- Fixed Effects ---
#>               term estimate std.error statistic  p.value
#>     Q0:(Intercept)   5.2652    0.3920   22.3123  < 2e-16
#>  alpha:(Intercept)   0.0072    0.0007  -50.6088  < 2e-16
#>           logsigma  -0.3234    0.0748   -4.3244 1.53e-05
#>           logsigma  -0.1093    0.0817   -1.3389    0.181
#>         logsigma_e  -1.4600    0.0232  -62.8668  < 2e-16
#>            rho_raw  -0.6190    0.1307   -4.7354 2.19e-06
#> 
#> --- Variance Components ---
#> (Q0/alpha RE SDs on log10 scale; residual SD on likelihood scale)
#>              Component Estimate
#>     sigma_b (Q0 RE SD)   0.3143
#>  sigma_c (alpha RE SD)   0.3893
#>  sigma_e (Residual SD)   0.2322
#> 
#> --- RE Correlations ---
#>                      Component Estimate
#>  rho_bc (Q0-alpha correlation)  -0.5504
#> 
#> --- Fit Statistics ---
#> Log-likelihood: -297.55 
#> AIC: 607.1 
#> BIC: 637.28 
#> 
#> --- Population Demand Metrics ---
#> Pmax: 7.6114  Omax: 12.5597  Method: analytic_lambert_w
#> 
#> --- Individual Parameter Summaries ---
#>   Q0: Min=1.0852  Med=5.6123  Mean=6.6880  Max=26.2169
#>   alpha: Min=0.0008  Med=0.0070  Mean=0.0117  Max=0.1083
#>   Pmax: Min=0.4518  Med=7.9771  Mean=8.8033  Max=37.9008
#>   Omax: Min=0.8402  Med=12.9591  Mean=16.9925  Max=112.4944
#> 
#> Notes:
#>   * 569 zero-consumption observations dropped for equation='exponential'.
```

``` r

plot(fit, type = "demand")
```

![Population demand curve from the exponential
equation.](tmb-mixed-effects_files/figure-html/quick-plot-1.png)

Population demand curve from the exponential equation.

The `exponential` equation (Hursh & Silberberg, 2008) models
log-transformed consumption and is the most reliable choice for
2-random-effect models. It automatically drops zero-consumption
observations.

## Choosing an Equation

[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
supports four demand equations:

| Equation | Response | Zeros | k | Best for |
|----|----|----|----|----|
| `"exponential"` | log(Q) | Dropped | Fixed at 2; optionally estimated | Most datasets; the usual choice for 2-RE models |
| `"exponentiated"` | Raw Q | Allowed | Fixed at 2; optionally estimated | Data with few zeros; 1-RE models |
| `"simplified"` | Raw Q | Allowed | None | Simpler model without k |
| `"zben"` | LL4(Q) | Allowed (via transform) | None | Wide dynamic range with LL4 compression |

### Mathematical Specifications

**Exponential** (Hursh & Silberberg, 2008): \log\_{10}(Q\_{ij}) =
\log\_{10}(Q\_{0i}) + k \left(e^{-\alpha_i \cdot Q\_{0i} \cdot C_j} -
1\right) + \varepsilon\_{ij} (fit on the natural-log scale, where the k
term is multiplied by \ln 10; the k reported is the base-10 span of the
original equation)

**Exponentiated** (Koffarnus et al., 2015): Q\_{ij} = Q\_{0i} \cdot
10^{k \left(e^{-\alpha_i \cdot Q\_{0i} \cdot C_j} - 1\right)} +
\varepsilon\_{ij}

**Simplified**: Q\_{ij} = Q\_{0i} \cdot e^{-\alpha_i \cdot Q\_{0i} \cdot
C_j} + \varepsilon\_{ij}

**Zero-bounded exponential (zben)** (see `src/MixedDemand.h` for full
specification): \log\_{10}(Q\_{0i}) \cdot
e^{-\frac{\alpha_i}{\log\_{10}(Q\_{0i})} \cdot Q\_{0i} \cdot C_j} +
\varepsilon\_{ij}

where Q\_{0i} = \exp(\mathbf{x}\_i^\top \boldsymbol{\beta}\_{Q_0} +
b_i), \alpha_i = \exp(\mathbf{x}\_i^\top \boldsymbol{\beta}\_\alpha +
c_i), and (b_i, c_i) are correlated random effects.

``` r

fit_exp <- fit_demand_tmb(dat, equation = "exponential",
                          random_effects = c("q0", "alpha"), verbose = 0)
```

``` r

# exponentiated converges reliably with 1-RE
fit_expon <- fit_demand_tmb(dat, equation = "exponentiated",
                            random_effects = "q0", verbose = 0)
```

``` r

fit_simp <- fit_demand_tmb(dat, equation = "simplified",
                           random_effects = "q0", verbose = 0)
```

``` r

dat$y_ll4 <- ll4(dat$y)
fit_zben <- fit_demand_tmb(dat, y_var = "y_ll4", equation = "zben",
                           random_effects = "q0", verbose = 0)
```

``` r

data.frame(
  equation = c("exponential", "exponentiated", "simplified", "zben"),
  random_effects = c("2-RE", "1-RE", "1-RE", "1-RE"),
  converged = c(fit_exp$converged, fit_expon$converged,
                fit_simp$converged, fit_zben$converged),
  AIC = round(c(AIC(fit_exp), AIC(fit_expon),
                AIC(fit_simp), AIC(fit_zben)), 1)
)
#>        equation random_effects converged    AIC
#> 1   exponential           2-RE      TRUE  607.1
#> 2 exponentiated           1-RE      TRUE 6746.9
#> 3    simplified           1-RE      TRUE 6743.4
#> 4          zben           1-RE      TRUE -893.6
```

AIC is comparable only between equations that model the same response
scale. Here `exponentiated` and `simplified` both model raw Q and can be
compared with each other; `exponential` models log Q and `zben` models
LL4(Q), so their AIC values are not comparable with the raw-Q equations
or with each other.

For convergence, the `exponential` equation is the most reliable choice
for 2-random-effect models. The `exponentiated` equation works well with
1-RE but can struggle to converge with 2-RE, especially with smaller
samples.

## Random Effects Structure

[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
supports two configurations:

- `random_effects = "q0"` (1-RE): random intercept on Q_0 only; \alpha
  is constant across subjects
- `random_effects = c("q0", "alpha")` (2-RE): random effects on both Q_0
  and \alpha with an estimated correlation

``` r

fit_1re <- fit_demand_tmb(dat, equation = "exponential",
                          random_effects = "q0", verbose = 0)
fit_2re <- fit  # reuse from Quick Start (exponential, 2-RE)
```

``` r

compare_models(fit_1re, fit_2re)
#> 
#> Model Comparison
#> ================================================== 
#> 
#>    Model          Class   Backend nobs df    logLik       AIC       BIC
#>  Model_1 beezdemand_tmb TMB_mixed 1131  4 -703.9366 1415.8732 1435.9967
#>  Model_2 beezdemand_tmb TMB_mixed 1131  6 -297.5496  607.0992  637.2843
#>  delta_AIC delta_BIC
#>   808.7741  798.7124
#>     0.0000    0.0000
#> 
#> Best model by BIC: Model_2 
#> 
#> Likelihood Ratio Tests:
#> ---------------------------------------- 
#>          Comparison  LR_stat df p_value
#>  Model_1 vs Model_2 812.7741  2  <2e-16
#> 
#> Notes:
#>   - LRT nesting assumption not verified.
```

A significant LRT p-value and substantially lower AIC/BIC for the 2-RE
model indicate that subjects differ meaningfully in both intensity (Q_0)
and elasticity (\alpha).

``` r

head(nlme::ranef(fit_2re))
#>   id        b_i        c_i q0_(Intercept) alpha_(Intercept)
#> 1 16 -0.8231033  0.4178510     -0.8231033         0.4178510
#> 2 24  0.1184444 -0.0807627      0.1184444        -0.0807627
#> 3 33  0.6447745 -0.3544123      0.6447745        -0.3544123
#> 4 40  0.1793134  0.5066451      0.1793134         0.5066451
#> 5 42  0.7535770 -0.9183795      0.7535770        -0.9183795
#> 6 49 -0.2751488  0.4130012     -0.2751488         0.4130012
```

The `b_i` column is the random deviation on log(Q_0) and `c_i` is the
random deviation on log(\alpha) for each subject.

## Fixing vs. Estimating k

For the `exponential` and `exponentiated` equations, k scales the range
of the demand curve. By default it is held at 2 (`estimate_k = FALSE`),
the convention of Hursh & Silberberg (2008) and the default of
[`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md).
Setting `estimate_k = TRUE` estimates it instead:

``` r

fit_k_fixed <- fit_demand_tmb(dat, equation = "exponential",
                               random_effects = c("q0", "alpha"),
                               verbose = 0)
fit_k_free <- fit_demand_tmb(dat, equation = "exponential",
                              random_effects = c("q0", "alpha"),
                              estimate_k = TRUE, verbose = 0)

data.frame(
  k = c("fixed at 2", "estimated"),
  converged = c(fit_k_fixed$converged, fit_k_free$converged),
  AIC = round(c(AIC(fit_k_fixed), AIC(fit_k_free)), 1),
  k_value = round(c(2, exp(coef(fit_k_free)[["log_k"]])), 3)
)
#>            k converged   AIC k_value
#> 1 fixed at 2      TRUE 607.1   2.000
#> 2  estimated      TRUE 551.1   1.405
```

### When the data support a free k

A free k needs curvature to estimate from. The response depends on k
through k(e^{-\alpha Q_0 C} - 1), and while \alpha Q_0 C stays small
that term is a straight line in price with slope k\alpha, so only the
product is identified. What separates k from \alpha is the bend that
appears as consumption approaches its floor, so a free k is estimable
only when the prices reach far enough for consumption to bottom out. On
data that stop short, k can drift to enormous values with a compensating
\alpha. The fit then reports a non-positive-definite Hessian, and the
derived P\_{max} and O\_{max} are meaningless.
[`check_demand_model()`](https://brentkaplan.github.io/beezdemand/reference/check_demand_model.md)
screens a free-k fit for that pattern and
[`summary()`](https://rdrr.io/r/base/summary.html) carries the note. The
screen reports what it detects; a fit that passes it has not been shown
to identify k.

Because 2 is a convention rather than an estimate, it is worth refitting
at a second value (say 1.5 or 3) and reporting how much the estimates
move. \alpha and the derived P\_{max}, O\_{max} and EV move most; Q_0
enters the likelihood jointly with \alpha, so it can shift as well.

The `simplified` and `zben` equations do not use a k parameter.

## Examining Results

### Summary and Coefficients

``` r

summary(fit_2re)
#> 
#> TMB Mixed-Effects Demand Model Summary
#> ================================================== 
#> 
#> Equation: exponential 
#> Backend: TMB_mixed 
#> Convergence: Yes 
#> Subjects: 99  Observations: 1131 
#> 
#> --- Fixed Effects ---
#>               term estimate std.error statistic  p.value
#>     Q0:(Intercept)   5.2652    0.3920   22.3123  < 2e-16
#>  alpha:(Intercept)   0.0072    0.0007  -50.6088  < 2e-16
#>           logsigma  -0.3234    0.0748   -4.3244 1.53e-05
#>           logsigma  -0.1093    0.0817   -1.3389    0.181
#>         logsigma_e  -1.4600    0.0232  -62.8668  < 2e-16
#>            rho_raw  -0.6190    0.1307   -4.7354 2.19e-06
#> 
#> --- Variance Components ---
#> (Q0/alpha RE SDs on log10 scale; residual SD on likelihood scale)
#>              Component Estimate
#>     sigma_b (Q0 RE SD)   0.3143
#>  sigma_c (alpha RE SD)   0.3893
#>  sigma_e (Residual SD)   0.2322
#> 
#> --- RE Correlations ---
#>                      Component Estimate
#>  rho_bc (Q0-alpha correlation)  -0.5504
#> 
#> --- Fit Statistics ---
#> Log-likelihood: -297.55 
#> AIC: 607.1 
#> BIC: 637.28 
#> 
#> --- Population Demand Metrics ---
#> Pmax: 7.6114  Omax: 12.5597  Method: analytic_lambert_w
#> 
#> --- Individual Parameter Summaries ---
#>   Q0: Min=1.0852  Med=5.6123  Mean=6.6880  Max=26.2169
#>   alpha: Min=0.0008  Med=0.0070  Mean=0.0117  Max=0.1083
#>   Pmax: Min=0.4518  Med=7.9771  Mean=8.8033  Max=37.9008
#>   Omax: Min=0.8402  Med=12.9591  Mean=16.9925  Max=112.4944
#> 
#> Notes:
#>   * 569 zero-consumption observations dropped for equation='exponential'.
```

The fixed-effect coefficient table reports `estimate` and `std.error` on
the scale set by `report_space`: the default, `"natural"`,
back-transforms Q_0, \alpha, and k off the log estimation scale. The
`statistic` and `p.value` columns are always computed on the estimation
(log) scale, where the Wald test is well defined; they are not
recomputed from the back-transformed `estimate` and `std.error` (that
recompute is degenerate for a strictly positive parameter because it
would test “ratio = 0” rather than “ratio = 1”). As a consequence,
`estimate / std.error` from the default table does not reproduce the
reported `statistic`. This matches the `broom` and `emmeans` convention;
pass `report_space = "internal"` (or `"log10"`) to read the estimate and
SE on the same scale as the test.

The `variance_components` block reports the Q_0 and \alpha random-effect
standard deviations on the **log10 scale**. TMB estimates these SDs on
the natural-log scale internally;
[`summary()`](https://rdrr.io/r/base/summary.html) divides them by
\log(10) so they are directly comparable with
[`nlme::VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html) on a
[`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
fit using the default `param_space = "log10"`. The residual SD is
reported on the model’s likelihood scale, which is equation-dependent
(log-consumption for `exponential`, the LL4-transformed `y_var` for
`zben`); the random-effect correlations are scale-invariant.

For users coming from `nlme` or `lme4`, the
[`VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html) accessor
returns these same variance components in the familiar
[`nlme::VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html) matrix
layout, i.e., a `Variance` / `StdDev` matrix (with a `Corr` column for
`pdSymm` fits) and a final `Residual` row:

``` r

VarCorr(fit_2re)
#>          Variance StdDev Corr 
#> Q0       0.0988   0.314       
#> alpha    0.1520   0.389  -0.55
#> Residual 0.0539   0.232
```

Note that [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
(shown next) reports the raw internal optimizer parameters instead. The
`logsigma` rows are the natural log of each RE SD, not the log10-scale
SDs from `summary()$variance_components`.

``` r

tidy(fit_2re)
#> # A tibble: 5 × 10
#>   term    estimate std.error statistic    p.value    df component estimate_scale
#>   <chr>      <dbl>     <dbl>     <dbl>      <dbl> <dbl> <chr>     <chr>         
#> 1 Q0:(In…  5.27     0.392         22.3  2.81e-110   Inf fixed     natural       
#> 2 alpha:…  0.00724  0.000705     -50.6  0           Inf fixed     natural       
#> 3 sigma_…  0.314   NA             NA   NA            NA variance  log10         
#> 4 sigma_…  0.389   NA             NA   NA            NA variance  log10         
#> 5 sigma_…  0.232   NA             NA   NA            NA variance  natural       
#> # ℹ 2 more variables: term_display <chr>, estimate_internal <dbl>
```

``` r

glance(fit_2re)
#> # A tibble: 1 × 10
#>   model_class  backend equation_form  nobs n_subjects n_random_effects converged
#>   <chr>        <chr>   <chr>         <int>      <int>            <int> <lgl>    
#> 1 beezdemand_… TMB_mi… exponential    1131         99                2 TRUE     
#> # ℹ 3 more variables: logLik <dbl>, AIC <dbl>, BIC <dbl>
```

### Subject-Specific Parameters

Each subject gets empirical Bayes estimates of Q_0, \alpha, P\_{max},
and O\_{max}:

``` r

spars <- get_subject_pars(fit_2re)
head(spars)
#>   id        b_i        c_i        Q0       alpha      Pmax      Omax
#> 1 16 -0.8231033  0.4178510  2.311769 0.011000602 11.414637  8.270063
#> 2 24  0.1184444 -0.0807627  5.927253 0.006681459  7.329904 13.616139
#> 3 33  0.6447745 -0.3544123 10.033113 0.005081908  5.693254 17.901874
#> 4 40  0.1793134  0.5066451  6.299245 0.012022070  3.833145  7.567388
#> 5 42  0.7535770 -0.9183795 11.186340 0.002891339  8.975033 31.464899
#> 6 49 -0.2751488  0.4130012  3.998693 0.010947381  6.631240  8.310269
#>   pmax_at_bound
#> 1         FALSE
#> 2         FALSE
#> 3         FALSE
#> 4         FALSE
#> 5         FALSE
#> 6         FALSE
```

``` r

spars |>
  summarise(
    across(c(Q0, alpha, Pmax, Omax),
           list(median = median, mean = mean, sd = sd),
           .names = "{.col}_{.fn}")
  ) |>
  tidyr::pivot_longer(everything(), names_to = c("parameter", "stat"),
                      names_sep = "_") |>
  tidyr::pivot_wider(names_from = stat, values_from = value) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))
#> # A tibble: 4 × 4
#>   parameter median    mean      sd
#>   <chr>      <dbl>   <dbl>   <dbl>
#> 1 Q0         5.61   6.69    4.70  
#> 2 alpha      0.007  0.0117  0.0146
#> 3 Pmax       7.98   8.80    5.48  
#> 4 Omax      13.0   17.0    16.6
```

### Amplitude–Persistence Decomposition

[`calculate_amplitude_persistence()`](https://brentkaplan.github.io/beezdemand/reference/calculate_amplitude_persistence.md)
collapses the per-subject parameters into two standardized composite
scores used in behavioral economic indices: **Amplitude** (intensity of
demand, the z-score of Q_0) and **Persistence** (sensitivity to price,
the mean of the z-scores of P\_{max}, O\_{max}, and 1/\alpha). No factor
analysis is involved; each is a plain z-score composite. The TMB method
extracts subject parameters from `fit$subject_pars` and delegates to the
default Z-score implementation, so the result is comparable across model
tiers.

``` r

ap <- calculate_amplitude_persistence(fit_2re)
head(ap)
#>   id        z_Q0      z_Pmax      z_Omax z_inv_alpha   Amplitude Persistence
#> 1 16 -0.93103155  0.47627407 -0.52701646 -0.52701646 -0.93103155  -0.1925863
#> 2 24 -0.16185398 -0.26873784 -0.20400271 -0.20400271 -0.16185398  -0.2255811
#> 3 33  0.71164905 -0.56724551  0.05494444  0.05494444  0.71164905  -0.1524522
#> 4 40 -0.08271426 -0.90650961 -0.56947256 -0.56947256 -0.08271426  -0.6818182
#> 5 42  0.95699283  0.03131627  0.87443209  0.87443209  0.95699283   0.5933935
#> 6 49 -0.57214617 -0.39616683 -0.52458721 -0.52458721 -0.57214617  -0.4817804
```

``` r

ap |>
  summarise(
    Amplitude_mean = mean(Amplitude, na.rm = TRUE),
    Persistence_mean = mean(Persistence, na.rm = TRUE),
    Amplitude_sd = sd(Amplitude, na.rm = TRUE),
    Persistence_sd = sd(Persistence, na.rm = TRUE)
  )
#>   Amplitude_mean Persistence_mean Amplitude_sd Persistence_sd
#> 1  -5.588131e-17    -2.271513e-17            1      0.8517458
```

Amplitude is a single z-score, so it has mean 0 and SD 1 within the
fitted dataset. Persistence is the mean of three z-scores and is not
re-standardized: with complete components its mean is 0, and its SD
depends on how strongly the three components correlate (it equals 1 only
when they are perfectly correlated). Missing components move both the
mean and the SD away from those values. For cross-sample comparisons,
supply external `basis_means` and `basis_sds` so the standardization
uses a fixed reference.

### Confidence Intervals

``` r

confint(fit_2re)
#> # A tibble: 6 × 5
#>   term              estimate conf.low conf.high level
#>   <chr>                <dbl>    <dbl>     <dbl> <dbl>
#> 1 Q0:(Intercept)       1.66     1.52     1.81    0.95
#> 2 alpha:(Intercept)   -4.93    -5.12    -4.74    0.95
#> 3 logsigma            -0.323   -0.470   -0.177   0.95
#> 4 logsigma            -0.109   -0.269    0.0507  0.95
#> 5 logsigma_e          -1.46    -1.51    -1.41    0.95
#> 6 rho_raw             -0.619   -0.875   -0.363   0.95
```

By default, confidence intervals are on the internal (log) scale. Use
`report_space = "natural"` for back-transformed intervals:

``` r

confint(fit_2re, report_space = "natural")
#> # A tibble: 6 × 5
#>   term              estimate conf.low conf.high level
#>   <chr>                <dbl>    <dbl>     <dbl> <dbl>
#> 1 Q0:(Intercept)     5.27     4.55      6.09     0.95
#> 2 alpha:(Intercept)  0.00724  0.00599   0.00877  0.95
#> 3 logsigma          -0.323   -0.470    -0.177    0.95
#> 4 logsigma          -0.109   -0.269     0.0507   0.95
#> 5 logsigma_e        -1.46    -1.51     -1.41     0.95
#> 6 rho_raw           -0.619   -0.875    -0.363    0.95
```

For a quick diagnostic on the Gaussian (Wald) approximation, request
Monte Carlo intervals with `method = "simulate"`. These draw `R` samples
from the asymptotic sampling approximation `N(coef, vcov)` (the same
Gaussian that Wald intervals assume; no prior is involved) and report
empirical quantiles. They are asymptotically Wald-equivalent, so a large
discrepancy between the two flags a fit where the Gaussian approximation
is suspect. The comparison is diagnostic rather than an accuracy
improvement, because the `simulate` method does not capture a
non-Gaussian sampling distribution and carries no positivity guarantee
on the internal scale. Set `seed` for reproducibility.

``` r

ci_wald <- confint(fit_2re)
ci_sim <- confint(fit_2re, method = "simulate", R = 2000, seed = 42)
data.frame(
  term = ci_wald$term,
  width_wald = ci_wald$conf.high - ci_wald$conf.low,
  width_sim = ci_sim$conf.high - ci_sim$conf.low
)
#>                term width_wald  width_sim
#> 1    Q0:(Intercept) 0.29183243 0.28646321
#> 2 alpha:(Intercept) 0.38167415 0.38942952
#> 3          logsigma 0.29319103 0.29765027
#> 4          logsigma 0.32006787 0.31764255
#> 5        logsigma_e 0.09103547 0.09041937
#> 6           rho_raw 0.51240654 0.51039522
```

### Variance-Covariance and the Delta Method

[`vcov()`](https://rdrr.io/r/stats/vcov.html) returns the fixed-effect
variance-covariance matrix from the TMB sdreport (the inverse of the
negative Hessian at the MLE, restricted to fixed effects after
Laplace-marginalizing the random effects). Combined with the optimizer’s
internal parameter vector (`coef(fit, type = "internal")`), it lets you
apply the delta method to any nonlinear function of the parameters via
[`car::deltaMethod`](https://rdrr.io/pkg/car/man/deltaMethod.html). Pass
the parameter vector explicitly so the call is stable across the planned
[`coef()`](https://rdrr.io/r/stats/coef.html) default change in a future
release.

``` r

beta <- coef(fit_2re, type = "internal")
car::deltaMethod(
  beta,
  paste0(names(beta)[1], " * 1"),
  vcov. = vcov(fit_2re)
)
#>             Estimate       SE    2.5 % 97.5 %
#> beta_q0 * 1 1.661116 0.074448 1.515200  1.807
```

`fitted(fit)` and `residuals(fit)` are also exposed as direct accessors,
matching the model-scale convention used by
[`broom::augment()`](https://generics.r-lib.org/reference/augment.html)
(log scale for `"exponential"`, natural/LL4 scale for others). Pass
`scale = "natural"` to back-transform.

### Predictions

Three prediction types are available:

``` r

# Fitted values for observed data
pred_resp <- predict(fit_2re, type = "response")
head(pred_resp)
#> # A tibble: 6 × 9
#>   id    gender   age binges totdrinks tothours     x     y .fitted
#>   <fct> <chr>  <dbl>  <dbl>     <dbl>    <dbl> <dbl> <dbl>   <dbl>
#> 1 16    Male      30      0         1        1  0        2   0.838
#> 2 16    Male      30      0         1        1  0.25     2   0.809
#> 3 16    Male      30      0         1        1  0.5      2   0.780
#> 4 16    Male      30      0         1        1  1        2   0.722
#> 5 16    Male      30      0         1        1  1.5      2   0.666
#> 6 16    Male      30      0         1        1  2        2   0.610
```

``` r

# Population demand curve at specific prices
predict(fit_2re, type = "demand", prices = c(0, 0.5, 1, 2, 5, 10, 20))
#> # A tibble: 7 × 2
#>   price .fitted
#>   <dbl>   <dbl>
#> 1   0     1.66 
#> 2   0.5   1.57 
#> 3   1     1.49 
#> 4   2     1.32 
#> 5   5     0.862
#> 6  10     0.201
#> 7  20    -0.796
```

``` r

# Subject-level parameter estimates (same as get_subject_pars)
pred_pars <- predict(fit_2re, type = "parameters")
head(pred_pars)
#> # A tibble: 6 × 8
#>   id       b_i     c_i    Q0   alpha  Pmax  Omax pmax_at_bound
#>   <chr>  <dbl>   <dbl> <dbl>   <dbl> <dbl> <dbl> <lgl>        
#> 1 16    -0.823  0.418   2.31 0.0110  11.4   8.27 FALSE        
#> 2 24     0.118 -0.0808  5.93 0.00668  7.33 13.6  FALSE        
#> 3 33     0.645 -0.354  10.0  0.00508  5.69 17.9  FALSE        
#> 4 40     0.179  0.507   6.30 0.0120   3.83  7.57 FALSE        
#> 5 42     0.754 -0.918  11.2  0.00289  8.98 31.5  FALSE        
#> 6 49    -0.275  0.413   4.00 0.0109   6.63  8.31 FALSE
```

For `type = "response"`, the `level` argument controls whether
predictions condition on subject random effects. `level = "subject"`
(the default) conditions on each subject’s random effects and needs an
`id` column in `newdata`; `level = "population"` sets the random effects
to zero and needs no `id`. Requesting both at once returns
`predict.fixed` (population) and `predict.id` (subject) side by side,
matching the `nlme::predict.lme(level = 0:1)` layout so `nlme`-based
plotting code runs unchanged.

``` r

px <- exp(seq(log(0.05), log(20), length.out = 60))
ids_show <- unique(dat$id)[1:6]

pred_levels <- predict(
  fit_2re,
  newdata = expand.grid(x = px, id = ids_show),
  level = c("population", "subject"),
  scale = "natural"
)
head(pred_levels)
#> # A tibble: 6 × 4
#>        x id    predict.fixed predict.id
#>    <dbl> <fct>         <dbl>      <dbl>
#> 1 0.05   16             5.36       2.36
#> 2 0.0553 16             5.36       2.36
#> 3 0.0613 16             5.35       2.36
#> 4 0.0678 16             5.35       2.36
#> 5 0.0751 16             5.34       2.35
#> 6 0.0831 16             5.33       2.35
```

The population-mean curve is identical for every subject, so a single
`predict.fixed` column overlays the per-subject `predict.id` curves.

``` r

ggplot(pred_levels, aes(x = x)) +
  geom_line(aes(y = predict.id, group = id), colour = "grey65") +
  geom_line(
    data = subset(pred_levels, id == ids_show[1]),
    aes(y = predict.fixed), colour = "#2c3e50", linewidth = 1.2
  ) +
  scale_x_log10() +
  labs(x = "Price", y = "Predicted consumption") +
  theme_minimal()
```

![Subject-conditional demand curves (grey) around the population-mean
curve
(dark).](tmb-mixed-effects_files/figure-html/predict-levels-plot-1.png)

Subject-conditional demand curves (grey) around the population-mean
curve (dark).

### Population Metrics

``` r

calc_group_metrics(fit_2re)
#> $Pmax
#> [1] 7.611377
#> 
#> $Omax
#> [1] 12.5597
#> 
#> $Qmax
#> [1] 1.650122
#> 
#> $elasticity_at_pmax
#> [1] -1
#> 
#> $method
#> [1] "analytic_lambert_w"
#> 
#> $pmax_at_bound
#> [1] FALSE
#> 
#> $conditioned_on
#> NULL
```

### Visualization

``` r

plot(fit_2re, type = "demand")
```

![Population-level demand curve with confidence
band.](tmb-mixed-effects_files/figure-html/plot-demand-1.png)

Population-level demand curve with confidence band.

``` r

plot(fit_2re, type = "individual")
```

![Individual demand curves for a random sample of
subjects.](tmb-mixed-effects_files/figure-html/plot-individual-1.png)

Individual demand curves for a random sample of subjects.

``` r

plot(fit_2re, type = "parameters")
```

![Distribution of subject-specific demand
parameters.](tmb-mixed-effects_files/figure-html/plot-parameters-1.png)

Distribution of subject-specific demand parameters.

## Diagnostics

After fitting, assess model health with the built-in diagnostic tools.

The fit object exposes a `hessian_pd` field that flags whether the
Hessian returned by
[`TMB::sdreport()`](https://rdrr.io/pkg/TMB/man/sdreport.html) is
positive definite. When `FALSE`, standard errors, p-values, and Wald
confidence intervals derived from the Hessian should be treated as
unreliable; the warning is also surfaced as a note in
[`summary()`](https://rdrr.io/r/base/summary.html) output and as a
`hessian_warning` attribute on
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) output.

``` r

fit_2re$hessian_pd
#> [1] TRUE
```

``` r

# Model health check: convergence, variance components, residual stats,
# and (since 0.3.0) Hessian positive-definiteness.
check_demand_model(fit_2re)
#> 
#> Model Diagnostics
#> ================================================== 
#> Model class: beezdemand_tmb 
#> 
#> Convergence:
#>   Status: Converged
#> 
#> Random Effects:
#>   sigma_b variance: 0.09877
#>   sigma_c variance: 0.1516
#> 
#> Residuals:
#>   Mean: -7.789e-05
#>   SD: 0.2133
#>   Range: [-0.8457, 1.172]
#>   Outliers: 15 observations
#> 
#> --------------------------------------------------
#> Issues Detected (1):
#>   1. Detected 15 potential outliers (|resid| > 3 SD)
#> 
#> Recommendations:
#>   - Investigate outlying observations
```

``` r

# Augment with fitted values and residuals
aug <- augment(fit_2re)
head(aug[, c("id", "x", "y", ".fitted", ".resid", ".std_resid")])
#> # A tibble: 6 × 6
#>   id        x     y .fitted  .resid .std_resid
#>   <fct> <dbl> <dbl>   <dbl>   <dbl>      <dbl>
#> 1 16     0        2   0.838 -0.145      -0.624
#> 2 16     0.25     2   0.809 -0.116      -0.498
#> 3 16     0.5      2   0.780 -0.0867     -0.373
#> 4 16     1        2   0.722 -0.0292     -0.126
#> 5 16     1.5      2   0.666  0.0275      0.118
#> 6 16     2        2   0.610  0.0835      0.360
```

``` r

# Q-Q plot of random effects
plot_qq(fit_2re)
```

![Q-Q plots of subject-level random
effects.](tmb-mixed-effects_files/figure-html/diagnostics-qq-1.png)

Q-Q plots of subject-level random effects.

``` r

# Random effects diagnostic panels
plot_re_diagnostics(fit_2re)
```

![Random effects diagnostic
panels.](tmb-mixed-effects_files/figure-html/diagnostics-re-1.png)

Random effects diagnostic panels.

``` r

# Residual plot (standard in every modeling workflow)
plot_residuals(fit_2re, type = "fitted")
```

![Residuals vs fitted values for the 2-RE exponential
model.](tmb-mixed-effects_files/figure-html/diagnostics-resid-1.png)

Residuals vs fitted values for the 2-RE exponential model.

These diagnostics help identify:

- **Q-Q plots**: Non-normality of random effects (heavy tails, outliers)
- **[`check_demand_model()`](https://brentkaplan.github.io/beezdemand/reference/check_demand_model.md)**:
  Convergence issues, boundary estimates, residual patterns
- **Augmented data**: Observation-level residuals for identifying poorly
  fitting subjects
- **Residual plots**: Heteroscedasticity, non-linearity, or outliers in
  the fitted vs residual pattern

## Advanced Visualization

`beezdemand` provides several specialized plots beyond the standard
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method. These
work with any `beezdemand_tmb` object.

### Expenditure and Elasticity

The expenditure curve shows total spending (P \times Q) as a function of
price, with vertical and horizontal reference lines at P\_{max} and
O\_{max}:

``` r

plot_expenditure(fit_2re)
```

![Expenditure curve with Pmax and Omax reference
lines.](tmb-mixed-effects_files/figure-html/viz-expenditure-1.png)

Expenditure curve with Pmax and Omax reference lines.

The elasticity curve shows how responsive demand is to price changes.
The dashed line at -1 marks unit elasticity (prices above this threshold
produce elastic demand):

``` r

plot_elasticity(fit_2re)
```

![Own-price elasticity curve with unit elasticity
reference.](tmb-mixed-effects_files/figure-html/viz-elasticity-1.png)

Own-price elasticity curve with unit elasticity reference.

### Loss Surface and Profile

The loss surface visualizes the sum-of-squared-residuals landscape over
a grid of (Q_0, \alpha) values, with the MLE marked. This helps assess
identifiability (a sharp, well-defined minimum indicates good
identification):

``` r

plot_loss_surface(fit_2re)
```

![2D loss surface (SSR) over the Q0-alpha parameter
space.](tmb-mixed-effects_files/figure-html/viz-loss-surface-1.png)

2D loss surface (SSR) over the Q0-alpha parameter space.

Profile plots show 1D slices through the loss surface, fixing one
parameter at its MLE and varying the other:

``` r

plot_loss_profile(fit_2re)
```

![1D loss profiles for Q0 and
alpha.](tmb-mixed-effects_files/figure-html/viz-loss-profile-1.png)

1D loss profiles for Q0 and alpha.

### Subject Heterogeneity

Visualize the distribution of subject-level \alpha estimates to assess
heterogeneity in price sensitivity:

``` r

plot_alpha_distribution(fit_2re)
```

![Distribution of subject-level alpha (elasticity)
estimates.](tmb-mixed-effects_files/figure-html/viz-alpha-dist-1.png)

Distribution of subject-level alpha (elasticity) estimates.

### Multi-Model Comparison

Overlay demand curves from multiple models to visualize how different
specifications affect the predicted demand function:

``` r

plot_demand_overlay(fit_1re, fit_2re, labels = c("1-RE", "2-RE"))
```

![Demand curves from 1-RE and 2-RE models
overlaid.](tmb-mixed-effects_files/figure-html/viz-overlay-1.png)

Demand curves from 1-RE and 2-RE models overlaid.

Compare parameter distributions side by side:

``` r

plot_model_comparison(fit_1re, fit_2re, labels = c("1-RE", "2-RE"))
```

![Side-by-side parameter estimates from 1-RE and 2-RE
models.](tmb-mixed-effects_files/figure-html/viz-model-comparison-1.png)

Side-by-side parameter estimates from 1-RE and 2-RE models.

## Group Comparisons

To test whether demand parameters differ by group, pass factor variables
via the `factors` argument. This adds fixed effects to the design
matrices for both Q_0 and \alpha.

``` r

# Filter to Male/Female for a clean two-level comparison
dat_mf <- dat |> filter(gender %in% c("Male", "Female"))

fit_gender <- fit_demand_tmb(
  dat_mf,
  y_var = "y", x_var = "x", id_var = "id",
  equation = "exponential",
  factors = "gender",
  random_effects = c("q0", "alpha"),
  verbose = 0
)
fit_gender
#> 
#> TMB Mixed-Effects Demand Model
#> 
#> Call:
#> fit_demand_tmb(data = dat_mf, y_var = "y", x_var = "x", id_var = "id", 
#>     equation = "exponential", random_effects = c("q0", "alpha"), 
#>     factors = "gender", verbose = 0)
#> 
#> Equation: exponential 
#> Convergence: Yes 
#> Number of subjects: 99 
#> Number of observations: 1131 
#> Observations dropped (zeros): 569 
#> Random effects: 2 (q0, alpha) 
#> Log-likelihood: -296.45 
#> AIC: 608.91 
#> 
#> Fixed Effects:
#>       Q0.0       Q0.1    alpha.0    alpha.1   logsigma   logsigma logsigma_e 
#>     1.5894     0.1643    -4.8140    -0.2691    -0.3292    -0.1239    -1.4600 
#>    rho_raw 
#>    -0.5997 
#> 
#> Use summary() for full results.
```

### Joint Tests

`anova(fit)` reports a joint Wald-χ² test for each parameter × factor
block (here, whether `gender` shifts Q_0 and \alpha). Pass additional
fits for a nested likelihood-ratio test.

``` r

anova(fit_gender)
#> # A tibble: 2 × 4
#>   Group          Chisq    df p.value
#>   <chr>          <dbl> <int>   <dbl>
#> 1 Q0 ~ gender     1.21     1   0.271
#> 2 alpha ~ gender  1.99     1   0.158
```

### Estimated Marginal Means

``` r

get_demand_param_emms(fit_gender, param = "Q0")
#> # A tibble: 2 × 6
#>   level         estimate estimate_log std.error conf.low conf.high
#>   <chr>            <dbl>        <dbl>     <dbl>    <dbl>     <dbl>
#> 1 gender=Female     4.90         1.59    0.0990     4.04      5.95
#> 2 gender=Male       5.78         1.75    0.112      4.64      7.19
```

``` r

get_demand_param_emms(fit_gender, param = "alpha")
#> # A tibble: 2 × 6
#>   level         estimate estimate_log std.error conf.low conf.high
#>   <chr>            <dbl>        <dbl>     <dbl>    <dbl>     <dbl>
#> 1 gender=Female  0.00812        -4.81     0.126  0.00634   0.0104 
#> 2 gender=Male    0.00620        -5.08     0.146  0.00466   0.00825
```

EMMs are reported on both the natural scale (`estimate`) and log scale
(`estimate_log`). The natural-scale estimates represent the
population-average Q_0 or \alpha for each group.

### Pairwise Comparisons

``` r

get_demand_comparisons(fit_gender, param = "Q0")
#> Demand Parameter Comparisons (tmb backend)
#> EMMs computed over: all fitted factors 
#> Contrast type: pairwise
#> P-value adjustment method: holm 
#> ================================================== 
#> 
#> Q0 (log10-scale contrasts):
#>       contrast estimate std.error conf.low conf.high p.value
#>  Female - Male   -0.071     0.065   -0.198     0.056   0.271
```

``` r

get_demand_comparisons(fit_gender, param = "alpha")
#> Demand Parameter Comparisons (tmb backend)
#> EMMs computed over: all fitted factors 
#> Contrast type: pairwise
#> P-value adjustment method: holm 
#> ================================================== 
#> 
#> alpha (log10-scale contrasts):
#>       contrast estimate std.error conf.low conf.high p.value
#>  Female - Male    0.117     0.083   -0.045     0.279   0.158
```

The `estimate_ratio` column gives the multiplicative ratio between
groups on the natural scale (e.g., a ratio of 1.34 means Group A’s Q_0
is 34% higher than Group B’s).

*Factor interaction and continuous covariate sections are skipped in
fast render mode. Set `BEEZDEMAND_VIGNETTE_MODE=full` to include them.*

## Model Comparison

Use
[`compare_models()`](https://brentkaplan.github.io/beezdemand/reference/compare_models.md)
to compare nested TMB models via likelihood ratio test, AIC, and BIC:

``` r

compare_models(fit_1re, fit_2re)
#> 
#> Model Comparison
#> ================================================== 
#> 
#>    Model          Class   Backend nobs df    logLik       AIC       BIC
#>  Model_1 beezdemand_tmb TMB_mixed 1131  4 -703.9366 1415.8732 1435.9967
#>  Model_2 beezdemand_tmb TMB_mixed 1131  6 -297.5496  607.0992  637.2843
#>  delta_AIC delta_BIC
#>   808.7741  798.7124
#>     0.0000    0.0000
#> 
#> Best model by BIC: Model_2 
#> 
#> Likelihood Ratio Tests:
#> ---------------------------------------- 
#>          Comparison  LR_stat df p_value
#>  Model_1 vs Model_2 812.7741  2  <2e-16
#> 
#> Notes:
#>   - LRT nesting assumption not verified.
```

Valid comparisons require models fit on the same data with the same
response scale. Equations that model different responses (e.g.,
exponential on log Q vs exponentiated on raw Q) cannot be compared via
AIC; equations that share a response scale (exponentiated and
simplified, both raw Q) can.

### Building Nested Models with `update()`

`update(fit, ...)` re-fits with named arguments substituted into the
original
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
call. Use it to build a candidate fit and drop or add factors /
covariates / random effects without typing the full fit call again:

``` r

# Drop the gender factor to test its joint significance:
fit_gender_null <- update(fit_gender, factors = NULL)
compare_models(fit_gender_null, fit_gender)
#> 
#> Model Comparison
#> ================================================== 
#> 
#>    Model          Class   Backend nobs df    logLik      AIC      BIC delta_AIC
#>  Model_1 beezdemand_tmb TMB_mixed 1131  6 -297.5496 607.0992 637.2843    0.0000
#>  Model_2 beezdemand_tmb TMB_mixed 1131  8 -296.4541 608.9083 649.1552    1.8091
#>  delta_BIC
#>     0.0000
#>    11.8708
#> 
#> Best model by BIC: Model_1 
#> 
#> Likelihood Ratio Tests:
#> ---------------------------------------- 
#>          Comparison LR_stat df p_value
#>  Model_1 vs Model_2  2.1909  2   0.334
#> 
#> Notes:
#>   - LRT nesting assumption not verified.
```

`evaluate = FALSE` returns the unevaluated call (for inspection) instead
of re-fitting, matching the convention of
[`stats::update.default`](https://rdrr.io/r/stats/update.html).

`formula(fit)` and `model.matrix(fit)` round out the introspection API.
`formula(fit)` returns a named list of one-sided formulas for `Q0` and
`alpha` plus the random-effect spec; `model.matrix(fit)` returns the
four design matrices the TMB template consumed (`X_q0`, `X_alpha`,
`Z_q0`, `Z_alpha`). This is a named list rather than the single matrix
`lm` or `lme4` return, because the TMB tier has two fixed-effect linear
predictors.

## Convergence Tips

If a model fails to converge, try these strategies in order:

| Strategy | How | When |
|----|----|----|
| Reduce random effects | `random_effects = "q0"` | 2-RE models struggling |
| Use exponential equation | `equation = "exponential"` | Exponentiated/simplified not converging |
| Fix k (the default) | `estimate_k = FALSE`, optionally with `k = <value>` | a free k drifting to extreme values |
| Increase iterations | `tmb_control = list(iter_max = 2000)` | “false convergence” messages |
| Try L-BFGS-B | `tmb_control = list(optimizer = "L-BFGS-B")` | nlminb not making progress |
| Set parameter bounds | `tmb_control = list(lower = ..., upper = ...)` | Estimates at boundaries |
| Warm start | `tmb_control = list(warm_start = prev_fit$opt$par)` | Refining a near-converged fit |

``` r

# Switch optimizer
fit <- fit_demand_tmb(
  dat, equation = "exponential",
  tmb_control = list(optimizer = "L-BFGS-B"),
  verbose = 0
)

# Warm-start from a previous fit
fit2 <- fit_demand_tmb(
  dat, equation = "exponential",
  tmb_control = list(warm_start = fit$opt$par),
  verbose = 0
)

# Apply parameter bounds (log_k is only a parameter when k is estimated)
fit3 <- fit_demand_tmb(
  dat, equation = "exponential", estimate_k = TRUE,
  tmb_control = list(
    lower = c(log_k = -2),
    upper = c(log_k = 4)
  ),
  verbose = 0
)

# Disable multi-start for faster iteration during exploration
fit4 <- fit_demand_tmb(
  dat, equation = "exponential",
  multi_start = FALSE,
  verbose = 2
)
```

### tmb_control Options

| Field             | Default    | Description                                   |
|-------------------|------------|-----------------------------------------------|
| `optimizer`       | `"nlminb"` | `"nlminb"` or `"L-BFGS-B"`                    |
| `iter_max`        | 1000       | Maximum iterations                            |
| `eval_max`        | 2000       | Maximum evaluations (nlminb only)             |
| `rel_tol`         | 1e-10      | Relative tolerance (nlminb only)              |
| `lower` / `upper` | NULL       | Named numeric bounds on log-scale parameters  |
| `warm_start`      | NULL       | Starting values from a previous `fit$opt$par` |
| `trace`           | 0          | Optimizer trace level                         |

## Choosing Between fit_demand_tmb() and fit_demand_mixed()

| Feature | [`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md) | [`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md) |
|----|----|----|
| Backend | TMB (C++, automatic differentiation) | nlme (R, numerical gradients) |
| Equations | exponential, exponentiated, simplified, zben | zben, simplified |
| k parameter | Fixed at 2 by default; optionally estimated | Not available |
| Random effects | 1 or 2 (Q0, alpha) | Configurable via nlme |
| Convergence | Reliable (AD + Laplace + multi-start) | Can struggle with nonlinear equations |
| Speed | Fast (compiled C++) | Variable |
| Post-hoc EMMs | [`get_demand_param_emms()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_emms.md) | [`get_demand_param_emms()`](https://brentkaplan.github.io/beezdemand/reference/get_demand_param_emms.md) (via emmeans) |
| Factors/covariates | Design matrices | Formula-based |

Prefer
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
for new work.
[`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
remains useful when you need specific `nlme` features like custom
correlation structures or when working with existing pipelines.

## References

Hursh, S. R., & Silberberg, A. (2008). Economic demand and essential
value. *Psychological Review*, 115(1), 186–198.

Koffarnus, M. N., Franck, C. T., Stein, J. S., & Bickel, W. K. (2015). A
modified exponential behavioral economic demand model to better describe
consumption data. *Experimental and Clinical Psychopharmacology*, 23(6),
504–512.

Kristensen, K., Nielsen, A., Berg, C. W., Skaug, H., & Bell, B. M.
(2016). TMB: Automatic differentiation and Laplace approximation.
*Journal of Statistical Software*, 70(5), 1–21.

## See Also

- [`vignette("fixed-demand")`](https://brentkaplan.github.io/beezdemand/articles/fixed-demand.md):
  Individual NLS demand curves
- [`vignette("mixed-demand")`](https://brentkaplan.github.io/beezdemand/articles/mixed-demand.md):
  NLME-based mixed-effects models
- [`vignette("mixed-demand-advanced")`](https://brentkaplan.github.io/beezdemand/articles/mixed-demand-advanced.md):
  Advanced topics: multi-factor designs, collapse_levels
- [`vignette("hurdle-demand-models")`](https://brentkaplan.github.io/beezdemand/articles/hurdle-demand-models.md):
  Two-part hurdle models for zero-heavy data
- [`vignette("model-selection")`](https://brentkaplan.github.io/beezdemand/articles/model-selection.md):
  Choosing the right demand model
