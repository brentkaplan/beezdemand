# Get Subject-Specific Parameters from TMB Model

Get Subject-Specific Parameters from TMB Model

## Usage

``` r
# S3 method for class 'beezdemand_tmb'
get_subject_pars(object, expanded = NULL, ...)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- expanded:

  Controls return shape for fits with within-id-varying design columns
  (factor-expanded random effects, within-id continuous covariates, or
  multi-block `pdBlocked` specs).

  - `NULL` (default): auto-detect. When fit-time within-id variation
    caused `NA` in cached `subject_pars$Q0`, runs the expansion
    machinery: rows are expanded across within-id factor levels (one row
    per (subject, factor-level) cell), and within-id numeric covariates
    are conditioned at the subject's mean (no row expansion from
    numerics). When the cached `Q0` has no `NA`, returns the wide
    one-row-per-subject shape unchanged.

  - `TRUE`: always attempt expansion. On a fit with no within-id
    variation, silently returns the wide shape.

  - `FALSE`: always return the wide shape. Emits a one-line warning on a
    fit with within-id variation (the returned `Q0`, `alpha`, `Pmax`,
    `Omax` are `NA`).

- ...:

  Additional arguments (currently unused).

## Value

When the resolved `expanded` is `FALSE`: data frame with columns `id`,
`b_i`, `c_i` (if 2 RE), `Q0`, `alpha`, `Pmax`, `Omax`. When the resolved
`expanded` is `TRUE`, the shape depends on the kind of within-id
variation: for fits with within-id factors, the within-subject factor
columns are added and rows are expanded to one per (subject,
factor-level) cell with per-cell `Q0`, `alpha`, `Pmax`, `Omax`; for fits
whose only within-id variation is numeric, the numerics are conditioned
at the subject's mean and the return is one row per subject (no added
factor columns) with finite `Q0` / `alpha`.

## Per-block random-effect matrices

For factor-expanded or multi-block fits, the wide table's `b_i` / `c_i`
columns hold the first RE column from each block (intercept slot for the
M1 baseline block, for example) for backward compatibility with
downstream consumers. Power users who need the full per-block RE
structure can access `attr(subject_pars, "re_q0_mat")` and
`attr(subject_pars, "re_alpha_mat")` as `n_subjects x re_dim` matrices
ordered by block.

## Examples

``` r
# \donttest{
data(apt)
fit <- fit_demand_tmb(apt, equation = "exponential", verbose = 0)
#>   equation='exponential': Dropped 14 zero-consumption observations (146 remaining).
head(get_subject_pars(fit))
#>    id        b_i        c_i        Q0       alpha      Pmax     Omax
#> 1  19  0.4347399 -0.6136355 10.058257 0.001637264 13.439089 44.13566
#> 2  30 -0.8308979  0.4420229  2.837025 0.004705279 16.579168 15.35759
#> 3  38 -0.3588356  0.1280378  4.548595 0.003437347 14.155026 21.02253
#> 4  60  0.3938846  0.1112281  9.655604 0.003380049  6.781236 21.37890
#> 5  68  0.4514144 -0.3174826 10.227378 0.002201589  9.829030 32.82253
#> 6 106 -0.1487057  0.4857013  5.612230 0.004915352  8.022711 14.70123
# }
```
