# Get Subject-Specific Parameters from an NLME Demand Model

Subject-level demand parameters for a `beezdemand_nlme` fit, matching
the column / scale / `expanded` contract of
[`get_subject_pars.beezdemand_tmb`](https://brentkaplan.github.io/beezdemand/reference/get_subject_pars.beezdemand_tmb.md).
Combines the population fixed effects with each subject's random-effect
deviations and back-transforms to the natural scale.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
get_subject_pars(object, expanded = NULL, ...)
```

## Arguments

- object:

  A `beezdemand_nlme` object.

- expanded:

  Controls the return shape for fits with within-id-varying design
  columns (within-subject factors, within-id covariates, or multi-block
  `pdBlocked` specs).

  - `NULL` (default): auto-detect. Expands to one row per (subject,
    factor-level) cell when within-id variation is present; otherwise
    returns the wide one-row-per-subject shape.

  - `TRUE`: always attempt expansion (no-op when there is no within-id
    variation).

  - `FALSE`: always return the wide shape; emits a one-line warning when
    within-id variation is present (the affected subjects' `Q0`,
    `alpha`, `Pmax`, `Omax` are `NA`).

- ...:

  Currently unused.

## Value

A data frame. Wide form: `id`, `b_i`, `c_i` (if alpha has random
effects), `Q0`, `alpha`, `Pmax`, `Omax`. Expanded form additionally
includes the within-subject factor column(s) with one row per (subject,
factor-level) cell. `Q0`, `alpha`, `Pmax`, `Omax` are on the natural
scale.

## Random-effect aliases (`b_i` / `c_i`)

`b_i` / `c_i` are the subject's first-block random-effect deviation for
Q0 / alpha. For parity with the TMB method these are reported on the
natural-log linear-predictor scale: for the default
`param_space = "log10"` the stored log10 deviation is multiplied by
`log(10)`; for `param_space = "natural"` the deviation is returned on
the natural parameter scale. The full per-coefficient random effects
remain available via `ranef()`.

## See also

[`get_subject_pars.beezdemand_tmb`](https://brentkaplan.github.io/beezdemand/reference/get_subject_pars.beezdemand_tmb.md)
