# Build display term names from TMB model coefficient names

Maps raw optimizer names (beta_q0, beta_alpha, etc.) to readable display
names (Q0:(Intercept), alpha:genderMale, etc.) using design matrix
column names. Used by summary(), tidy(), and confint() to avoid
duplicated logic.

## Usage

``` r
.tmb_build_term_names(object, nms = NULL)
```

## Arguments

- object:

  A `beezdemand_tmb` object.

- nms:

  Character vector of raw parameter names (from `names(coef(object))`).
  If NULL, extracted from object.

## Value

A list with components:

- term:

  Character vector of display names.

- q0_idx:

  Integer vector of beta_q0 positions.

- alpha_idx:

  Integer vector of beta_alpha positions.

- other_idx:

  Integer vector of non-beta positions.
