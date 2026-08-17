# Per-RE-column starting log-SD values, scaled for continuous slopes

Returns the `logsigma` starting vector in canonical block order (per
block: Q0 columns then alpha columns). Intercept and factor-dummy
columns keep the flat `base` start (byte-identical to the historical
`rep(log(0.5), n)`), while a *numeric* continuous-covariate slope column
is started at `base - log(spread)` so the slope's contribution
`w_i * x_c` is on the same scale as an intercept deviation – a slope SD
lives on (covariate)^-1 units. New behavior engages only when a numeric
RE-RHS term is present, so factor and intercept-only fits are unaffected
(TICKET-051).

## Usage

``` r
.tmb_re_logsigma_starts(re_parsed, data, base = log(0.5))
```

## Arguments

- re_parsed:

  Output of `.normalize_re_input()`.

- data:

  Long-format fit data, or NULL (-\> all columns at `base`).

- base:

  Numeric; flat starting log-SD (default `log(0.5)`).

## Value

Numeric vector of length `n_logsigma`.
