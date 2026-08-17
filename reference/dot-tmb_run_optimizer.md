# Run a Single TMB Optimization

Dispatches to either `nlminb` or `optim(method = "L-BFGS-B")` and
normalizes the return value so that downstream code always sees the same
field names regardless of optimizer.

## Usage

``` r
.tmb_run_optimizer(obj, start, tmb_control, user_specified, verbose)
```

## Arguments

- obj:

  TMB objective function object (from
  [`TMB::MakeADFun`](https://rdrr.io/pkg/TMB/man/MakeADFun.html)).

- start:

  Named numeric vector of starting parameter values.

- tmb_control:

  Control list (merged defaults + user overrides).

- user_specified:

  Character vector of field names the user explicitly provided in
  `tmb_control`.

- verbose:

  Integer verbosity level.

## Value

A list with elements:

- `opt`:

  Named list with `$par`, `$objective`, `$convergence`, `$message` –
  guaranteed non-NULL character for `$message`.

- `warnings`:

  Character vector of optimizer warnings captured during the run.
