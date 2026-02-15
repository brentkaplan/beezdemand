# Fit cross-price demand with NLS (+ robust fallbacks)

Fits a \*\*cross-price demand\*\* curve using log10-parameterization for
numerical stability. The optimizer estimates parameters on the log10
scale where applicable, ensuring positive constraints are naturally
satisfied.

\*\*Equation forms:\*\*

\- \*\*Exponentiated\*\* (default): \$\$y = Q\_{alone} \cdot 10^{I \cdot
\exp(-\beta \cdot x)}\$\$

\- \*\*Exponential\*\* (fits on log10 response scale): \$\$\log\_{10}(y)
= \log\_{10}(Q\_{alone}) + I \cdot \exp(-\beta \cdot x)\$\$

\- \*\*Additive\*\* (level on \\y\\): \$\$y = Q\_{alone} + I \cdot
\exp(-\beta \cdot x)\$\$

where \\x\\ is the alternative product price (or "cross" price) and
\\y\\ is consumption of the target good.

\*\*Optimizer parameters (log10 parameterization):\*\*

- `log10_qalone`: \\\log\_{10}(Q\_{alone})\\ - baseline consumption when
  the alternative is effectively absent.

- `I`: cross-price \*\*interaction intensity\*\*; sign and magnitude
  reflect substitution/complementarity. Unconstrained (can be negative
  for substitutes).

- `log10_beta`: \\\log\_{10}(\beta)\\ - rate at which cross-price
  influence decays as \\x\\ increases.

Natural-scale values are recovered as \\Q\_{alone} =
10^{log10\\qalone}\\ and \\\beta = 10^{log10\\beta}\\.

The function first attempts a multi-start nonlinear least squares fit
(\`nls.multstart\`). If that fails—or if explicit \`start_vals\` are
provided—it falls back to \`minpack.lm::nlsLM\`. Optionally, it will
make a final attempt with \`nlsr::wrapnlsr\`. Returns either the fitted
model or a structured object with metadata for downstream methods.

## Usage

``` r
fit_cp_nls(
  data,
  equation = c("exponentiated", "exponential", "additive"),
  start_vals = NULL,
  iter = 100,
  bounds = NULL,
  fallback_to_nlsr = TRUE,
  return_all = TRUE
)
```

## Arguments

- data:

  A data frame with columns \`x\` (alternative price) and \`y\`
  (consumption). Additional columns are ignored. Input is validated
  internally.

- equation:

  Character string; model family, one of \`c("exponentiated",
  "exponential", "additive")\`. Default is \`"exponentiated"\`.

- start_vals:

  Optional \*\*named list\*\* of initial values for parameters
  \`log10_qalone\`, \`I\`, and \`log10_beta\`. If \`NULL\`, the function
  derives plausible ranges from the data and uses multi-start search.

- iter:

  Integer; number of random starts for \`nls.multstart\` (default
  \`100\`).

- bounds:

  Deprecated. Log10-parameterized parameters are naturally unbounded.
  This argument is ignored but retained for backwards compatibility.

- fallback_to_nlsr:

  Logical; if \`TRUE\` (default), try \`nlsr::wrapnlsr\` when both
  multi-start NLS and \`nlsLM\` fail.

- return_all:

  Logical; if \`TRUE\` (default), return a list containing the model and
  useful metadata. If \`FALSE\`, return the bare fitted model object.

## Value

If \`return_all = TRUE\` (default): a list of class \`"cp_model_nls"\`:

- \`model\`: the fitted object from the successful backend.

- \`method\`: one of \`"nls_multstart"\`, \`"nlsLM"\`, or
  \`"wrapnlsr"\`.

- \`equation\`: the model family used.

- \`start_vals\`: named list of starting values (final used).

- \`nlsLM_fit\`, \`nlsr_fit\`: fits from later stages (if attempted).

- \`data\`: the 2-column data frame actually fit.

If \`return_all = FALSE\`: the fitted model object from the successful
backend.

## Details

\*\*Start values.\*\* When \`start_vals\` is missing, the function: (1)
estimates a reasonable range for \`log10_qalone\` from the observed
\`y\`, (2) estimates \`log10_beta\` from the price range, and (3)
launches a multi-start grid in \`nls.multstart\`.

\*\*Zero handling for exponential equation.\*\* Since the exponential
equation fits on the \\\log\_{10}(y)\\ scale, observations with \\y \le
0\\ are automatically removed with a warning. Use the exponentiated or
additive forms if you need to retain zero consumption values.

\*\*Fitting pipeline (short-circuiting):\*\*

1.  \`nls.multstart::nls_multstart()\` with random starts.

2.  If that fails (or if \`start_vals\` provided):
    \`minpack.lm::nlsLM()\` using \`start_vals\` (user or internally
    estimated).

3.  If that fails and \`fallback_to_nlsr = TRUE\`: \`nlsr::wrapnlsr()\`.

The returned object has class \`"cp_model_nls"\` (when \`return_all =
TRUE\`) with components: \`model\`, \`method\` (the algorithm used),
\`equation\`, \`start_vals\`, \`nlsLM_fit\`, \`nlsr_fit\`, and the
\`data\` used. This is convenient for custom print/summary/plot methods.

## Convergence & warnings

\- Check convergence codes and residual diagnostics from the underlying
fit. - Poor scaling or extreme \`y\` dispersion can make parameters
weakly identified. - For \`"exponential"\`, the model fits on the
\\\log\_{10}(y)\\ scale internally.

## See also

[`check_unsystematic_cp`](https://brentkaplan.github.io/beezdemand/reference/check_unsystematic_cp.md)
for pre-fit data screening,
[`validate_cp_data`](https://brentkaplan.github.io/beezdemand/reference/validate_cp_data.md)
for input validation.

## Examples

``` r
## --- Example: Real data (E-Cigarettes, id = 1) ---
dat <- structure(list(
  id = c(1, 1, 1, 1, 1, 1),
  x = c(2, 4, 8, 16, 32, 64),
  y = c(3, 5, 5, 16, 17, 13),
  target = c("alt", "alt", "alt", "alt", "alt", "alt"),
  group = c("E-Cigarettes", "E-Cigarettes", "E-Cigarettes",
            "E-Cigarettes", "E-Cigarettes", "E-Cigarettes")
), row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame"))

## Fit the default (exponentiated) cross-price form
fit_ecig <- fit_cp_nls(dat, equation = "exponentiated", return_all = TRUE)
summary(fit_ecig)         # model summary
#> Cross-Price Demand Model Summary
#> ================================
#> 
#> Model Specification:
#> Equation type: exponentiated 
#> Functional form: y ~ (10^log10_qalone) * 10^(I * exp(-(10^log10_beta) * x)) 
#> Fitting method: nls_multstart 
#> Method details: Multiple starting values optimization with nls.multstart 
#> 
#> Coefficients:
#>               Estimate Std. Error t value  Pr(>|t|)    
#> log10_qalone  1.194135   0.060311 19.7995 0.0002815 ***
#> I            -1.268376   0.837302 -1.5148 0.2270524    
#> log10_beta   -0.737728   0.265129 -2.7825 0.0688459 .  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Confidence Intervals:
#>               2.5 % 97.5 %
#> log10_qalone  1.002  1.386
#> I            -3.933  1.396
#> log10_beta   -1.581  0.106
#> 
#> Fit Statistics:
#> R-squared: 0.8597 
#> AIC: 34.06 
#> BIC: 33.23 
#> 
#> Parameter Interpretation (natural scale):
#> qalone (Q_alone): 15.64  - consumption at zero alternative price
#> I: -1.268  - interaction parameter (substitution direction)
#> beta: 0.1829  - sensitivity parameter (sensitivity of relation to price)
#> 
#> Optimizer parameters (log10 scale):
#> log10_qalone: 1.194 
#> log10_beta: -0.7377 
fit_ecig$method           # backend actually used (e.g., "nls_multstart")
#> [1] "nls_multstart"
coef(fit_ecig$model)      # parameter estimates: log10_qalone, I, log10_beta
#> log10_qalone            I   log10_beta 
#>    1.1941354   -1.2683758   -0.7377282 
```
