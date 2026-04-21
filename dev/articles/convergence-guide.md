# Understanding Convergence in Demand Models

## Introduction

If you’ve fitted demand models with `beezdemand`, you’ve likely
encountered messages like “convergence code 1”, “false convergence”, or
a non-positive-definite `apVar`. These can be alarming, especially if
you’re accustomed to linear models where convergence is virtually
guaranteed. This vignette explains what these messages actually mean and
provides a systematic approach for evaluating whether your model results
are trustworthy.

Two myths persist in applied behavioral economics:

1.  **“Convergence = good model.”** A model can converge to a local
    minimum, a boundary solution, or nonsensical parameter estimates.
    Convergence status tells you the optimizer stopped happily, not that
    it stopped at the right place.

2.  **“Non-convergence = useless results.”** A model that hits an
    iteration limit or triggers a “false convergence” warning may have
    parameter estimates that are practically identical to those from a
    fully converged run. The optimizer’s stopping rule fired before it
    was satisfied, but that doesn’t mean the estimates are wrong.

This vignette covers convergence across all model tiers in `beezdemand`:

- **TMB models**
  ([`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md),
  [`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md))
  report numeric codes
- **NLME models**
  ([`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md))
  report warning messages
- **Fixed-effect NLS models**
  ([`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md))
  either converge or fail outright

The diagnostic principles are the same across all tiers. By the end
you’ll have a six-item checklist for evaluating any fitted demand model
regardless of convergence status.

## What Is Convergence?

### Optimization in Nonlinear Models

Unlike linear regression, which has a closed-form solution, nonlinear
demand models must be fitted iteratively. The optimizer starts from
initial parameter values and takes steps through parameter space, trying
to minimize an objective function:

- **TMB models**: minimize negative log-likelihood (NLL)
- **NLME models**: minimize restricted/maximum likelihood (REML/ML)
- **NLS models**: minimize residual sum of squares (RSS)

At each iteration, the optimizer evaluates the objective function,
computes gradients (the direction of steepest descent), and takes a
step. It stops when one of several **stopping rules** fires:

1.  **Gradient near zero**: the objective function is flat, suggesting a
    minimum
2.  **Parameter change below tolerance**: successive iterations produce
    nearly identical estimates
3.  **Iteration limit reached**: the budget of iterations is exhausted
4.  **Step size collapses**: the optimizer can’t make progress

The convergence status you see in model output tells you **which
stopping rule fired**. It does not tell you whether the solution is the
global minimum, whether the parameters are scientifically plausible, or
whether the model fits the data well.

### TMB Convergence Codes (nlminb)

TMB models fitted with
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
and
[`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)
use the `nlminb` optimizer by default. It reports the following
convergence codes:

| Code | What Triggered It                                                     | What It Means                                                                                                                                                                                  |
|:----:|-----------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|  0   | Relative improvement in objective \< `rel.tol`                        | The optimizer is satisfied that it found a minimum. This is the “normal” exit.                                                                                                                 |
|  1   | Number of iterations \>= `iter.max`                                   | The optimizer ran out of its iteration budget before satisfying its convergence criterion. The estimates may be fine — it just wasn’t given enough iterations to confirm.                      |
|  8   | Step size shrinks to zero but tolerance not met (“false convergence”) | The optimizer cannot make further progress but hasn’t confirmed it reached a minimum. Often occurs when the solution is near a parameter boundary or when the likelihood surface is very flat. |

**Code 0** is what you want but doesn’t guarantee the solution is
correct — the optimizer may have converged to a local minimum or a
boundary.

**Code 1** (iteration limit) is often benign. If the objective function
was still decreasing, increasing `iter_max` in `tmb_control` may resolve
it. If the objective had plateaued, the current estimates are likely at
or very near the minimum.

**Code 8** (false convergence) sounds alarming but is common in
well-specified demand models. TMB provides exact gradients via automatic
differentiation, so code 8 is rarer than with finite-difference methods.
When it does occur, it typically means the solution is at a feasibility
boundary (e.g., a random effect variance near zero) rather than that the
optimizer is stuck.

#### L-BFGS-B Convergence Codes

When using `optimizer = "L-BFGS-B"` in `tmb_control`,
[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
uses [`stats::optim()`](https://rdrr.io/r/stats/optim.html) with
L-BFGS-B method. It reports different codes:

| Code | What Triggered It                      |
|:----:|----------------------------------------|
|  0   | Successful convergence                 |
|  1   | Iteration limit reached (`maxit`)      |
|  51  | Warning from L-BFGS-B (see `$message`) |
|  52  | Error in L-BFGS-B                      |

### NLME Convergence Warnings

NLME models fitted with
[`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)
use a fundamentally different optimization architecture than TMB. Rather
than a single optimizer minimizing a single objective, NLME alternates
between two procedures:

1.  **EM algorithm**: estimates variance components (random effect
    variances and correlations)
2.  **PNLS (penalized nonlinear least squares)**: estimates fixed
    effects and predicts random effects, conditional on the variance
    components

These two steps alternate until the overall likelihood converges. After
convergence, the **approximate variance-covariance matrix** (`apVar`) is
computed from the Hessian at the solution to provide standard errors for
the variance components.

NLME signals problems through R warnings rather than numeric codes. The
package captures these during fitting and stores them in the
`fit_warnings` field of the returned object. The following table
summarizes the most common warnings:

| Warning Pattern                               | What It Means                                            | Severity |
|-----------------------------------------------|----------------------------------------------------------|----------|
| `"false convergence"`                         | Step size shrank below minimum before gradient converged | Moderate |
| `"step halving factor reduced below minimum"` | PNLS step-halving hit its floor                          | Moderate |
| `"maximum number of iterations reached"`      | Hit `msMaxIter` or `maxIter` limit                       | Low      |
| `"iteration limit reached"`                   | Same as above                                            | Low      |
| `"singular"`                                  | Design or correlation matrix is rank-deficient           | High     |
| Non-PD `apVar`                                | Hessian not positive definite at solution                | High     |

#### Understanding Severity

**Low severity** (iteration limits): The model may just need more
iterations. Try increasing `msMaxIter` or `maxIter` in `nlme_control`.
If the same estimates are obtained with more iterations, the original
solution was fine.

**Moderate severity** (“false convergence”, “step halving”): These mean
the optimizer had difficulty making progress. The estimates may be fine
— especially if the parameters are plausible and the random effect
variances are not near zero. These warnings are analogous to TMB’s code
8.

**High severity** (“singular”, non-PD `apVar`): These suggest structural
problems. A singular correlation matrix means two random effects are
almost perfectly correlated, indicating overparameterization. A
non-positive-definite `apVar` means the Hessian at the solution is not
well-behaved, so standard errors for variance components are unreliable.
However, the point estimates (fixed effects and random effect
predictions) may still be fine.

#### The apVar Issue

The `apVar` failure deserves special attention because it is common and
often misunderstood. You can check for it programmatically:

``` r
# After fitting an NLME model
fit <- fit_demand_mixed(data, y_var = "y_ll4", equation_form = "zben")

# apVar is a matrix when successful, a character string describing the error
# when it fails
is_apvar_ok <- is.matrix(fit$model$apVar)
```

When `apVar` fails:

- **Fixed-effect estimates** (`fixef(fit$model)`) are still valid — they
  don’t depend on `apVar`
- **Random-effect predictions** (`ranef(fit$model)`) are still valid —
  they are empirical Bayes predictions conditional on the fixed effects
- **Variance component standard errors** are not available — you cannot
  construct confidence intervals for the random effect variances
- **`intervals(fit$model)`** will fail for the random effects portion

This is analogous to TMB’s `sdreport` failure: point estimates are fine
but uncertainty quantification for variance parameters is compromised.

### NLS Convergence (Fixed-Effect Models)

[`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
handles convergence automatically through a multi-backend fallback
strategy. For each subject, the package tries:

1.  `nls.multstart()` with random starting values
2.  If that fails,
    [`minpack.lm::nlsLM()`](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html)
    (Levenberg-Marquardt)
3.  If that fails,
    [`nlsr::wrapnlsr()`](https://rdrr.io/pkg/nlsr/man/wrapnlsr.html)

Models either converge or fail outright; partial convergence doesn’t
occur. The returned object tracks per-subject success/failure in the
`Notes` column. Use
[`check_demand_model()`](https://brentkaplan.github.io/beezdemand/reference/check_demand_model.md)
to see the failure rate:

``` r
fit <- fit_demand_fixed(apt, equation = "hs")
diagnostics <- check_demand_model(fit)
print(diagnostics)
```

A high failure rate (\>20%) suggests data quality problems —
insufficient price range, too many zero-consumption observations, or
extreme outliers — rather than modeling issues.

### The Key Insight

Convergence status reflects the optimizer’s stopping rule, not the
quality of the solution. A non-converged fit with plausible parameters
and valid standard errors can be more trustworthy than a converged fit
at a boundary or local minimum. This principle applies equally to TMB
numeric codes and NLME warning messages.

The next section provides a systematic checklist for evaluating fit
quality regardless of convergence status.

## Diagnostic Checklist

Use these six checks to evaluate any fitted demand model. Each check
includes parallel examples for TMB and NLME models.

### 1. Parameter Plausibility

The most important check is whether the estimated parameters make
scientific sense. For demand models: - **Q0** (intensity) should be
positive and in the range of observed consumption at low prices -
**alpha** (elasticity) should be positive and not extreme - **k**
(scaling constant) typically falls between 1 and 4 for purchase task
data

#### TMB Models

``` r
fit_tmb <- fit_demand_tmb(data, y_var = "y", x_var = "x", id_var = "id")

# Population-level (fixed) parameters
coef(fit_tmb)

# Full summary with standard errors
summary(fit_tmb)

# Subject-level parameters (fixed + random effects)
subject_pars <- get_subject_pars(fit_tmb)
head(subject_pars)

# Check ranges
summary(subject_pars$Q0)
summary(subject_pars$alpha)
```

Look for:

- Negative Q0 or alpha values (biologically implausible)
- Extreme values (Q0 orders of magnitude larger than max observed
  consumption)
- Subject-level estimates that cluster at boundaries

#### NLME Models

``` r
fit_nlme <- fit_demand_mixed(data, y_var = "y_ll4", equation_form = "zben")

# Population-level fixed effects
fixef(fit_nlme$model)

# Subject-level deviations from population mean
ranef(fit_nlme$model)

# Subject-level parameters (fixed + random)
subject_pars <- get_subject_pars(fit_nlme)
head(subject_pars)
```

For NLME models, pay attention to whether the random effects are
symmetric and roughly normally distributed. Highly skewed or multimodal
random effects suggest model misspecification or influential subjects.

#### Hurdle Models

``` r
fit_hurdle <- fit_demand_hurdle(data, y_var = "y", x_var = "x", id_var = "id")

# Population parameters
coef(fit_hurdle)

# Subject-level parameters
subject_pars <- get_subject_pars(fit_hurdle)
head(subject_pars)
```

### 2. Standard Errors and Variance Estimates

Finite, reasonable standard errors indicate that the likelihood surface
is well-behaved near the solution.

#### TMB Models

``` r
# Check if standard errors were successfully computed
fit_tmb$se_available

# Hessian positive-definiteness gate (since 0.3.0): TRUE means the inverse
# Hessian is well-conditioned, so SEs / p-values / Wald CIs are trustworthy.
# FALSE means they are not — and `fit_demand_tmb()` / `fit_demand_hurdle()`
# also emit a cli warning at fit time when verbose >= 1.
fit_tmb$hessian_pd

# View SEs in summary output (notes section flags pdHess = FALSE)
summary(fit_tmb)
```

Warning signs:

- `hessian_pd = FALSE`: standard errors and p-values may be unreliable
  (saddle point, near-singular Hessian, or boundary solution)
- `se_available = FALSE`:
  [`TMB::sdreport()`](https://rdrr.io/pkg/TMB/man/sdreport.html) failed
  entirely; SEs are missing
- Very large SEs (orders of magnitude larger than the estimates): flat
  likelihood
- `NA` or `NaN` SEs: numerical problems in the Hessian computation

#### NLME Models

``` r
# Check apVar status
is_apvar_ok <- is.matrix(fit_nlme$model$apVar)
cat("apVar OK:", is_apvar_ok, "\n")

# If apVar is OK, get confidence intervals on variance components
if (is_apvar_ok) {
  intervals(fit_nlme$model, which = "var-cov")
}

# Fixed effect standard errors are always available (from the PNLS step)
summary(fit_nlme$model)$tTable
```

Warning signs:

- `apVar` is a character string (not a matrix): variance component SEs
  unavailable
- Near-zero random effect variances: the random effect is not
  contributing to the model; consider removing it
- Very wide confidence intervals on variance components: model may be
  overparameterized

### 3. Gradient Norm (TMB-Specific)

For TMB models, you can check the gradient at the solution directly. A
small gradient confirms the optimizer stopped at (or very near) a
stationary point.

``` r
# Compute gradient at the solution
grad <- fit_tmb$tmb_obj$gr(fit_tmb$opt$par)
max_grad <- max(abs(grad))
cat("Maximum absolute gradient:", max_grad, "\n")

# Rule of thumb:
# < 0.001: excellent — optimizer found a clean minimum
# 0.001-0.01: good — likely near a minimum
# > 0.01: concerning — may not be at a minimum
```

NLME does not expose gradients directly, so this check is TMB-only. For
NLME models, rely on the other five checks.

### 4. Hessian / apVar Status

The Hessian matrix (second derivatives of the objective function) tells
you about the curvature at the solution. A positive-definite Hessian
confirms the solution is at a local minimum (not a saddle point or
maximum).

#### TMB Models

``` r
# Preferred (since 0.3.0): the dedicated `hessian_pd` accessor on the fit
# object. TRUE = positive definite; FALSE = not; NA = sdreport failed.
fit_tmb$hessian_pd

# Equivalent low-level path through the sdreport object:
if (!is.null(fit_tmb$sdr)) {
  isTRUE(fit_tmb$sdr$pdHess)
}
```

If `hessian_pd` is `FALSE`, the solution may be at a saddle point or on
the boundary of the feasible region. The fitting function emits a cli
warning at fit time when `verbose >= 1`, and
[`summary()`](https://rdrr.io/r/base/summary.html) adds a corresponding
note. [`tidy()`](https://generics.r-lib.org/reference/tidy.html) returns
its data unchanged but tags the result with a `hessian_warning`
attribute for downstream pipelines:

``` r
out <- tidy(fit_tmb)
attr(out, "hessian_warning")
```

When triggered, try refitting with different starting values, switching
optimizers (`tmb_control$optimizer = "L-BFGS-B"`), or warm-starting from
the flagged fit (`tmb_control$warm_start = fit_tmb$opt$par`). The same
`hessian_pd` field exists on `beezdemand_hurdle` objects.

#### NLME Models

``` r
# Check apVar (NLME's version of the Hessian check)
cat("apVar OK:", is.matrix(fit_nlme$model$apVar), "\n")

# Check random effects correlation for near-singularity
vc <- nlme::VarCorr(fit_nlme$model)
print(vc)
# Look at the Corr column — values > |0.99| indicate near-singularity
```

For NLME models, also check the random effects correlation matrix. If
two random effects are correlated above \|0.99\|, they are nearly
collinear — the model is trying to estimate two separate sources of
variation that the data cannot distinguish. Consider switching to
`pdDiag` covariance structure (which forces zero correlation) or
removing one of the random effects.

### 5. Comparison with Nested Converged Models

If a non-converged complex model beats a simpler converged model by a
large AIC margin, the parameters are in a good region of the likelihood
surface. AIC and BIC depend on the log-likelihood at the parameter
values, not on convergence status.

#### TMB Models

``` r
# Fit a simpler model (e.g., fewer random effects)
fit_simple <- fit_demand_tmb(data, y_var = "y", x_var = "x", id_var = "id",
                             random_effects = "q0")
fit_complex <- fit_demand_tmb(data, y_var = "y", x_var = "x", id_var = "id",
                              random_effects = c("q0", "alpha"))

# Compare
AIC(fit_simple, fit_complex)
```

#### NLME Models

``` r
# Fit with random effect on Q0 only vs. Q0 + alpha
fit_q0 <- fit_demand_mixed(data, y_var = "y_ll4", equation_form = "zben",
                           random_effects = Q0 ~ 1)
fit_both <- fit_demand_mixed(data, y_var = "y_ll4", equation_form = "zben",
                             random_effects = Q0 + alpha ~ 1)

# Compare AIC
AIC(fit_q0$model, fit_both$model)

# Or use anova for a formal likelihood ratio test
anova(fit_q0$model, fit_both$model)
```

### 6. Stability Across Starting Values

If different starting values lead to the same (or very similar)
parameter estimates, you can be confident the solution is not a local
minimum.

#### TMB Models

[`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
performs multi-start optimization automatically. You can verify
stability by checking whether different starting configurations yield
the same negative log-likelihood:

``` r
# Fit with default multi-start
fit1 <- fit_demand_tmb(data, y_var = "y", x_var = "x", id_var = "id")

# Fit with warm start from the first fit
fit2 <- fit_demand_tmb(data, y_var = "y", x_var = "x", id_var = "id",
                       tmb_control = list(warm_start = fit1$opt$par))

# Compare NLL values
cat("Fit 1 NLL:", fit1$opt$objective, "\n")
cat("Fit 2 NLL:", fit2$opt$objective, "\n")
```

#### NLME Models

``` r
# Fit with pooled NLS starting values (data-driven)
fit_pooled <- fit_demand_mixed(data, y_var = "y_ll4", equation_form = "zben",
                               start_values = "pooled_nls")

# Fit with different manual starting values
fit_manual <- fit_demand_mixed(data, y_var = "y_ll4", equation_form = "zben",
                               start_values = list(
                                 fixed = c(Q0 = 5, alpha = 0.001)
                               ))

# Compare fixed effects
fixef(fit_pooled$model)
fixef(fit_manual$model)
```

If both runs yield similar fixed-effect estimates (within a few
percent), the solution is stable.

## Decision Framework

### TMB Decision Table

Use this table after running the diagnostic checklist on a TMB model
([`fit_demand_tmb()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_tmb.md)
or
[`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)):

|  Code  | Gradient Small? | Hessian PD? | SEs Finite? | Params Plausible? | Verdict                                                                  |
|:------:|:---------------:|:-----------:|:-----------:|:-----------------:|--------------------------------------------------------------------------|
|   0    |       Yes       |     Yes     |     Yes     |        Yes        | **Trust the results**                                                    |
|   0    |       Yes       |     No      |      —      |         —         | Saddle point: refit with different starts                                |
|   0    |       Yes       |     Yes     |     Yes     |        No         | Converged to wrong minimum: check data, try different starts             |
| 1 or 8 |       Yes       |     Yes     |     Yes     |        Yes        | **Likely trustworthy** — run diagnostics, increase iterations to confirm |
| 1 or 8 |       No        |     No      |      —      |         —         | Simplify model (fewer random effects, fix k)                             |
| 1 or 8 |        —        |      —      |      —      |        No         | Estimates unreliable: simplify or check data quality                     |

### NLME Decision Table

Use this table after running the diagnostic checklist on an NLME model
([`fit_demand_mixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_mixed.md)):

| Warning             | apVar OK? | RE Corr \< \|0.99\|? | RE Var \> 0? | Params Plausible? | Verdict                                                 |
|---------------------|:---------:|:--------------------:|:------------:|:-----------------:|---------------------------------------------------------|
| None                |    Yes    |         Yes          |     Yes      |        Yes        | **Trust the results**                                   |
| “false convergence” |    Yes    |         Yes          |     Yes      |        Yes        | **Likely trustworthy** — consider increasing iterations |
| “false convergence” |    No     |          —           |      —       |         —         | SEs unreliable: consider simplifying RE structure       |
| “max iterations”    |     —     |          —           |      —       |        Yes        | Increase `msMaxIter`/`maxIter` and refit                |
| “singular”          |     —     |          No          |      —       |         —         | Overparameterized: reduce RE structure                  |
| Any                 |     —     |          —           |  Near zero   |         —         | Remove that random effect                               |
| Any                 |     —     |          —           |      —       |        No         | Estimates unreliable: check data, simplify model        |

## Remedies by Model Tier

### NLME Remedies

NLME convergence issues typically fall into a few categories. The table
below maps symptoms to solutions:

| Problem              | Symptom                                       | Remedy                                      |
|----------------------|-----------------------------------------------|---------------------------------------------|
| Overparameterized RE | Near-singular correlation, RE variance near 0 | Remove random effect on alpha; keep only Q0 |
| Slow convergence     | “maximum number of iterations reached”        | Increase `msMaxIter` and `maxIter`          |
| False convergence    | “false convergence” warning                   | Loosen `pnlsTol`, increase `niterEM`        |
| Non-PD apVar         | `is.character(model$apVar)`                   | Switch to `pdDiag` covariance structure     |
| Bad starting values  | Immediate failure or wild estimates           | Use pooled NLS starts or manually specify   |
| LL4 transform issues | Extreme residuals, poor fit                   | Check lambda parameter; verify y_ll4 values |

#### Increasing Iterations

The simplest remedy for iteration-limit warnings. The `nlme_control`
argument accepts a named list of control parameters:

``` r
fit <- fit_demand_mixed(
  data,
  y_var = "y_ll4",
  equation_form = "zben",
  nlme_control = list(
    msMaxIter = 500,  # Max iterations for the outer optimization (default: 200)
    maxIter = 500,    # Max iterations for the PNLS step (default: 200)
    niterEM = 200     # Number of EM iterations before switching to Newton (default: 100)
  )
)
```

#### Loosening Tolerance for False Convergence

When the PNLS step reports “false convergence”, it means the step size
shrank below the minimum tolerance before the gradient converged.
Loosening the PNLS tolerance allows the optimizer more room to maneuver:

``` r
fit <- fit_demand_mixed(
  data,
  y_var = "y_ll4",
  equation_form = "zben",
  nlme_control = list(
    pnlsTol = 1e-2,     # Loosen PNLS tolerance (default: 1e-3)
    tolerance = 1e-5,    # Slightly loosen overall tolerance (default: 1e-6)
    niterEM = 150        # More EM iterations can help with difficult surfaces
  )
)
```

#### Switching Covariance Structure

If the random effects correlation is near \|1.0\| or `apVar` fails,
switching to a diagonal covariance structure (`pdDiag`) removes the
correlation parameter entirely. This reduces the number of parameters
and often resolves convergence issues:

``` r
# Default uses pdDiag (diagonal); try pdSymm for unstructured if you have
# enough data, or stay with pdDiag if convergence is problematic
fit_diag <- fit_demand_mixed(
  data,
  y_var = "y_ll4",
  equation_form = "zben",
  covariance_structure = "pdDiag"  # No correlation between random effects
)

# Compare with unstructured (pdSymm) if convergence allows
fit_symm <- fit_demand_mixed(
  data,
  y_var = "y_ll4",
  equation_form = "zben",
  covariance_structure = "pdSymm"  # Estimate correlation between random effects
)
```

#### Simplifying Random Effects

If a random effect variance is near zero, the data does not support
subject-level variation in that parameter. Removing it simplifies the
model and often resolves convergence problems:

``` r
# Random effects on both Q0 and alpha (more complex)
fit_both <- fit_demand_mixed(
  data,
  y_var = "y_ll4",
  equation_form = "zben",
  random_effects = Q0 + alpha ~ 1
)

# Random effect on Q0 only (simpler, often sufficient)
fit_q0_only <- fit_demand_mixed(
  data,
  y_var = "y_ll4",
  equation_form = "zben",
  random_effects = Q0 ~ 1
)

# Compare fit
AIC(fit_both$model, fit_q0_only$model)
```

### TMB Remedies

TMB convergence issues have their own set of solutions:

| Problem           | Symptom                            | Remedy                                   |
|-------------------|------------------------------------|------------------------------------------|
| Local minimum     | Multiple starts give different NLL | Increase multi-start sets                |
| Flat likelihood   | Large SEs, code 1                  | Simplify model (fewer RE, fix k)         |
| Boundary solution | Parameter at bound, code 1         | Widen or narrow bounds                   |
| Iteration limit   | Code 1                             | Increase `iter_max`                      |
| False convergence | Code 8                             | Try more iterations; try L-BFGS-B        |
| Saddle point      | Hessian not PD                     | Different starts; try L-BFGS-B optimizer |
| Zero RE variance  | sigma near 0                       | Remove that random effect                |

#### Adjusting TMB Control

``` r
fit <- fit_demand_tmb(
  data,
  y_var = "y", x_var = "x", id_var = "id",
  tmb_control = list(
    iter_max = 2000,      # Increase iteration limit (default: 1000)
    eval_max = 4000,      # Increase function evaluation limit (default: 2000)
    rel_tol = 1e-12       # Tighten relative tolerance for more precision
  )
)
```

#### Switching Optimizer

If `nlminb` produces code 8 (false convergence), switching to L-BFGS-B
sometimes helps because it uses a different line search strategy:

``` r
fit_lbfgsb <- fit_demand_tmb(
  data,
  y_var = "y", x_var = "x", id_var = "id",
  tmb_control = list(optimizer = "L-BFGS-B")
)
```

#### Using Warm Starts

If you have a partially converged fit, you can use it as a starting
point for a fresh optimization:

``` r
# First fit (may not fully converge)
fit1 <- fit_demand_tmb(data, y_var = "y", x_var = "x", id_var = "id")

# Use as warm start for a second attempt
fit2 <- fit_demand_tmb(data, y_var = "y", x_var = "x", id_var = "id",
                       tmb_control = list(warm_start = fit1$opt$par))
```

#### Hurdle Model Remedies

Hurdle models have additional options for controlling complexity:

``` r
# Reduce random effects: 2-RE instead of 3-RE
fit_2re <- fit_demand_hurdle(
  data,
  y_var = "y", x_var = "x", id_var = "id",
  random_effects = c("zeros", "q0")  # Drop alpha random effect
)

# Try a different Part II equation
fit_snd <- fit_demand_hurdle(
  data,
  y_var = "y", x_var = "x", id_var = "id",
  part2_equation = "simplified_exponential"  # Fewer parameters (no k)
)
```

### Fixed-Effect NLS Remedies

[`fit_demand_fixed()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_fixed.md)
handles convergence automatically via multi-backend fallback. When many
subjects fail to converge, the issue is almost always data quality
rather than model specification:

- **Too few price points**: demand curves need at least 4-5 prices to
  fit reliably
- **No variation in consumption**: if consumption is constant across
  prices, the model has nothing to fit
- **Extreme outliers**: a single wildly atypical data point can prevent
  convergence
- **Too many zeros**: consider
  [`fit_demand_hurdle()`](https://brentkaplan.github.io/beezdemand/reference/fit_demand_hurdle.md)
  instead, which handles zeros explicitly

``` r
# Check data quality first
systematic <- CheckUnsystematic(data, deltaq = 0.025, bounce = 0.1, reversals = 0)
table(systematic$TotalPass)

# Try a different equation if "hs" fails for many subjects
fit_koff <- fit_demand_fixed(data, equation = "koff")
```

## Using check_demand_model()

The
[`check_demand_model()`](https://brentkaplan.github.io/beezdemand/reference/check_demand_model.md)
function automates the most important diagnostic checks for all model
tiers. It returns a `beezdemand_diagnostics` object that summarizes
convergence status, boundary conditions, residual patterns, and random
effect diagnostics.

### TMB Example

``` r
fit_hurdle <- fit_demand_hurdle(apt, y_var = "y", x_var = "x", id_var = "id")
diagnostics <- check_demand_model(fit_hurdle)
print(diagnostics)
```

The output shows:

- **Convergence**: status and any optimizer messages
- **Random Effects**: variance estimates and whether any are near zero
- **Residuals**: summary statistics and outlier count
- **Issues/Recommendations**: actionable suggestions

### NLME Example

``` r
fit_nlme <- fit_demand_mixed(data, y_var = "y_ll4", equation_form = "zben")
diagnostics <- check_demand_model(fit_nlme)
print(diagnostics)
```

For NLME models, the diagnostics additionally check for:

- Stored fit warnings (e.g., “false convergence”)
- `apVar` status (Hessian positive-definiteness)
- Random effect correlation near-singularity (\|corr\| \> 0.99)

### Live Example with Fixed-Effect Model

Here is a live example using the built-in `apt` dataset:

``` r
data(apt)
fit <- fit_demand_fixed(apt, equation = "hs")
diagnostics <- check_demand_model(fit)
print(diagnostics)
#> 
#> Model Diagnostics
#> ================================================== 
#> Model class: beezdemand_fixed 
#> 
#> Convergence:
#>   Status: Converged
#> 
#> Residuals:
#>   Mean: 0.0284
#>   SD: 0.5306
#>   Range: [-1.458, 2.228]
#>   Outliers: 3 observations
#> 
#> --------------------------------------------------
#> Issues Detected (1):
#>   1. Detected 3 potential outliers across subjects
```

### Accessing Fit Warnings Directly

For NLME models, you can also inspect the captured warnings directly:

``` r
# Warnings captured during NLME fitting
fit_nlme$fit_warnings

# These are also surfaced by glance()
glance_result <- broom::glance(fit_nlme)
glance_result$converged
```

## Summary

Key takeaways:

1.  **Convergence codes and warnings are about stopping rules, not
    solution quality.** Code 0 or no warnings means the optimizer is
    satisfied — it doesn’t guarantee a good solution. Non-convergence
    doesn’t mean the estimates are wrong.

2.  **Use the six-item diagnostic checklist.** Parameter plausibility,
    standard errors, gradient norm (TMB), Hessian/apVar status, model
    comparison, and starting value stability.

3.  **NLME and TMB have different mechanics but the same diagnostic
    principles.** NLME uses EM + PNLS iterations and reports warnings;
    TMB uses direct likelihood optimization and reports numeric codes.
    Both can produce trustworthy results even when not fully converged.

4.  **[`check_demand_model()`](https://brentkaplan.github.io/beezdemand/reference/check_demand_model.md)
    automates the most important checks.** Use it on any fitted model to
    get a quick summary of potential issues.

5.  **When in doubt, fit simpler and compare.** Reduce random effects,
    try diagonal covariance, or switch equation forms. If a simpler
    converged model produces similar estimates to a non-converged
    complex model, the estimates are likely trustworthy.

### Cross-References

- [`vignette("hurdle-demand-models")`](https://brentkaplan.github.io/beezdemand/articles/hurdle-demand-models.md)
  — Fitting TMB hurdle models
- [`vignette("mixed-demand")`](https://brentkaplan.github.io/beezdemand/articles/mixed-demand.md)
  — Fitting NLME mixed-effects models
- [`vignette("mixed-demand-advanced")`](https://brentkaplan.github.io/beezdemand/articles/mixed-demand-advanced.md)
  — Advanced NLME topics including control tuning
- [`vignette("fixed-demand")`](https://brentkaplan.github.io/beezdemand/articles/fixed-demand.md)
  — Fitting fixed-effect NLS models
- [`vignette("model-selection")`](https://brentkaplan.github.io/beezdemand/articles/model-selection.md)
  — Choosing between model tiers

## References

- Gay, D. M. (1990). Usage summary for selected optimization routines
  (Computing Science Technical Report No. 153). AT&T Bell Laboratories.
  (Describes the PORT routines underlying `nlminb`.)

- Kristensen, K., Nielsen, A., Berg, C. W., Skaug, H., & Bell, B. M.
  (2016). TMB: Automatic differentiation and Laplace approximation.
  *Journal of Statistical Software*, *70*(5), 1-21.

- Nocedal, J., & Wright, S. J. (2006). *Numerical Optimization* (2nd
  ed.). Springer.

- Pinheiro, J. C., & Bates, D. M. (2000). *Mixed-Effects Models in S and
  S-PLUS*. Springer. (The definitive reference for the `nlme` package’s
  optimization architecture.)
