# Fit Nonlinear Mixed-Effects Demand Model

Fits a nonlinear mixed-effects model for behavioral economic demand
data. The function allows Q0 and alpha parameters to vary by specified
factors and supports different demand equation forms.

## Usage

``` r
fit_demand_mixed(
  data,
  y_var,
  x_var,
  id_var,
  factors = NULL,
  factor_interaction = FALSE,
  equation_form = c("zben", "simplified", "exponentiated"),
  param_space = c("log10", "natural"),
  k = NULL,
  custom_model_formula = NULL,
  fixed_rhs = NULL,
  continuous_covariates = NULL,
  start_value_method = c("heuristic", "pooled_nls"),
  random_effects = Q0 + alpha ~ 1,
  covariance_structure = c("pdDiag", "pdSymm"),
  start_values = NULL,
  collapse_levels = NULL,
  nlme_control = list(msMaxIter = 200, niterEM = 100, maxIter = 200, pnlsTol = 0.001,
    tolerance = 1e-06, apVar = TRUE, minScale = 1e-09, opt = "nlminb", msVerbose = FALSE),
  method = "ML",
  ...
)
```

## Arguments

- data:

  A data frame.

- y_var:

  Character string, the name of the dependent variable column. For
  `equation_form = "zben"`, this should be log-transformed consumption
  (e.g., "y_ll4"). For `equation_form = "simplified"` or
  `"exponentiated"`, this should be raw, untransformed consumption
  (e.g., "y").

- x_var:

  Character string, the name of the independent variable column (e.g.,
  "x", price).

- id_var:

  Character string, the name of the subject/group identifier column for
  random effects.

- factors:

  A character vector of factor names (up to two) by which Q0 and alpha
  are expected to vary (e.g., `c("dose", "treatment")`).

- factor_interaction:

  Logical. If `TRUE` and two factors are provided, their interaction
  term is included in the fixed effects for Q0 and alpha. Defaults to
  `FALSE` (additive effects).

- equation_form:

  Character string specifying the demand equation form. Options are:

  - `"zben"` (default): Assumes `y_var` is log-transformed. Equation:
    `y_var ~ Q0 * exp(-(10^alpha / Q0) * (10^Q0) * x_var)`. Model
    parameter `Q0` represents `log10(True Max Consumption)`. Model
    parameter `alpha` represents `log10(True Alpha Sensitivity)`.

  - `"simplified"`: Assumes `y_var` is raw (untransformed) consumption.
    Equation: `y_var ~ (10^Q0) * exp(-(10^alpha) * (10^Q0) * x_var)`.
    Model parameter `Q0` represents `log10(True Max Consumption)`. Model
    parameter `alpha` represents `log10(True Alpha Sensitivity)`.

  - `"exponentiated"`: Koffarnus et al. (2015) exponentiated equation.
    Assumes `y_var` is raw (untransformed) consumption. Requires the `k`
    parameter. Equation (log10 param_space):
    `y_var ~ (10^Q0) * 10^(k * (exp(-(10^alpha) * (10^Q0) * x_var) - 1))`.
    Equation (natural param_space):
    `y_var ~ Q0 * 10^(k * (exp(-alpha * Q0 * x_var) - 1))`.

- param_space:

  Character. Parameterization used for fitting core demand parameters.
  One of:

  - `"log10"`: treat `Q0` and `alpha` as `log10(Q0)` and `log10(alpha)`
    (default)

  - `"natural"`: treat `Q0` and `alpha` as natural-scale parameters

  Notes:

  - For `equation_form = "zben"`, only `"log10"` is currently supported.

  - For `equation_form = "simplified"` or `"exponentiated"`, both
    `"log10"` and `"natural"` are supported.

- k:

  Numeric. Range parameter (in log10 units) used with
  `equation_form = "exponentiated"`. If `NULL` (default), k is
  calculated from the data range: `log10(max(y)) - log10(min(y)) + 0.5`.
  Ignored for other equation forms.

- custom_model_formula:

  An optional custom nonlinear model formula (nlme format). If provided,
  this overrides `equation_form`. The user is responsible for ensuring
  the `y_var` scale matches the formula and that starting values are
  appropriate. The formula should use parameters named `Q0` and `alpha`.

- fixed_rhs:

  Optional one-sided formula or character string specifying the
  right-hand side (RHS) for the fixed-effects linear models of `Q0` and
  `alpha`. When provided, this RHS is used for both parameters and
  overrides `factors`, `factor_interaction`, and `continuous_covariates`
  for building the fixed-effects design matrix. Example:
  `"~ 1 + drug * dose + session"`.

- continuous_covariates:

  Optional character vector of continuous (numeric) predictor names to
  be included additively in the fixed-effects RHS when `fixed_rhs` is
  `NULL`. These variables are not coerced to factors and are stored for
  downstream functions (e.g., plotting) to condition on.

- start_value_method:

  Character, method to generate starting values if `start_values` is
  NULL. Options: "heuristic" (default, uses data-driven heuristics) or
  "pooled_nls" (fits a simpler pooled NLS model first; falls back to
  heuristic if NLS fails).

- random_effects:

  A formula or a list of formulas for the random effects structure.
  Default `nlme::pdDiag(Q0 + alpha ~ 1)`.

- covariance_structure:

  Character, covariance structure for random effects. Options:
  `"pdDiag"` (default) or `"pdSymm"`

- start_values:

  Optional named list of starting values for fixed effects. If `NULL`,
  defaults are estimated based on `equation_form` and `y_var` scale.

- collapse_levels:

  Optional named list specifying factor level collapsing separately for
  Q0 and alpha parameters. Structure:

      list(
        Q0 = list(factor_name = list(new_level = c(old_levels), ...)),
        alpha = list(factor_name = list(new_level = c(old_levels), ...))
      )

  Either `Q0` or `alpha` (or both) can be omitted to use original factor
  levels for that parameter. This allows different collapsing schemes
  for each parameter. Ignored if `fixed_rhs` is provided.

- nlme_control:

  Control parameters for
  [`nlme::nlme()`](https://rdrr.io/pkg/nlme/man/nlme.html).

- method:

  Fitting method for
  [`nlme::nlme()`](https://rdrr.io/pkg/nlme/man/nlme.html) ("ML" or
  "REML"). Default "ML".

- ...:

  Additional arguments passed to
  [`nlme::nlme()`](https://rdrr.io/pkg/nlme/man/nlme.html).

## Value

An object of class `beezdemand_nlme`.

## Examples

``` r
# \donttest{
# Basic mixed-effects demand fit with apt data
# Transform consumption using LL4 for the zben equation
apt_ll4 <- apt |> dplyr::mutate(y_ll4 = ll4(y))

fit <- fit_demand_mixed(
  data = apt_ll4,
  y_var = "y_ll4",
  x_var = "x",
  id_var = "id",
  equation_form = "zben"
)
#> Generating starting values using method: 'heuristic'
#> Using heuristic method for starting values.
#> --- Fitting NLME Model ---
#> Equation Form: zben
#> Param Space: log10
#> NLME Formula: y_ll4 ~ Q0 * exp(-(10^alpha/Q0) * (10^Q0) * x)
#> Start values (first few): Q0_int=0.812, alpha_int=-3
#> Number of fixed parameters: 2 (Q0: 1, alpha: 1)
print(fit)
#> Demand NLME Model Fit ('beezdemand_nlme' object)
#> ---------------------------------------------------
#> 
#> Call:
#> fit_demand_mixed(data = apt_ll4, y_var = "y_ll4", x_var = "x", 
#>     id_var = "id", equation_form = "zben")
#> 
#> Equation Form Selected:  zben 
#> NLME Model Formula:
#> y_ll4 ~ Q0 * exp(-(10^alpha/Q0) * (10^Q0) * x)
#> <environment: 0x56398b9795e0>
#> Fixed Effects Structure (Q0 & alpha):  ~ 1 
#> Factors: None
#> ID Variable for Random Effects:  id 
#> 
#> Start Values Used (Fixed Effects Intercepts):
#>   Q0 Intercept (log10 scale):  0.8117 
#>   alpha Intercept (log10 scale):  -3 
#> 
#> --- NLME Model Fit Summary (from nlme object) ---
#> Nonlinear mixed-effects model fit by maximum likelihood
#>   Model: nlme_model_formula_obj 
#>   Data: data 
#>   Log-likelihood: 146.7891
#>   Fixed: list(Q0 ~ 1, alpha ~ 1) 
#>         Q0      alpha 
#>  0.8582675 -1.9745159 
#> 
#> Random effects:
#>  Formula: list(Q0 ~ 1, alpha ~ 1)
#>  Level: id
#>  Structure: Diagonal
#>                Q0    alpha   Residual
#> StdDev: 0.1690922 0.228735 0.07914359
#> 
#> Number of Observations: 160
#> Number of Groups: 10 
#> 
#> --- Additional Fit Statistics ---
#> Log-likelihood:  146.8 
#> AIC:  -283.6 
#> BIC:  -268.2 
#> ---------------------------------------------------
summary(fit)
#> 
#> Nonlinear Mixed-Effects Demand Model Summary
#> ================================================== 
#> 
#> Model Specification:
#>   Equation form: zben 
#>   ID variable: id 
#> 
#> Data Summary:
#>   Subjects: 10 
#>   Observations: 160 
#> 
#> Fixed Effects:
#>           Value Std.Error        DF t-value  p-value    
#> Q0      7.21552   0.91936 149.00000   7.848 4.21e-15 ***
#> alpha   0.01060   0.00182 149.00000   5.827 5.65e-09 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Random Effects:
#> id = pdDiag(list(Q0 ~ 1,alpha ~ 1)) 
#>          Variance    StdDev    
#> Q0       0.028592173 0.16909220
#> alpha    0.052319710 0.22873502
#> Residual 0.006263707 0.07914359
#> 
#> Residual standard error: 0.0791 
#> 
#> Model Fit:
#>   Log-Likelihood: 146.79 
#>   AIC: -283.58 
#>   BIC: -268.2 
# }
```
