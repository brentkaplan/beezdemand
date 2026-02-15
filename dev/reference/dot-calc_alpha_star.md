# Compute Normalized Alpha (Alpha Star) via the Delta Method

Implements Strategy B normalization of the demand elasticity parameter
\\\alpha\\ so that values are comparable across different \\k\\ values
(Rzeszutek et al., 2025). The formula is \\\alpha^\* = -\alpha / \ln(1 -
1/(k \cdot \ln(b)))\\ where \\b\\ is the logarithmic base used by the
demand equation (10 for HS/Koff, \\e\\ for hurdle models).

## Usage

``` r
.calc_alpha_star(params, param_scales, vcov = NULL, base = c("e", "10"))
```

## Arguments

- params:

  Named list of parameter values. Must contain entries matchable to
  alpha and k (e.g., `alpha`, `log_alpha`, `log10_alpha`).

- param_scales:

  Named list indicating the scale of each parameter in `params`:
  `"natural"`, `"log"`, or `"log10"`.

- vcov:

  Optional. Either a variance–covariance matrix with named rows/columns,
  or a named numeric vector of standard errors for the alpha and k
  parameters.

- base:

  Character; the logarithmic base: `"e"` (natural log, used by hurdle
  models) or `"10"` (log10, used by HS/Koff equations).

## Value

A list with elements:

- estimate:

  Numeric scalar; the alpha_star value, or `NA`.

- se:

  Numeric scalar; delta-method SE, or `NA`.

- note:

  Character or `NULL`; diagnostic message if alpha_star could not be
  computed.

## Details

Standard errors are obtained via the delta method when a
variance–covariance matrix (or SE vector) is supplied.

## References

Rzeszutek, M. J., Regnier, S. D., Franck, C. T., & Koffarnus, M. N.
(2025). Overviewing the exponential model of demand and introducing a
simplification that solves issues of span, scale, and zeros.
\*Experimental and Clinical Psychopharmacology\*.
