# Parameter Naming Registry for beezdemand

This file defines canonical parameter naming conventions across all
model families. It serves as the single source of truth for parameter
names, scales, and mappings.

## Naming Convention

Parameters follow snake_case with scale prefixes:

- `log10_<param>`: parameter in log10 space (e.g., log10_alpha)

- `log_<param>`: parameter in natural log space (e.g., log_alpha)

- `natural_<param>`: parameter in natural/linear space (e.g.,
  natural_alpha)

- `<param>`: canonical name without prefix (context determines scale)

## Equation Registry

Equations are grouped by model family:

- **NLS (FitCurves)**: `hs`, `koff`, `linear`. The `k` parameter
  defaults to fixed but supports multiple modes: `"fit"`, `"ind"`,
  `"share"`, `"range"`.

- **NLME (fit_demand_mixed)**: `zben`, `nlme_simplified`,
  `nlme_exponentiated`. Only `nlme_exponentiated` uses `k` (always
  fixed).

- **Hurdle Part II (fit_demand_hurdle)**: `hurdle_zhao`, `hurdle_hs`,
  `hurdle_snd`. Parameters are in log space. `hurdle_snd` has no `k`
  parameter. Each variant has 2RE and 3RE sub-models with different
  variance/correlation parameters.

- **Cross-price (fit_cp_nls)**: `cp_exponential`, `cp_exponentiated`,
  `cp_additive`. All use `log10_qalone`, `I`, `log10_beta`.

## Derived Metrics Naming

- `pmax_model`: Pmax derived from fitted model parameters

- `pmax_obs`: Pmax from observed/empirical data (max expenditure price)

- `omax_model`: Omax derived from fitted model parameters

- `omax_obs`: Omax from observed/empirical data

- `alpha_star`: Normalized alpha (Strategy B; Rzeszutek et al., 2025).
  Makes alpha comparable across different k values by adjusting for the
  scaling constant. Computed as \\\alpha^\* = -\alpha / \ln(1 - 1/(k
  \cdot \ln(b)))\\ where \\b\\ is the logarithmic base (10 for HS/Koff,
  \\e\\ for hurdle models). Returned by
  [`FitCurves`](https://brentkaplan.github.io/beezdemand/reference/FitCurves.md),
  [`tidy()`](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_fixed.md)
  on fixed fits, and
  [`tidy()`](https://brentkaplan.github.io/beezdemand/reference/tidy.beezdemand_hurdle.md)
  on hurdle fits. Standard error (`alpha_star_se`) is obtained via the
  delta method. Requires \\k \cdot \ln(b) \> 1\\; otherwise `NA` is
  returned.

## Legacy Column Mappings

Legacy FitCurves output uses different naming. The mappings are:

- `Pmaxd`: derived Pmax from model (approximate formula)

- `Pmaxa`: analytic Pmax from Lambert W

- `Pmaxe`: empirical/observed Pmax from data

- `Omaxd`: derived Omax from model

- `Omaxa`: analytic Omax from Lambert W

- `Omaxe`: empirical/observed Omax from data

- `Q0d`: fitted Q0 (intensity)

- `Alpha`: fitted alpha (elasticity parameter)

- `K`: k parameter (range in log units)
