# Calculate Amplitude and Persistence

Calculates Amplitude and Persistence latent factors from demand metrics
for various beezdemand model objects.

## Usage

``` r
calculate_amplitude_persistence(
  fit,
  amplitude = c("Intensity", "Q0d", "Q0"),
  persistence = c("BP0", "Pmaxe", "Omaxe", "Alpha"),
  use_inv_alpha = TRUE,
  strict = TRUE,
  min_persistence_components = 2L,
  empirical_y_var = NULL,
  basis_means = NULL,
  basis_sds = NULL,
  ...
)
```

## Arguments

- fit:

  An object of class \`beezdemand_fixed\`, \`beezdemand_hurdle\`,
  \`beezdemand_nlme\`, or a \`data.frame\`.

- amplitude:

  Character vector of column names to consider for the Amplitude factor.
  The function will use the first column found in the data. Default is
  \`c("Intensity", "Q0d", "Q0")\`.

- persistence:

  Character vector of column names to include in the Persistence factor.
  Default is \`c("BP0", "Pmaxe", "Omaxe", "Alpha")\`. All found columns
  will be used.

- use_inv_alpha:

  Logical. If "Alpha" (or a variation) is present in \`persistence\`,
  should it be inverted (1/Alpha) before standardization? Default is
  \`TRUE\`.

- strict:

  Logical. If \`TRUE\` (default), missing metrics, duplicated \`id\`s,
  and other data integrity problems produce errors instead of warnings.

- min_persistence_components:

  Integer. Minimum number of non-missing standardized persistence
  components required to compute \`Persistence\` for a given \`id\`. If
  fewer are available, \`Persistence\` is set to \`NA\`. Default is
  \`2\`.

- empirical_y_var:

  For \`beezdemand_nlme\` objects, optional column name in \`fit\$data\`
  to use when computing empirical indices (e.g., \`BP0\`). This is
  important when the fitted \`y_var\` is transformed (e.g., log10). If
  \`NULL\`, the method will attempt to choose a sensible default and may
  error in \`strict\` mode if ambiguous.

- basis_means:

  Optional named numeric vector of means to use for Z-score
  standardization. Names must match the columns used (e.g.,
  \`c(Intensity = 10, BP0 = 5)\`). If NULL (default), the sample means
  are used.

- basis_sds:

  Optional named numeric vector of standard deviations to use for
  Z-score standardization. Names must match the columns used. If NULL
  (default), the sample SDs are used.

- ...:

  Additional arguments passed to methods.

## Value

A data frame with the original ID and calculated Amplitude and
Persistence scores, along with the standardized (Z-scored) constituent
metrics.

## Details

This function calculates Amplitude and Persistence by: 1. Extracting the
relevant demand metrics from the \`fit\` object. 2. Resolving requested
metric columns (case-insensitive, with limited synonym support for
common beezdemand outputs). 3. Inverting Alpha if requested (1/Alpha).
4. Standardizing (Z-scoring) the metrics. If \`basis_means\` and
\`basis_sds\` are provided, they are used; otherwise, the current
sample's statistics are used. 5. Aggregating the Z-scores into the two
latent factors.

\*\*Amplitude\*\* is defined by the variable specified in \`amplitude\`
(typically Intensity/Q0). \*\*Persistence\*\* is defined as the mean of
the standardized values of the variables specified in \`persistence\`
(typically Breakpoint, Pmax, Omax, and 1/Alpha).

## Models

\* \*\*beezdemand_fixed\*\*: Extracts metrics from \`fit\$results\`. \*
\*\*beezdemand_hurdle\*\*: Extracts metrics from \`fit\$subject_pars\`.
\* \*\*beezdemand_nlme\*\*: Calculates subject-specific parameters from
fixed and random effects. Parameters \`Q0\` and \`Alpha\` are assumed to
be on \`log10\` scale for \`zben\` and \`simplified\` equations and are
converted to linear scale. Omax and Pmax are calculated empirically from
predictions. Breakpoint is calculated empirically from the raw data.

## Examples

``` r
# \donttest{
data(apt, package = "beezdemand")
fit <- FitCurves(apt, "hs", k = "share")
#> Warning: Zeros found in data not compatible with equation! Dropping zeros!
#> Beginning search for best-starting k
#> Best k fround at 0.93813356574003 = err: 0.744881846162718
#> Searching for shared K, this can take a while...
calculate_amplitude_persistence(fit)
#>     id z_Intensity z_BP0    z_Pmaxe    z_Omaxe z_inv_alpha   Amplitude
#> 1   19  1.22315326    NA  0.6635528  0.9612281   1.4326498  1.22315326
#> 2   30 -1.45249450    NA  1.5142616 -0.6624680  -0.8616538 -1.45249450
#> 3   38 -1.07025910    NA -0.6975812 -0.5975202  -0.4247018 -1.07025910
#> 4   60  1.22315326    NA -0.5274394 -0.4026766  -0.3998881  1.22315326
#> 5   68  1.22315326    NA -0.3572977  0.3766975   0.4872452  1.22315326
#> 6  106 -0.68802371    NA -1.0378647 -0.9872073  -0.9645431 -0.68802371
#> 7  113 -0.30578831    NA  0.6635528  0.9612281   1.4535929 -0.30578831
#> 8  142  0.45868247    NA  1.5142616  1.9354458   0.9744894  0.45868247
#> 9  156  0.07644708    NA -0.6975812 -0.5975202  -0.5369514  0.07644708
#> 10 188 -0.68802371    NA -1.0378647 -0.9872073  -1.1602392 -0.68802371
#>     Persistence
#> 1   1.019143585
#> 2  -0.003286756
#> 3  -0.573267704
#> 4  -0.443334708
#> 5   0.168881670
#> 6  -0.996538352
#> 7   1.026124625
#> 8   1.474732278
#> 9  -0.610684268
#> 10 -1.061770372
# }
```
