# Calculate K Scaling Parameter for Demand Curve Fitting

Calculates the k scaling parameter used in demand curve equations to
normalize consumption across different units or ranges. The k value is
derived from the logarithmic range of consumption values.

This is the modern replacement for \[GetK()\], with explicit parameters
for the adjustment value and optional verbose output.

## Usage

``` r
get_k(
  data,
  use_means = TRUE,
  adjustment = 0.5,
  x_var = "x",
  y_var = "y",
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame in long format with columns for price and consumption

- use_means:

  Logical indicating whether to calculate k from mean consumption by
  price (TRUE, default) or from individual consumption values (FALSE)

- adjustment:

  Numeric adjustment added to the log range (default: 0.5). This value
  ensures k is slightly larger than the observed range.

- x_var:

  Character string specifying the column name for price (default: "x")

- y_var:

  Character string specifying the column name for consumption (default:
  "y")

- verbose:

  Logical indicating whether to print calculation details (default:
  FALSE)

## Value

A single numeric value representing the k scaling parameter

## Details

The k parameter is calculated as:

\$\$k = \log\_{10}(\text{max}) - \log\_{10}(\text{min}) +
\text{adjustment}\$\$

where max and min are the maximum and minimum non-zero consumption
values.

\## Use in Demand Equations

The k parameter appears in several demand curve equations:

- **Hursh & Silberberg (2008)**: Scales the exponential term

- **Koffarnus et al. (2015)**: Normalizes the exponentiated model

- Ensures numerical stability during model fitting

\## Calculation Modes

- **use_means = TRUE** (default): Calculates k from mean consumption at
  each price point. Recommended when data has multiple subjects, as it
  reduces influence of individual outliers.

- **use_means = FALSE**: Calculates k from the full range of individual
  consumption values. May be preferable for single-subject data or when
  individual variability is theoretically important.

## Note

- Only non-zero consumption values are used (zero values are excluded)

- Missing values (NA) are automatically removed via \`na.rm = TRUE\`

- The default adjustment of 0.5 is conventional but can be modified

## See also

- \[GetK()\] - Legacy function (superseded)

- \[FitCurves()\] - Uses k parameter in demand curve fitting

## Examples

``` r
if (FALSE) { # \dontrun{
data(apt, package = "beezdemand")

# Calculate k using default settings (mean range + 0.5)
k_val <- get_k(apt)

# Calculate k from individual values
k_ind <- get_k(apt, use_means = FALSE)

# Calculate with custom adjustment
k_custom <- get_k(apt, adjustment = 1.0)

# Show calculation details
k_verbose <- get_k(apt, verbose = TRUE)
} # }
```
