# S3 Methods for beezdemand_descriptive Objects

Methods for printing, summarizing, and visualizing objects of class
\`beezdemand_descriptive\` created by \[get_descriptive_summary()\].

## Usage

``` r
# S3 method for class 'beezdemand_descriptive'
print(x, ...)

# S3 method for class 'beezdemand_descriptive'
summary(object, ...)

# S3 method for class 'beezdemand_descriptive'
plot(x, x_trans = "identity", y_trans = "identity", show_zeros = FALSE, ...)
```

## Arguments

- x, object:

  A \`beezdemand_descriptive\` object

- ...:

  Additional arguments (currently unused)

- x_trans:

  Character string specifying x-axis transformation. Options: "identity"
  (default), "log10", "log", "sqrt". See \[scales::transform_log10()\]
  etc.

- y_trans:

  Character string specifying y-axis transformation. Options: "identity"
  (default), "log10", "log", "sqrt", "pseudo_log" (signed log).

- show_zeros:

  Logical indicating whether to show proportion of zeros as labels on
  the boxplot (default: FALSE)

## Value

- \`print()\` - Returns the object invisibly (called for side effects)

- \`summary()\` - Returns a list with extended summary information

- \`plot()\` - Returns a ggplot2 object

## Details

\## Print Method Displays a compact summary showing the number of
subjects and prices analyzed, plus a preview of the statistics table.

\## Summary Method Provides extended information including:

- Data summary (subjects, prices analyzed)

- Distribution of means across prices (min, median, max)

- Proportion of zeros by price (range)

- Missing data summary

\## Plot Method Creates a boxplot showing the distribution of
consumption at each price point. Features:

- Red cross markers indicate means

- Boxes show median and quartiles

- Whiskers extend to 1.5 \* IQR

- Supports axis transformations (log, sqrt, etc.)

- Uses modern beezdemand styling via \[theme_apa()\]

## See also

\[get_descriptive_summary()\]

## Examples

``` r
if (FALSE) { # \dontrun{
data(apt, package = "beezdemand")
desc <- get_descriptive_summary(apt)

# Print compact summary
print(desc)

# Extended summary
summary(desc)

# Default boxplot
plot(desc)

# With log-transformed y-axis
plot(desc, y_trans = "log10")

# With both axes transformed
plot(desc, x_trans = "log10", y_trans = "pseudo_log")
} # }
```
