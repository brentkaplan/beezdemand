# S3 Methods for beezdemand_empirical Objects

Methods for printing, summarizing, and visualizing objects of class
\`beezdemand_empirical\` created by \[get_empirical_measures()\].

## Usage

``` r
# S3 method for class 'beezdemand_empirical'
print(x, ...)

# S3 method for class 'beezdemand_empirical'
summary(object, ...)

# S3 method for class 'beezdemand_empirical'
plot(x, type = "histogram", ...)
```

## Arguments

- x, object:

  A \`beezdemand_empirical\` object

- ...:

  Additional arguments passed to plotting functions

- type:

  Character string specifying plot type. Options:

  - "histogram" (default) - Faceted histograms showing distribution of
    each measure

  - "matrix" - Scatterplot matrix showing pairwise relationships between
    measures

## Value

- \`print()\` - Returns the object invisibly (called for side effects)

- \`summary()\` - Returns a list with extended summary information

- \`plot()\` - Returns a ggplot2 object

## Details

\## Print Method Displays a compact summary showing the number of
subjects analyzed and a preview of the empirical measures table.

\## Summary Method Provides extended information including:

- Data summary (subjects, zero consumption patterns, completeness)

- Descriptive statistics for each empirical measure (min, median, mean,
  max, SD)

- Missing data patterns

\## Plot Method Creates visualizations of empirical measures across
subjects.

\*\*Histogram type\*\* (default):

- Six-panel faceted plot showing distribution of each measure

- Helps identify central tendencies and outliers

- Uses modern beezdemand styling

\*\*Matrix type\*\*:

- Scatterplot matrix (pairs plot) showing relationships between measures

- Useful for identifying correlated demand metrics

- Lower triangle: scatterplots with smoothed trend lines

- Diagonal: density plots

- Upper triangle: correlation coefficients

## See also

\[get_empirical_measures()\]

## Examples

``` r
if (FALSE) { # \dontrun{
data(apt, package = "beezdemand")
emp <- get_empirical_measures(apt)

# Print compact summary
print(emp)

# Extended summary
summary(emp)

# Histogram of measure distributions
plot(emp)

# Scatterplot matrix
plot(emp, type = "matrix")
} # }
```
