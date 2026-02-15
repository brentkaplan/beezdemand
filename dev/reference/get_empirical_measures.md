# Calculate Empirical Demand Measures

Calculates empirical (model-free) measures of demand from purchase task
data. These metrics characterize consumption patterns without fitting a
demand curve model.

This is the modern replacement for \[GetEmpirical()\], returning a
structured S3 object with dedicated methods for printing, summarizing,
and visualizing.

## Usage

``` r
get_empirical_measures(data, x_var = "x", y_var = "y", id_var = "id")
```

## Arguments

- data:

  A data frame in long format with columns for subject ID, price, and
  consumption

- x_var:

  Character string specifying the column name for price (default: "x")

- y_var:

  Character string specifying the column name for consumption (default:
  "y")

- id_var:

  Character string specifying the column name for subject ID (default:
  "id")

## Value

An S3 object of class \`beezdemand_empirical\` containing:

- **measures** - Data frame with one row per subject and columns:

  - *id* - Subject identifier

  - *Intensity* - Consumption at lowest price (demand intensity)

  - *BP0* - Breakpoint 0: first price where consumption = 0

  - *BP1* - Breakpoint 1: last price with non-zero consumption

  - *Omaxe* - Empirical Omax: maximum total expenditure (price ×
    consumption)

  - *Pmaxe* - Empirical Pmax: price at which maximum expenditure occurs

- **call** - The matched call

- **data_summary** - List with n_subjects, has_zeros, and complete_cases

## Details

\## Empirical Measures

- **Intensity** - The consumption value at the lowest price point.
  Reflects unrestricted demand or preferred level of consumption.

- **BP0** (Breakpoint 0) - The first price at which consumption drops to
  zero. If consumption never reaches zero, BP0 = NA. Indicates the price
  threshold where the commodity becomes unaffordable or undesirable.

- **BP1** (Breakpoint 1) - The highest price at which consumption is
  still non-zero. If all consumption is zero, BP1 = NA. Represents the
  upper limit of the commodity's value to the consumer.

- **Omaxe** (Empirical Omax) - The maximum observed expenditure across
  all prices (max of price × consumption). Represents peak spending on
  the commodity.

- **Pmaxe** (Empirical Pmax) - The price at which maximum expenditure
  occurs. If maximum expenditure is zero, Pmaxe = 0. If multiple prices
  have the same maximum expenditure, the highest price is returned.

## Note

- Data must not contain duplicate prices within a subject (will error)

- Missing values (NA) in consumption are preserved in calculations where
  applicable

- Breakpoints require at least one zero consumption value to be
  meaningful

## See also

- \[GetEmpirical()\] - Legacy function (superseded)

- \[plot.beezdemand_empirical()\] - Visualization method

- \[summary.beezdemand_empirical()\] - Extended summary

## Examples

``` r
if (FALSE) { # \dontrun{
data(apt, package = "beezdemand")

# Calculate empirical measures
emp <- get_empirical_measures(apt)
print(emp)

# View measures table
emp$measures

# Extended summary with distribution info
summary(emp)

# Visualize distribution of measures
plot(emp)  # histogram by default
plot(emp, type = "matrix")  # scatterplot matrix
} # }
```
