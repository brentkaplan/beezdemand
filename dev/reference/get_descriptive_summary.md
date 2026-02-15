# Calculate Descriptive Statistics by Price

Calculates summary statistics for consumption data at each price point,
including measures of central tendency (mean, median), variability (SD),
range (min, max), and data quality (proportion of zeros, missing
values).

This is the modern replacement for \[GetDescriptives()\], returning a
structured S3 object with dedicated methods for printing, summarizing,
and visualizing.

## Usage

``` r
get_descriptive_summary(data, x_var = "x", y_var = "y", id_var = "id")
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

An S3 object of class \`beezdemand_descriptive\` containing:

- **statistics** - Data frame with 8 columns (Price, Mean, Median, SD,
  PropZeros, NAs, Min, Max) and one row per unique price

- **call** - The matched call

- **data_summary** - List with n_subjects, n_prices, and prices vector

## Details

For each unique price in the dataset, the function calculates:

- **Mean** - Average consumption across subjects (rounded to 2 decimals)

- **Median** - Median consumption (rounded to 2 decimals)

- **SD** - Standard deviation (rounded to 2 decimals)

- **PropZeros** - Proportion of subjects with zero consumption (0-1)

- **NAs** - Count of missing values

- **Min** - Minimum consumption value (rounded to 2 decimals)

- **Max** - Maximum consumption value (rounded to 2 decimals)

## See also

- \[GetDescriptives()\] - Legacy function (superseded)

- \[plot.beezdemand_descriptive()\] - Visualization method

- \[summary.beezdemand_descriptive()\] - Extended summary

## Examples

``` r
if (FALSE) { # \dontrun{
data(apt, package = "beezdemand")

# Calculate descriptive statistics
desc <- get_descriptive_summary(apt)
print(desc)

# View statistics table
desc$statistics

# Create visualization
plot(desc)

# Extended summary with distribution info
summary(desc)
} # }
```
