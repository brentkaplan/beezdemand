# Tidy method for beezdemand_nlme

Tidy method for beezdemand_nlme

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
tidy(
  x,
  effects = c("fixed", "ran_pars"),
  report_space = c("natural", "log10"),
  ...
)
```

## Arguments

- x:

  A beezdemand_nlme object

- effects:

  Which effects to include: "fixed" (default), "ran_pars", or both

- report_space:

  Character. Reporting space for core parameters. One of \`"natural"\`
  or \`"log10"\` (default depends on \`param_space\` used for fitting).

- ...:

  Additional arguments (ignored)

## Value

A tibble of model coefficients with columns: - \`term\`: Parameter
name - \`estimate\`: Point estimate - \`std.error\`: Standard error -
\`statistic\`: t-value - \`p.value\`: P-value - \`component\`: "fixed"
or "variance"
