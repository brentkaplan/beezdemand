# Summary method for beezdemand_nlme

Returns a structured summary object containing model coefficients, fit
statistics, and random effects information.

## Usage

``` r
# S3 method for class 'beezdemand_nlme'
summary(object, report_space = c("natural", "log10"), ...)
```

## Arguments

- object:

  A beezdemand_nlme object

- report_space:

  Character. Reporting space for core parameters. One of \`"natural"\`
  or \`"log10"\` (default depends on \`param_space\` used for fitting).

- ...:

  Additional arguments (passed to summary.nlme)

## Value

A \`summary.beezdemand_nlme\` object (inherits from
\`beezdemand_summary\`) with fields including: - \`call\`: The original
function call - \`model_class\`: "beezdemand_nlme" - \`backend\`:
"nlme" - \`equation_form\`: The equation form used ("zben" or
"simplified") - \`coefficients\`: Tibble of fixed effects with
std.error, statistic, p.value - \`random_effects\`: VarCorr output for
random effects - \`logLik\`, \`AIC\`, \`BIC\`: Model fit statistics
