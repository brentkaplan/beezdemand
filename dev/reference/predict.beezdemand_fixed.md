# Predict Method for beezdemand_fixed

Predict Method for beezdemand_fixed

## Usage

``` r
# S3 method for class 'beezdemand_fixed'
predict(
  object,
  newdata = NULL,
  type = c("response", "link"),
  se.fit = FALSE,
  interval = c("none", "confidence"),
  level = 0.95,
  ...
)
```

## Arguments

- object:

  A \`beezdemand_fixed\` object.

- newdata:

  A data frame containing a price column matching the fitted object's
  \`x_var\`. If \`NULL\`, uses the unique observed prices when
  available.

- type:

  One of \`"response"\` (default) or \`"link"\`.

- se.fit:

  Logical; if \`TRUE\`, includes a \`.se.fit\` column (currently \`NA\`
  because vcov is not available from legacy fixed fits).

- interval:

  One of \`"none"\` (default) or \`"confidence"\`. When requested,
  \`.lower\`/\`.upper\` are returned as \`NA\` because vcov is
  unavailable.

- level:

  Confidence level when \`interval = "confidence"\`. Currently used only
  for validation.

- ...:

  Unused.

## Value

A tibble containing the original \`newdata\` columns, plus \`.fitted\`
and, when requested, \`.se.fit\` and \`.lower\`/\`.upper\`. If
\`newdata\` does not include an id column, predictions are returned for
all subjects (cross product of \`newdata\` × subjects) unless \`k\` is
subject-specific (\`k = "ind"\`).
