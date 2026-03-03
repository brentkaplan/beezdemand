# Format Parameter Name with Scale Prefix

Formats a canonical parameter name with the appropriate scale prefix.

## Usage

``` r
format_param_name(param, scale = c("natural", "log", "log10"))
```

## Arguments

- param:

  Character. Canonical parameter name (e.g., "alpha").

- scale:

  Character. One of "natural", "log", "log10".

## Value

Character. Formatted parameter name (e.g., "log10_alpha").
