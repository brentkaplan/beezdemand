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

## Examples

``` r
if (FALSE) { # \dontrun{
format_param_name("alpha", "log10")
# Returns: "log10_alpha"
format_param_name("q0", "natural")
# Returns: "natural_q0"
} # }
```
