# Get Canonical Parameter Name

Maps legacy or variant parameter names to canonical names.

## Usage

``` r
get_canonical_param(name)
```

## Arguments

- name:

  Character. Input parameter name.

## Value

Character. Canonical parameter name or original if no mapping found.

## Examples

``` r
if (FALSE) { # \dontrun{
get_canonical_param("Q0d")
# Returns: "q0"
get_canonical_param("Alpha")
# Returns: "alpha"
} # }
```
