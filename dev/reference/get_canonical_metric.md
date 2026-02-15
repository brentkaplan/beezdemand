# Get Canonical Derived Metric Name

Maps legacy derived metric names to canonical names.

## Usage

``` r
get_canonical_metric(name)
```

## Arguments

- name:

  Character. Input metric name.

## Value

Character. Canonical metric name or original if no mapping found.

## Examples

``` r
if (FALSE) { # \dontrun{
get_canonical_metric("Pmaxd")
# Returns: "pmax_model"
get_canonical_metric("Pmaxe")
# Returns: "pmax_obs"
} # }
```
