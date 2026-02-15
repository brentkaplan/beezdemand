# Check for Unsystematic Patterns in Cross-Price Data

Analyzes whether consumption data shows systematic trends or
unsystematic patterns ("bounces") with respect to price. Includes
detection of zero-value reversal/return sequences and allows flexible
output based on the level of detail requested. See Rzeszutek et al. (in
press) for more details.

## Usage

``` r
check_unsystematic_cp(
  data,
  delta_threshold = 0.025,
  bounce_down_threshold = 0.1,
  bounce_up_threshold = 0.1,
  bounce_none_threshold = 0.1,
  rev_zeroes = 2,
  ret_nums = 2,
  expected_down = FALSE,
  verbose = FALSE,
  detailed = FALSE
)
```

## Arguments

- data:

  A data frame with columns 'x' and 'y', where 'x' is price and 'y' is
  consumption.

- delta_threshold:

  Numeric. Threshold for detecting log-scale trends (default 0.025).

- bounce_down_threshold:

  Numeric. Minimum downward bounce proportion to count as significant in
  upward trends.

- bounce_up_threshold:

  Numeric. Minimum upward bounce proportion to count as significant in
  downward trends.

- bounce_none_threshold:

  Numeric. Minimum bounce proportion to count as significant in no-trend
  cases.

- rev_zeroes:

  Integer. Length of zero sequences to detect reversals (default 2).

- ret_nums:

  Integer. Length of non-zero sequences to detect returns (default 2).

- expected_down:

  Logical. If TRUE, suppress reversal detection.

- verbose:

  Logical. If TRUE, print intermediate values (default FALSE).

- detailed:

  Logical. If TRUE, return additional columns including all trend/bounce
  flags.

## Value

A data frame of class \`cp_unsystematic\` with core results:

- delta_direction:

  Character: 'down', 'up', or 'none'.

- bounce_direction:

  Character: 'up', 'down', 'significant', or 'none'.

- bounce_any:

  Logical. TRUE if any bounce pattern detected.

- bounce_above:

  Integer. Number of upward changes meeting threshold.

- bounce_below:

  Integer. Number of downward changes meeting threshold.

- reversals:

  Integer. Detected reversals from 0 to non-0.

- returns:

  Integer. Detected returns from non-0 to 0.

If \`detailed = TRUE\`, returns additional columns:

- delta_down:

  Logical. Significant downward trend.

- delta_up:

  Logical. Significant upward trend.

- delta_none:

  Logical. No significant trend.

- bounce_up:

  Logical. Significant bounce up in a downward trend.

- bounce_down:

  Logical. Significant bounce down in an upward trend.

- bounce_none:

  Logical. Significant bounces in no-trend data.

## Examples

``` r
x_seq <- 10^(seq(-2, 2, length.out = 10))
pattern <- data.frame(x = x_seq, y = c(10, 5, 10, 9, 10, 13, 10, 10, 7, 9))
check_unsystematic_cp(pattern)
#>   delta_direction bounce_direction bounce_any bounce_above bounce_below
#> 1            none      significant       TRUE            2            3
#>   reversals returns
#> 1         0       0
```
