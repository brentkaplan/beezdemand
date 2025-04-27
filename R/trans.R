ll4 <- function(x, lambda = 4, base = 10) {
  # LL4(x) = log(x^lambda + 1) / lambda
  log(x^lambda + 1, base = base) / lambda
}

ll4_inv <- function(y, lambda = 4, base = 10) {
  # Inverse: y -> x = (base^(y * lambda) - 1)^(1/lambda)
  (base^(y * lambda) - 1)^(1 / lambda)
}

scale_ll4 <- function(..., lambda = 4) {
  # Creates a scale transform for use in ggplot2
  # e.g. + scale_y_continuous(trans = "ll4") or scale_ll4()
  trans <- scales::trans_new(
    name = "ll4",
    transform = function(x) ll4(x, lambda),
    inverse = function(x) ll4_inv(x, lambda),
    domain = c(0, Inf) # only defined for nonnegative x
  )
  ggplot2::scale_y_continuous(..., trans = trans)
}

# Create a transform object similar to 'pseudo_log_trans()',
# but using LL4 for y >= 0:
pseudo_ll4_trans <- function(lambda = 4) {
  trans <- scales::trans_new(
    name = "pseudo_ll4",
    transform = function(x) ll4(x, lambda),
    inverse = function(x) ll4_inv(x, lambda),
    # Domain for LL4 is [0, Inf)
    domain = c(1e-8, Inf)
  )
}
