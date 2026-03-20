#' Log-Logistic Transformation (LL4-like)
#'
#' Applies a log-logistic like transformation, specifically `log_base(x^lambda + 1) / lambda`.
#' This transformation is useful for compressing data that spans several orders of
#' magnitude while handling zero values gracefully (as `x=0` yields `0`).
#' It's a variation related to the Box-Cox transformation or a generalized logarithm.
#'
#' @param x A numeric vector or scalar of non-negative values to be transformed.
#' @param lambda A positive numeric scalar, the lambda parameter of the transformation.
#'   Controls the curvature. Default is `4`.
#' @param base A positive numeric scalar, the base of the logarithm. Default is `10`.
#'
#' @return A numeric vector or scalar of the transformed values.
#'   Returns `NaN` for `x < 0` if `lambda` results in non-real numbers (e.g., even root of negative).
#'   However, the intended domain is `x >= 0`.
#'
#' @export
#' @examples
#' ll4(0)
#' ll4(1)
#' ll4(10)
#' ll4(100)
#' ll4(c(0, 1, 10, 100, 1000))
#'
#' # Using a different lambda or base
#' ll4(10, lambda = 2)
#' ll4(10, base = exp(1)) # Natural log base
ll4 <- function(x, lambda = 4, base = 10) {
  # Ensure x is non-negative for the intended use of x^lambda
  if (any(x < 0, na.rm = TRUE)) {
    warning(
      "Input 'x' contains negative values; LL4 is typically for non-negative inputs. Result may be NaN."
    )
  }
  # LL4(x) = log(x^lambda + 1) / lambda
  log(x^lambda + 1, base = base) / lambda
}

#' Inverse Log-Logistic Transformation (Inverse LL4-like)
#'
#' Applies the inverse of the `ll4` transformation.
#' Given `y = ll4(x)`, this function calculates `x = (base^(y * lambda) - 1)^(1/lambda)`.
#'
#' @param y A numeric vector or scalar of transformed values (output from `ll4`).
#' @param lambda A positive numeric scalar, the lambda parameter used in the original
#'   `ll4` transformation. Must match the one used for the forward transform. Default is `4`.
#' @param base A positive numeric scalar, the base of the logarithm used in the
#'   original `ll4` transformation. Must match. Default is `10`.
#'
#' @return A numeric vector or scalar of the original, untransformed values.
#'   Returns `0` when the intermediate quantity `base^(y * lambda) - 1` is
#'   negative (i.e., when `y < 0` for `base = 10` and even `lambda`), since
#'   consumption cannot be negative.
#'
#' @details
#' **Domain and boundary behavior.** The inverse LL4 transformation is defined
#' for `y >= 0` (when `base = 10` and `lambda = 4`). For `y < 0`, the
#' intermediate quantity `base^(y * lambda) - 1` becomes negative, and raising
#' a negative number to the fractional power `1/lambda` is undefined in real
#' arithmetic. In this case, the function returns `0` (consumption cannot be
#' negative).
#'
#' This boundary condition arises in practice when a model predicts fitted values
#' below zero on the LL4 scale --- typically for extrapolation to very high
#' prices. The mapping to zero is the natural floor because `ll4(0) = 0` and
#' the LL4 transformation is monotonically increasing on `[0, Inf)`.
#'
#' @export
#' @examples
#' original_values <- c(0, 1, 10, 100, 1000)
#' transformed_values <- ll4(original_values)
#' back_transformed_values <- ll4_inv(transformed_values)
#' print(data.frame(original_values, transformed_values, back_transformed_values))
#' all.equal(original_values, back_transformed_values) # Should be TRUE or very close
#'
#' # Negative y values are mapped to 0 (consumption floor)
#' ll4_inv(-0.5, lambda = 4, base = 10) # Returns 0
ll4_inv <- function(y, lambda = 4, base = 10) {
  val_inside_root <- (base^(y * lambda) - 1)
  result <- ifelse(
    is.na(val_inside_root), NA_real_,
    ifelse(val_inside_root >= 0, val_inside_root^(1 / lambda), 0)
  )
  result
}

#' Create an LL4-like Scale for ggplot2 Axes
#'
#' This function generates a `ggplot2` continuous scale that applies the `ll4`
#' transformation (and its inverse `ll4_inv`) to an axis. This is useful for
#' visualizing data spanning multiple orders of magnitude while handling zeros.
#'
#' @param ... Arguments passed on to `ggplot2::scale_y_continuous` or
#'   `ggplot2::scale_x_continuous` (e.g., `name`, `breaks`, `labels`).
#' @param lambda A positive numeric scalar, the lambda parameter for the `ll4`
#'   transformation. Default is `4`.
#'
#' @return A `ggplot2` scale object.
#' @seealso \code{\link{ll4}}, \code{\link{ll4_inv}}, \code{\link[scales]{trans_new}}
#' @export
#' @examples
#' \donttest{
#' if (require(ggplot2) && require(scales)) {
#'   set.seed(123)
#'   df <- data.frame(
#'     x = 1:100,
#'     y_raw = c(0, 0.1, 0.5, 1, 5, 10, 50, 100, 500, 1000, sample(1:2000, 90, replace = TRUE))
#'   )
#'
#'   # Plot with y-axis on LL4 scale
#'   ggplot(df, aes(x = x, y = y_raw)) +
#'     geom_point() +
#'     scale_ll4(name = "Y-axis (LL4 Scale)", lambda = 4) +
#'     ggtitle("Data with LL4 Transformed Y-Axis")
#'
#'   # Can also be used for x-axis by replacing scale_y_continuous in its definition
#'   # Or by creating a scale_x_ll4 variant.
#' }
#' }
scale_ll4 <- function(..., lambda = 4) {
  # Ensure scales package is available for trans_new
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop(
      "Package 'scales' is required for scale_ll4(). Please install it.",
      call. = FALSE
    )
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for scale_ll4(). Please install it.",
      call. = FALSE
    )
  }

  # Base for the ll4 transformation (default is 10)
  # If user wants to change base, they'd have to modify ll4 and ll4_inv calls here
  # For simplicity, assuming base=10 as per ll4/ll4_inv defaults
  current_base <- 10

  trans <- scales::trans_new(
    name = paste0("ll4_lambda", lambda), # Name can include lambda for clarity
    transform = function(x) ll4(x, lambda = lambda, base = current_base),
    inverse = function(x) ll4_inv(x, lambda = lambda, base = current_base),
    domain = c(0, Inf) # Intended domain for ll4
  )
  ggplot2::scale_y_continuous(..., trans = trans)
}

#' Create a Pseudo-Log LL4 Transformation Object for ggplot2
#'
#' Generates a `scales::trans` object using the `ll4` transformation.
#' This transformation object can be passed to the `trans` argument of
#' `ggplot2::scale_x_continuous` or `ggplot2::scale_y_continuous`.
#' It's designed for non-negative data and handles zero values gracefully.
#' The "pseudo" aspect is conceptual, similar to `pseudo_log_trans` in that it
#' handles a range including zero, but the transformation is `ll4`.
#'
#' @param lambda A positive numeric scalar, the lambda parameter for the `ll4`
#'   transformation. Default is `4`.
#'
#' @return A `trans` object (from the `scales` package).
#' @seealso \code{\link{ll4}}, \code{\link{ll4_inv}}, \code{\link[scales]{trans_new}}
#' @export
#' @examples
#' \donttest{
#' if (require(ggplot2) && require(scales)) {
#'   set.seed(123)
#'   df <- data.frame(
#'     x_vals = c(0, 0.01, 0.1, 1, 10, 100, 1000, NA), # Include 0 and NA
#'     y_vals = c(0, 10, 50, 100, 500, 1000, 2000, 50)
#'   )
#'
#'   # Using pseudo_ll4_trans for the y-axis
#'   ggplot(df, aes(x = x_vals, y = y_vals)) +
#'     geom_point() +
#'     scale_y_continuous(trans = pseudo_ll4_trans(lambda = 4),
#'                        name = "Y-Values (Pseudo-LL4 Scale)") +
#'     ggtitle("Y-Axis with Pseudo-LL4 Transformation")
#'
#'   # Using pseudo_ll4_trans for the x-axis
#'   ggplot(df, aes(x = x_vals, y = y_vals)) +
#'     geom_point() +
#'     scale_x_continuous(trans = pseudo_ll4_trans(lambda = 2), # Different lambda
#'                        name = "X-Values (Pseudo-LL4 Scale)") +
#'     ggtitle("X-Axis with Pseudo-LL4 Transformation")
#' }
#' }
pseudo_ll4_trans <- function(lambda = 4) {
  # Ensure scales package is available for trans_new
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop(
      "Package 'scales' is required for pseudo_ll4_trans(). Please install it.",
      call. = FALSE
    )
  }

  # Base for the ll4 transformation (default is 10)
  current_base <- 10

  scales::trans_new(
    name = paste0("pseudo_ll4_lambda", lambda),
    transform = function(x) ll4(x, lambda = lambda, base = current_base),
    inverse = function(x) ll4_inv(x, lambda = lambda, base = current_base),
    domain = c(0, Inf) # ll4 itself handles x=0 returning 0.
    # Using a small positive like 1e-8 for domain if strict log behavior was mimicked,
    # but ll4(0)=0, so domain [0, Inf) is fine.
  )
}
