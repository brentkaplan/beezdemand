#' @importFrom lifecycle deprecated
NULL

## Lifecycle badges for exported functions

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}
