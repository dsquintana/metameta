#' Calculate standard error based on confidence intervals
#'
#' @param lower A lower confidence interval bound
#' @param upper An upper confidence interval bound
#' @return A standard error value
#' @examples
#' ci_to_se(-0.12, 1.71)
#' ci_to_se(0.07, 1.12)


ci_to_se <- function(lower, upper) {
  se <- ((upper) - (lower)) / (2 * 1.96)
}
