
#' Calculate the Mode of a Numeric Vector
#'
#' This function computes the statistical mode of a numeric vector.
#' It returns the value that occurs most frequently in the input.
#' If multiple values share the highest frequency, the first such
#' value (in sorted order) is returned.
#'
#' @param x A numeric vector.
#'
#' @return A numeric value representing the mode of \code{x}.
#'
#' @examples
#' mode_numeric(c(1, 2, 2, 3))
#'
#' @export
mode_numeric <- function(x) {
  freq_table <- table(x)
  mode_value <- names(freq_table)[which.max(freq_table)]
  as.numeric(mode_value)
}
