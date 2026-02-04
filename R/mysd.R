#' My Standard Deviation calculator
#'
#' @param x numeric vector
#' @param na.rm logical, remove NA values from vector
#' @importFrom stats var
#'
#' @returns standard deviation as float
#' @export
#'
#' @examples
#' mysd(c(1,2,3,4,5))
mysd <- function(x,na.rm = TRUE){
  if(na.rm){
    x <- x[!is.na(x)]
  }
  sqrt(var(x))
}
