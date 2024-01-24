#ddt <- read.csv(file.choose())
#' A quadratic function
#'
#' @param x a numeric vector
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' my quad(x=1:10)
myquad <- function(x){
  x^2 - 5*x + 6
}
