#' Calculate Ticket Numbers
#'
#' This function calculates the minimum variance of ticket numbers
#' based on given parameters.
#'
#' @param N The total number of tickets. Default is 200.
#' @param p The probability parameter. Default is 0.95.
#' @param gamma The gamma parameter. Default is 0.02.
#' @return A list containing calculated ticket numbers and other related information.
#' @export
#' @examples
#' ntickets()
#' @importFrom stats pbinom pnorm which.min
#' @importFrom graphics plot curve



ntickets <- function(
    N= 200,
    p=0.95,
    gamma = 0.02){

  nd = seq(0.9*N, 1.3*N,1)

  vard = 1-gamma-pbinom(N,nd,p,lower.tail = TRUE)


  n_at_min_vard <- nd[which.min(abs(vard))]

  plot(nd, abs(vard), type = "b", col = "red", xlab = "n", ylab = "Probability", main = "Gamma and Var vs n")


  #-----------------------------------------------------------
  nc = seq(0.9*N, 1.3*N,.00001)
  varn = 1-gamma-pnorm(N+0.5,nc*p,sqrt(nc*p*(1-p)),lower.tail = TRUE)

  #n_at_min_varn <- which.min(abs(varn))

  #p = curve(1 - gamma - pnorm(N + 0.5, x*p, sqrt(x*p*(1 - p)), lower.tail = TRUE), from = 180, to = 250, xlab = "n", ylab = "Probability", main = "Gamma and Var vs n")
  expression_values <- 1 - gamma - pnorm(N + 0.5, nc*p, sqrt(nc*p*(1 - p)), lower.tail = TRUE)

  cv = curve(1 - gamma - pnorm(N + 0.5, x*p, sqrt(x*p*(1 - p)), lower.tail = TRUE), from = 0.9*N, to = 1.3*N, xlab = "n", ylab = "Probability", main = "Gamma and Var vs n")

  # Finding the minimum value and corresponding x
  n_at_min_varn <- nc[which.min(abs(expression_values))]

  list(n_at_min_varn = n_at_min_varn,
       n_at_min_vard = nd[which.min(abs(vard))],
       nd = nd[1:20],
       nc = nc[1:20],
       N = N,
       p = p,
       gamma = gamma)

}
