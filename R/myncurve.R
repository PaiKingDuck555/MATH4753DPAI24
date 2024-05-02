#' Title
#'
#' @param mu this is also just a fake description
#' @param sigma this is also just a fake description
#' @param a this is also just a fake description
#'
#' @return graph with a curve
#' @export
#'
#' @examples
#' @importFrom stats dnorm pnorm
#' @importFrom graphics curve polygon


myncurve = function(mu, sigma,a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma))

  list(mu = mu, sigma = sigma)

  # Define the range of the area to highlight
  xcurve <- seq(-1000,a, length = 1000) # Length defines the precision (1000 is standard practice)

  # Calculate the density of the normal distribution for this range
  ycurve <- dnorm(xcurve, mu,sigma)

  polygon(c(-1000, xcurve, a), c(0, ycurve, 0), col = "red")

  # Calculate the Area
  area = pnorm(a, mu, sigma) - pnorm(-1000, mu, sigma)
  area

}
