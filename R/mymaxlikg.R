#' The Log Likelihood Maximization Function
#'
#' @param lfun log likelihood function of theta, commonly represented as l(theta)
#' @param theta sequence of theta values
#'
#' @return theta estimate to maximize log likelihood and hence likelihood
#' @export
#' @importFrom graphics axis
#'
#' @examples
#' mymaxlikg(lfun=function(p){log(dbinom(2, 6, p)*dbinom(4, 10, p))}, theta=seq(0, 1, length = 10000))
mymaxlikg <- function(lfun, theta) {
  nth <- length(theta) # nu. of values used in theta
  thmat <- matrix(theta, nrow = nth, ncol = 1, byrow = TRUE) # Matrix of theta
  z <- apply(thmat, 1, lfun) # z holds the log lik values
  zmax <- max(which(z == max(z))) # finding the INDEX of the max lik
  plot(theta, exp(z), type = "l") # plot of lik
  abline(v = theta[zmax], col = "blue") # vertical line through max
  axis(3, theta[zmax], round(theta[zmax], 4)) # one tick on the third axis
  theta[zmax] # theta corresponding to max lik
}
