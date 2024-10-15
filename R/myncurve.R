#' The Normal CDF Plot Function
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a probability upper limit
#'
#' @return list containing mu, sigma, and P(Y <= a)
#' @export
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @examples
#' myncurve(mu = 10, sigma = 5, a = 4)
myncurve <- function(mu, sigma, a) {
  # Declare x variable
  x <- NULL

  # Plot Density:
  curve(dnorm(x, mean = mu, sd = sigma), # Normal distribution density function
        xlim = c(mu - 3 * sigma, mu + 3 * sigma), # xlim adjusted to include mu +- 3 * sigma
        xlab = "Y", # x-axis label
        ylab = "Density", # y-axis label
        main = paste0("Normal Distribution, mean = ", mu, ", sd = ", sigma, ", P(Y <= ", a, ")")) # Plot title

  # Plot Region:
  ll <- mu - 3 * sigma # Lower limit
  ul <- a # Upper limit
  xcurve <- seq(ll, ul, length = 3000) # Vector for x curve, 3000 coordinates
  ycurve <- dnorm(xcurve, mu, sigma) # Vector for y curve
  polygon(c(ll, xcurve, ul), # x coordinates
          c(0, ycurve, 0), # y coordinates
          col = "red") # Color

  # Return list with mu, sigma, and rounded probability:
  list(mu = mu, # mean
       sigma = sigma, # sd
       area = round(pnorm(a, mu, sigma), 4)) # P(Y <= a)
}
