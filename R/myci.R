#' The Mean Confidence Interval Function
#'
#' @param x sample from the population to find mean of
#' @param alpha probability outside the confidence interval
#' @param rnd number of decimal places to round to in output
#'
#' @return (1-alpha)100% confidence interval for population mean in vector of length 2
#' @export
#' @importFrom stats qt sd
#'
#' @examples
#' myci(x = rnorm(30, 10, 12), alpha = 0.05)
myci <- function(x, alpha = 0.05, rnd = 4) {
  ybar <- mean(x) # Sample mean
  s <- sd(x) # Sample standard deviation
  n <- length(x) # Sample size

  # t_(alpha/2):
  t <- qt(1 - alpha / 2, n - 1)

  # Lower limit:
  ll <- ybar - t * s / sqrt(n)
  ll <- round(ll, rnd)

  # Upper limit:
  ul <- ybar + t * s / sqrt(n)
  ul <- round(ul, rnd)

  # Return vector with ll and ul:
  c(ll, ul)
}
