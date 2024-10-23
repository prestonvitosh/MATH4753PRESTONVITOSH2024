#' The Bootstrap Interval Function
#'
#' @param iter number of samples for resampling
#' @param x sample to resample from
#' @param fun function to apply on resamples
#' @param alpha probability outside the intended bootstrap interval
#' @param cx character expansion number
#' @param rnd number of decimal places to round to in the graph
#' @param ... other graphical arguments for the histogram
#'
#' @return a named list with the interval, x, fun, the vector of results from fun on x, and the point estimate
#' @export
#' @importFrom graphics segments text
#' @importFrom stats quantile
#'
#' @examples
#' myboot2(iter = 10000, x = rbeta(20, 3, 4), fun = "var", alpha = 0.2, cx = 1.5, rnd = 3)
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, rnd = 2, ...) { #Notice where the ... is repeated in the code
  n <- length(x) #sample size

  y <- sample(x, n * iter, replace = TRUE)

  rs.mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun) # xstat is a vector and will have iter values in it
  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2)) # Nice way to form a confidence interval

  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para <- hist(xstat,
               freq = FALSE,
               las = 1,
               main = paste0("Histogram of Bootstrap Sample Statistics\nfun = ", fun, ", alpha = ", alpha, ", n = ", n, ", iter = ", iter),
               ...)

  # mat will be a matrix that contains the data, this is done so that I can use apply()
  mat <- matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)

  # pte is the point estimate
  # This uses whatever fun is
  pte <- apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "black") # Vertical line
  segments(ci[1], 0, ci[2], 0, lwd = 4) # Make the segment for the ci
  text(ci[1], 0, paste0("(", round(ci[1], rnd)), col = "red", cex = cx)
  text(ci[2], 0, paste0(round(ci[2], rnd), ")"), col = "red", cex = cx)

  # Plot the point estimate 1/2 way up the density
  text(pte, max(para$density) / 2, round(pte, rnd), cex = cx)

  # Some output to use if necessary
  # Adjust to include xstat and pte
  invisible(list(ci = ci,
                 fun = fun,
                 x = x,
                 xstat = xstat,
                 pte = pte))
}
