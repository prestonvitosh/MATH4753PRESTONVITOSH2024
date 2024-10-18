#' The CLT Normal from Uniform Function
#'
#' @param n size of each sample
#' @param iter number of samples
#' @param a lower limit of uniform
#' @param b upper limit of uniform
#'
#' @return the vector of sample means
#' @export
#' @importFrom graphics hist lines
#' @importFrom stats density dunif runif
#'
#' @examples
#' mycltu(n = 20, iter = 10000, a = 0, b = 10)
mycltu <- function(n, iter, a = 0, b = 10) {
  ## r-random sample from the uniform
  y <- runif(n * iter, a, b)

  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  ## Apply the function mean to the columns (2) of the matrix
  ## These are placed in a vector w
  w <- apply(data, 2, mean)

  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param <- hist(w, plot = FALSE)

  ## Since the histogram will be a density plot we will find the max density
  ymax <- max(param$density)

  ## To be on the safe side we will add 10% more to this
  ymax <- 1.1 * ymax

  ## Now we can make the histogram
  hist(w, freq = FALSE, ylim = c(0, ymax), main = paste("Histogram of Sample Mean\nSample Size = ", n), xlab = "Sample Mean")

  ## Add a density curve made from the sample distribution
  lines(density(w), col = "blue", lwd = 3) # add a density plot

  ## Add a theoretical normal curve
  x <- NULL # Declare x variable
  curve(dnorm(x, mean = (a + b) / 2, sd = (b - a) / (sqrt(12 * n))), add = TRUE, col = "red", lty = 2, lwd = 3) # add a theoretical curve

  ## Add the density from which the samples were taken
  curve(dunif(x, a, b), add = TRUE, lwd = 4)

  ## Return the vector of sample means
  w
}
