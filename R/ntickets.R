#' The Optimal Plane Tickets Function
#'
#' @param N amount of seats on plane
#' @param gamma intended probability of overbooking
#' @param p individual probability of a show
#'
#' @return a named list with optimal tickets, solved with a discrete distribution (nd) and solved with a continuous distribution (nc), and inputs N, p, and gamma
#' @export
#' @importFrom graphics abline
#' @importFrom stats pbinom qbinom qnorm uniroot
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
ntickets <- function(N, gamma, p) {
  # n = number of tickets to be sold

  ###########################################
  # Solve for nd
  ###########################################

  # Y ~ Binomial(n, p), P(Y <= N) = 1 - gamma
  f.discrete <- function(n) {
    N - qbinom(1 - gamma, size = n, prob = p) # P(Y <= N) = 1 - gamma, N = qbinom(1 - gamma, n, p)
  }

  # Optimize discrete function to find when it equals 0 to solve for n
  n.discrete <- uniroot(f.discrete, lower = N, upper = N * 2)$root # uniroot() returns named list, access root with $root (f(root) = 0)

  # Since discrete, truncate floating point. E.g., if the optimization returns 109.7, sell 109 tickets to have no more than gamma chance of overbooking
  n.discrete <- floor(n.discrete) # Floor rounds down positive numbers

  ###########################################
  # Solve for nc
  ###########################################

  # Approximation with Y ~ N(np, npq), P(Y <= N) = 1 - gamma
  f.continuous <- function(n) {
    N - qnorm(1 - gamma, # P(Y <= N) = 1 - gamma, N = qnorm(1 - gamma, mean, sd)
              mean = n * p, # mu = np
              sd = sqrt(n * p * (1 - p))) # var = sd^2 = npq
  }

  # Optimize continuous function to find when it equals 0 to solve for n
  n.continuous <- uniroot(f.continuous, lower = N, upper = N * 2)$root

  ###########################################
  # Discrete graph
  ###########################################

  # Function for finding discrete probability of overbooking, varying n
  # Y ~ Binomial(n, p)
  curve.discrete <- function(n) {
    1 - pbinom(N, size = n, prob = p) # P(Y > N) = 1 - P(Y <= N)
  }

  # Set x values for discrete plot
  x.discrete <- N:ceiling(1.1 * N) # From N to 1.1N

  # Discrete curve
  plot(x.discrete, # x
       curve.discrete(x.discrete), # y
       type = "b", # Plot type = both points and lines
       pch = 16, # Fill in points
       main = paste0("Objective Vs n to find optimal tickets sold (", n.discrete, "), gamma = ", gamma, ", N = ", N, ", discrete"), # Plot title with interpolated values
       ylab = "Objective", # x-axis label
       xlab = "n") # y-axis label

  # Add vertical line and horizontal line intersecting at (nd, f(nd))
  abline(v = n.discrete, col = "red", lwd = 2) # Vertical line at x = nc, line thickened with lwd
  abline(h = curve.discrete(n.discrete), col = "red", lwd = 2) # Horizontal line at y = f(nc), line thickened with lwd

  ###########################################
  # Continuous graph
  ###########################################

  # Function for finding continuous probability of overbooking, varying n
  # Y ~ N(np, npq)
  curve.continuous <- function(n) {
    1 - pnorm(N, mean = n * p, sd = sqrt(n * p * (1 - p))) # P(Y > N) = 1 - P(Y <= N)
  }

  # Continuous curve
  curve(curve.continuous(x), # f(x)
        from = N, # Lower limit of x
        to = 1.1 * N, # Upper limit of x
        main = paste0("Objective Vs n to find optimal tickets sold (", round(n.continuous, 4), "), gamma = ", gamma, ", N = ", N, ", continuous"), # Plot title with interpolated values
        ylab = "Objective", # x-axis label
        xlab = "n") # y-axis label

  # Add vertical line and horizontal line intersecting at (nc, f(nc))
  abline(v = n.continuous, col = "blue") # Vertical blue line at x = nc
  abline(h = curve.continuous(n.continuous), col = "blue") # Horizontal blue line at y = f(nc)

  ###########################################
  # Print named list
  ###########################################

  # Output results (and inputs) in a named list
  list(nd = n.discrete, # Optimal tickets according to binomial
       nc = n.continuous, # Optimal tickets according to normal approximation
       N = N, # Input amount of seats on plane
       p = p, # Input individual probability of a show
       gamma = gamma) # Input intended probability of overbooking
}
