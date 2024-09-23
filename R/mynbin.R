#' The Negative Binomial Probability Function
#'
#' @param y number of trials until rth success
#' @param r number of successes to evaluate on
#' @param p probability of success in each trial
#'
#' @return probability of y trials for rth success when probability of success is p
#' @export
#'
#' @examples
#' mynbin(10,3,0.4)
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
