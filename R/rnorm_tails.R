#' Random generation of normal mixture deviations
#'
#' Random generation for a mixture of normal distributions, both centered
#' around zero and bias corrected, as well as exponentiated.
#'
#' @param n sample size, or a vector of random deviates to update if
#' `replace = TRUE`.
#' @param sigma variance of the underlying normal distribution, defaults
#' to 0.1. Ignored if `replace = TRUE`.
#' @param highsigma variance of the sporadic normal distribution, should be
#' higher than `sigma`, defaults to 1.5.
#' @param rate rate at which the wider sporadic normal distribution replaces
#' the underlying distribution. This is determined by a binomial process
#' using `rbinom(1, n, rate)`.
#' @param replace logical. Whether to replace a vector of deviates using
#' sporadic values or create it from scratch. If `TRUE`, then
#' `n` needs to be a vector of positive deviates centered around 1.
#' @return a vector of random deviates of length `n`. If there were sporadic
#' deviates where the `highsigma` distribution replaced the underlying one,
#' then the positions of these events are stored in the `ht` attribute of the
#' vector.
#
#' @details Both normal distributions are bias corrected independently so that
#' their means in log-space equal to 1 (\eqn{-\sigma^2/2}).
#'
#' @importFrom stats rbinom rnorm
#'
#' @examples
#' rnt <- rnorm_tails(500, sigma = 0.2, highsigma = 1.5, rate = 1/19)
#' plot_tails(rnt)
#'
#' @export
rnorm_tails <- function(n, sigma = 0.1, highsigma = 2, rate = 1/38,
                        replace = FALSE) {
  if (replace) {
    x <- n
    n <- length(x)
  } else {
    x <- exp(rnorm(n, -0.5 * sigma^2, sigma))
  }
  nhigh <- rbinom(1, n, rate)
  high_ind <- sample(1:n, nhigh)
  new_samp <- exp(rnorm(nhigh, -0.5 * highsigma^2, highsigma))
  if (nhigh > 0) {
    attr(x, "ht") <- high_ind
    x[high_ind] <- new_samp
  }
  return(x)
}
