#' Random generation of Student-t deviations
#'
#' Random generation for the Student-t distribution, centered around zero,
#' exponentiated, with `df` degrees of freedom.
#'
#' @param n sample size.
#' @param df degrees of freedom (> 0). Default is df = 10. df = -Inf is
#' allowed if bias.correct = FALSE, where it becomes equivalent to the standard
#' normal distribution.
#' @param bias.correct Logical. Should we bias correct the mean using
#' the log-normal distribution correction factor (\eqn{-\sigma^2/2}).
#' @return a vector of random deviates of length `n`.
#'
#' @importFrom LaplacesDemon rst
#'
#' @examples
#' rst <- rst_tails(50, 3)
#' plot_tails(rst)
#'
#' @export
rst_tails <- function(n, df = 10, bias.correct = FALSE) {
  ncp <- 0
  if (bias.correct) {
    ncp <- -0.5
    exp(LaplacesDemon::rst(n, mu = ncp, nu = df))
  } else {
  exp(rt(n, df = df))
    }
}

rst <- rst_tails(50, df = 100000, bias.correct = TRUE)
plot_tails(rst)
