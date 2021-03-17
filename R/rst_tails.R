#' Random generation of Student-t deviations
#'
#' Random generation for the Student-t distribution, centered around zero,
#' exponentiated, with `df` degrees of freedom.
#'
#' @param n sample size.
#' @param df degrees of freedom (> 0). Default is df = 10. df = -Inf is
#' allowed if bias.correct = FALSE, where it becomes equivalent to the standard
#' normal distribution.
#' @param bias_correct logical. Should we bias correct using the sample mean.
#'   The sample mean is calculated from exponentiated distribution,
#'   but is subtracted, which is then re-exponentiated; therefore the
#'   bias-correction should produce a mean of exactly 1.
#' @param ac auto-correlation value, between -1 and 1. If `ac != 0` autocorrelation is
#'   incorporated in the vector using an AR(\emph{1}) process.
#' @param log logical. Whether to return the distribution before it is
#'   bias corrected and exponentiated. This is not equivalent to
#'   log-transforming the distribution when `log = FALSE`.
#' @param skew value of skewness parameter. If unspecified then no
#'  skewness is incorporated. Defaults to `NULL`.
#' @param seed seed. Numeric for `set.seed()`. Defaults to NA where no seed is
#'   set.
#'
#' @return a vector of random deviates of length `n`.
#'
#' @importFrom fGarch rsstd
#'
#' @examples
#' rst <- rst_tails(50, 3)
#' plot_tails(rst)
#' mean(rst) == 1
#'
#' @export
rst_tails <- function(n, df = 10, bias_correct = TRUE, ac = 0, log = FALSE,
                      skew = NULL, seed = NA) {

  if (!is.na(seed)) set.seed(seed)

  if (!is.null(skew)) {
    x <- rsstd(n, nu = df, xi = skew)
  } else {
    x <- rt(n, df = df)
  }

  if (ac != 0) x <- acfy(x, ac)

  if (log) {
    return(x)
  }

  x_exp <- exp(x)

  if (bias_correct) {
    bias_corr <- log(mean(x_exp))
    x_corr <- x - bias_corr
    x_corr_exp <- exp(x_corr)
    x_corr_exp
  } else {
    x_exp
  }
}
