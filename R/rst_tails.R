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
#'   but is subtracted from the distribution in normal space,
#'    which is then re-exponentiated. Therefore, the
#'   bias-correction produces a distribution with a mean of exactly 1.
#' @param ac auto-correlation value, between -1 and 1. If `ac != 0` then
#' autocorrelation is incorporated in the vector using an AR(\emph{1}) process.
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
#' @importFrom stats rt
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

  if (is.na(n) || n <= 0 || n != trunc(n) || length(n) != 1) {
    stop("'n' must be a positive integer.", call. = FALSE)
  }

  if (df <= 0) stop("Degrees of freedom parameter must be greater than zero.",
                    call. = FALSE)

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
    return(sample_bias_corr(x))
  } else {
    x_exp
  }
}
