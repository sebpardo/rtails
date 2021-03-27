#' Random generation of normal mixture deviations
#'
#' Random generation for a mixture of normal distributions, both centered
#' around zero and bias corrected, as well as exponentiated.
#'
#' @param n sample size, or a vector of random deviates to update if
#' `replace = TRUE`.
#' @param sigma variance of the underlying normal distribution, defaults
#' to 0.1. Ignored if `replace = TRUE`.
#' @param high_sigma variance of the sporadic normal distribution, should be
#' higher than `sigma`, defaults to 1.5.
#' @param rate rate at which the wider sporadic normal distribution replaces
#' the underlying distribution. This is determined by a binomial process
#' using `rbinom(1, n, rate)`.
#' @param bias_correct logical. Should we bias correct analytically.
#' @param sample_bias_correct logical. Should we bias correct using the sample
#'   mean, which results in a mean of exactly 1. Defaults to `FALSE`. If
#'   `bias_correct = TRUE` then this parameter is ignored (i.e. equals `FALSE`).
#' @param ac auto-correlation value, between -1 and 1. If `ac != 0`
#'   autocorrelation is incorporated in the vector using an AR(\emph{1})
#'   process, and is applied after exponentiating except when `log = TRUE`.
#' @param log logical. Whether to return the distribution before it is
#'   bias corrected and exponentiated. This is not equivalent to
#'   log-transforming the distribution when `log = FALSE`.
#' @param skew value of skewness parameter. When set, the skew-normal
#'  distribution from the `sn` package is used (i.e. `sn::rsn`), therefore
#'  when setting `seed` the drawn values will probably differ to those
#'  without skew.
#'  Defaults to `NULL`, where no skewness is incorporated and the
#'  traditional `rnorm` function is used.
#' @param seed seed. Numeric for `set.seed()`. Defaults to NA where no seed is
#'   set.
#' @param replace logical. Whether to replace a vector of deviates using
#' sporadic values or create it from scratch. If `TRUE`, then
#' `n` needs to be a vector of positive deviates centered around 1.
#'
#'
#' @return a vector of random deviates of length `n`. If there were sporadic
#' deviates where the `high_sigma` distribution replaced the underlying one,
#' then the positions of these events are stored in the `ht` attribute of the
#' vector.
#
#' @details Both normal distributions are bias corrected independently so that
#' their means in log-space equal to 1 (\eqn{-\sigma^2/2}).
#'
#' @importFrom stats rbinom rnorm
#' @importFrom sn rsn
#'
#' @examples
#' rnt <- rnorm_tails(500, sigma = 0.2, high_sigma = 1.5, rate = 1/19)
#' plot_tails(rnt)
#'
#' @export
rnorm_tails <- function(n, sigma = 0.1, high_sigma = 2, rate = 1/38,
                        bias_correct = TRUE, sample_bias_correct = FALSE,
                        ac = 0, log = FALSE, plus_one = FALSE,
                        skew = NULL, seed = NA, replace = FALSE) {

  if (sigma <= 0 || high_sigma <= 0) {
    stop("'sigma' and 'high_sigma' must be greater than zero.",
         call. = FALSE)
  }
  if (sigma >= high_sigma) {
  stop("'sigma' is equal or greater than 'high_sigma'.",
  "The latter has to be greater.", call. = FALSE)
  }
  if (bias_correct == TRUE & sample_bias_correct == TRUE) {
    warning("Both bias_correct and sample_bias_correct set as TRUE, ",
            "ignoring sample_bias_correct.", call. = FALSE)
  }

  bias_corr <- bias_corr_h <- 0

  if (replace) {
    if (is.na(n) || !is.double(n) || length(n) < 1) {
      stop("'n' must be a vector of deviations when replace = TRUE.",
           call. = FALSE)
      }
    if ( n == trunc(n) && length(n) == 1) {
      warning("'n' is an integer of length one, therefore",
              "'replace = TRUE' might be undesirable.", call. = FALSE)
    }
    x_exp <- n
    n <- length(x_exp)

  } else {

    if (is.na(n) || n <= 0 || n != trunc(n) || length(n) != 1) {
      stop("'n' must be a positive integer.", call. = FALSE)
    }
    if (bias_correct) bias_corr <- -0.5 * sigma^2
    if (!is.na(seed)) set.seed(seed)
    x <- rnorm(n, bias_corr, sigma)
    x_exp <- exp(x)

    if (!is.null(skew)) {
      if (!is.na(seed)) set.seed(seed)
      x <- rsn(n, bias_corr, sigma, alpha = skew)
    }
  }

  nhigh <- rbinom(1, n, rate)
  high_ind <- sample(1:n, nhigh)

  if (bias_correct) bias_corr_h <- -0.5 * high_sigma^2

  if (!is.na(seed)) set.seed(seed)
  new_samp <- rnorm(nhigh, bias_corr_h, high_sigma)

  if (!is.null(skew)) {
    if (!is.na(seed)) set.seed(seed)
    new_samp <- rsn(nhigh, bias_corr_h, high_sigma, alpha = skew)
  }

  new_samp_exp <- exp(new_samp)

  if (nhigh > 0) {
    if (!replace) attr(x, "ht") <- high_ind
    x_exp[high_ind] <- new_samp_exp
    if (!replace) x[high_ind] <- new_samp
  }

  if (sample_bias_correct == TRUE & bias_correct == FALSE) {
    if (replace) {
      stop("Cannot sample bias correct when replace = FALSE",
           "as 'n' is a positive vector centered around 1.", call. = FALSE)
    }
    x_exp <- sample_bias_corr(x)

  }

  # Autocorrelation is applied after exponentiating except when log = TRUE.
  if (ac != 0) x_exp <- acfy(x_exp, ac)
  attr(x_exp, "ht") <- high_ind

  if (log) {
    if (ac != 0) x <- acfy(x, ac)
    return(x)
  }
  x_exp
}
