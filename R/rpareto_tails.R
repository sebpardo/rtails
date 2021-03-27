#' Random generation of Pareto deviations
#'
#' Random generation for the Pareto distribution, centered around a mean
#' of one, with parameter `shape`.
#'
#' @param n sample size.
#' @param shape shape parameter. Default is shape = 2.
#' @param bias_correct logical. Should we bias correct.
#'   The mean to use for bias correction can only be calculated when
#'   `shape > 1`.
#' @param sample_bias_correct logical. Should we bias correct using the sample
#'   mean. Defaults to `FALSE`.If `bias_correct = TRUE` then this parameter
#'   is ignored (i.e. equals `FALSE`).
#' @param ac auto-correlation value, between -1 and 1. If `ac != 0`
#'   autocorrelation is incorporated in the vector using
#'   an AR(\emph{1}) process.
#' @param log logical. Whether to return the log-transformed distribution.
#' @param seed seed. Numeric for `set.seed()`. Defaults to NA where no seed is
#'   set.
#'
#' @importFrom EnvStats rpareto
#'
#' @examples
#' rpt <- rpareto_tails(50, 3)
#' plot_tails(rpt)
#'
#' @export
rpareto_tails <- function(n, shape = 3, bias_correct = TRUE,
                          sample_bias_correct = FALSE, ac = 0,
                          log = FALSE, seed = NA) {

  if (shape <= 0) stop("Shape parameter must be greater than zero.",
                       call. = FALSE)
  if (bias_correct && sample_bias_correct) {
    warning("Both bias_correct and sample_bias_correct set as TRUE, ",
            "ignoring 'sample_bias_correct'.", call. = FALSE)
  }

  rpar <- generate_pareto_par(shape)
  bias_corr <- 1
  if (bias_correct) bias_corr <- rpar$location

  if (!is.na(seed)) set.seed(seed)
  rp <- rpareto(n, bias_corr, shape)


  if (ac != 0) {
    rp <- acfy(rp, ac)
  }

    if (sample_bias_correct == TRUE & bias_correct == FALSE) {
    rp <- rp - mean(rp) + 1
    if(any(rp < 0)) {
      warning("Negative values produced after sample bias correction.",
              call. = FALSE)
    }
  }


  if (log) {
    return(log(rp))
  } else {
      rp
    }
}

# Function to return the location parameter such that mean of the
# distribution is 1. Also calculates the variance of the distribution
generate_pareto_par <- function(shape) {
  if(shape <= 1) {
    warning("Mean is infinite because shape <= 1.")
    mu <- Inf
    location <- NA
  } else {
    mu <- 1
    location <- mu * (shape - 1) / shape
  }

  if(shape <= 2) {
    warning("Variance is infinite because shape <= 2.")
    variance <- Inf
  } else {
    variance <- location^2 * shape / (shape - 1) / (shape - 1) / (shape - 2)
  }

  return(list(mu = mu, location = location, variance = variance))
}

