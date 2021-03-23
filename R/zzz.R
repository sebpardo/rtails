# add autocorrelation (AR1)
acfy <- function(d, ac) {
  if (ac < -1 || ac > 1) stop("Autocorrelation value not between -1 and 1.", call. = FALSE)
  nd <- length(d)
  d_ac <- numeric(length = nd)
  d_ac[1] <- d[1]
  for (i in seq(2, nd)) {
    d_ac[i] <- ac * d_ac[i - 1] + d[i]
  }
  d_ac
}


# bias correct using the sample mean
# argument must be pre-exponentiation (or in log-space, depending on
# which way you look at it)
sample_bias_corr <- function(d) {
  d_exp <- exp(d)
  if (any(d_exp == Inf)) stop("Mean cannot be estimated for bias correction as draws are too heavy-tailed: at least one draw approximates 'Inf' when exponentiated. Please adjust distribution parameters accordingly.", call. = FALSE)
  bias_corr <- log(mean(d_exp))
  d_corr <- d - bias_corr
  d_corr_exp <- exp(d_corr)
  d_corr_exp
}


