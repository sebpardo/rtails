# add autocorrelation (AC1)
acfy <- function(d, ac) {
  nd <- length(d)
  d_ac <- numeric(length = nd)
  d_ac[1] <- d[1]
  for (i in 2:nd) {
    d_ac[i] <- ac * d_ac[i - 1] + d[i]
  }
  d_ac
}
