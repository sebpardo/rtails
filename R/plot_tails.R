#' Plot heavy-tailed deviations
#'
#' A simple graphical representation of random heavy-tailed deviations created
#' with `rpareto_tails()`, `rst_tails()`, `rnorm_tails()`, or any other
#' multiplicative random deviates.
#'
#' @param vec Vector of random deviates.
#'
#' @return A scatter plot showing the deviation values in the vector.
#' Sample mean is shown by green line, and the marginal distribution is
#' shown by the rug plot on the right side of the plot.
#'
#' @details If `vec` is the output of `rnorm_tails()` where sporadic values
#' have been included, these values are marked in red.
#'
#' @importFrom ggplot2 ggplot scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 coord_cartesian aes geom_point geom_line
#' @importFrom ggplot2 geom_hline geom_rug
#'
#' @examples
#' rpt <- rpareto_tails(50, 3)
#' plot_tails(rpt)
#'
#' @export
#'

plot_tails <- function(vec) {
  n <- length(vec); mean <- mean(vec)
  mix <- rep(FALSE, n)
  if (!is.null(attr(vec, "ht"))) {
    pos_mix <- attr(vec, "ht")
    mix[pos_mix] <- TRUE
  }

  df <- data.frame(x = seq_len(n), y = vec, mix = mix)
  ggplot(df, aes(x, y)) +
    scale_x_continuous(limit = c(min(df$x), max(df$x))) +
    scale_y_continuous(limit = c(ifelse(min(df$y) < 0, min(df$y), 0), max(df$y))) +
    coord_cartesian(clip = "off") +
    geom_hline(yintercept = 0:1, color = "grey") +
    geom_line(aes(x, mean), size = 0.5, color = "darkgreen") +
    geom_point(shape = 1, color = ifelse(mix, "red", "black")) +
    geom_line(size = 0.2, color = "grey60") +
    geom_rug(size = 0.1, sides = "r",
             col=rgb(.5, 0, 0, alpha = ifelse(n > 150, 0.2, 0.7))) +
    theme_pbs()
}
