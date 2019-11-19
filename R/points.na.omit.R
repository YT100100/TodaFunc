points.na.omit <- function(x, y, ...) {

  selector <- (!is.na(x)) & (!is.na(y))
  if (sum(selector) == 0) stop('No points available to plot.')

  x0 <- x[selector]
  y0 <- y[selector]
  points(x0, y0, ...)

}
