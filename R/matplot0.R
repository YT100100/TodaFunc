#' Expanded matplot()
#'
#' @details
#' This function is very similar to `matplot()`,
#' but removes NA when plotting.
#'
#' @export
#'

matplot0 <- function(

  x = NULL, y, type = "l",
  lty = 1:5, lwd = 1, lend = par("lend"),
  pch = 1,
  col = 1:6, cex = 1, bg = 1,
  xlab = 'x', ylab = 'y', xlim = NULL, ylim = NULL

) {

  # generate x
  if (is.null(x)) {
    x <- 1:nrow(y)
  }

  # plot white space
  if (is.null(xlim)) xlim <- range(x, na.rm = TRUE)
  if (is.null(ylim)) ylim <- range(y, na.rm = TRUE)
  plot(0, 0, type = 'n',
       xlim = xlim, ylim = ylim,
       xlab = xlab, ylab = ylab)

  # adjust the length of graphic parameters
  n_variable <- ncol(y)
  parnames <- c('lty', 'lwd', 'lend', 'pch', 'col', 'cex', 'bg')
  for (parnames_i in parnames) {
    parval <- get(parnames_i)
    parval0 <- rep(parval, length.out = n_variable)
    assign(parnames_i, parval0)
  }

  # loop for each samples
  for (i in 1:n_variable) {

    y.i <- y[, i]

    # select data which is not NA
    selector.x <- is.na(x)
    selector.y <- is.na(y.i)
    selector <- !(selector.x | selector.y)

    # if all data is NA
    if (all(!selector)) next

    # remove NA
    x.i <- x[selector]
    y.i <- y.i[selector]

    # plot
    points(x.i, y.i, type = type,
           lty = lty[i], lwd = lwd[i], lend = lend[i],
           pch = pch[i],
           col = col[i], cex = cex[i], bg = bg[i])

  }

}

# #### test ####
#
# # create data
# N <- 100
# x <- seq(0, 1, length.out = N)
# y <- data.frame(
#   y1 = x + rnorm(N, sd = 0.1),
#   y2 = 0.5 * x + rnorm(N, sd = 0.1),
#   y3 = 0.5 + 0.5 * x + rnorm(N, sd = 0.1)
# )
# matplot(x = x, y = y, type = 'l')
#
# # create lack of data
# y[40:43, ] <- NA
# matplot(x = x, y = y, type = 'l')
#
# # parameters
# matplot0(x = x, y = y, col = c('green', 'red', 'blue'),
#          lty = c(1, 3), lwd = c(3, 2, 4))
