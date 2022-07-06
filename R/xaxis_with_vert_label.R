#' Add x-axis with vertical labels
#'
#' @param at Equivalent to `at` argument in `axis()` function.
#' @param labels Equivalent to `labels` argument in `axis()` function.
#' @param srt An angle for label rotation.
#' @param pos A parameter to adjust vertical positions of labels.
#' @param adj A parameter to adjust relationship between coordinates of labels
#' and actual positions of labels.
#'
#' @export

xaxis_with_vert_label <- function(
    at, labels, srt = 90, pos = 0.05, adj = c(1, 0.5)) {

  stopifnot(length(at) == length(labels))

  usr <- par()$usr
  y <- sum(usr[3:4] * (1:0 + pos * c(1, -1)))

  par(xpd = TRUE)
  axis(1, at = at, labels = rep('', length(labels)))
  text(at, y, labels, srt = srt, adj = adj)
  par(xpd = FALSE)

}
