#' legend() outside plot
#'
#' This equals to `legend(x = par()$usr[2], y = par()$usr[4], box.col = NA,
#' xjust   = 1, yjust   = 0, ...)`.
#'
#' @export

legend_outside <- function (...) {

  par(xpd = TRUE)
  legend(x = par()$usr[2],
         y = par()$usr[4],
         box.col = NA,
         xjust   = 1,
         yjust   = 0,
         ...)
  par(xpd = FALSE)

}

