#' Plot results of prcomp() and PPCA()
#'
#' @param pca_res Results of PCA (`prcomp()` or `PPCA()`).
#' @param x_pc A numeric vector of PCs to be used as x-axis.
#' @param y_pc A numeric vector of PCs to be used as y-axis. Its length should be the same as `x_pc`.
#' @param labels A character vector to be shown as hover text in plotly.
#' @param colors A character vector of groups which is reflected to point colors.
#' @param filename A character to be used in filename.
#'
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom htmlwidgets saveWidget
#'
#' @export

biplot_PCA <- function(pca_res, x_pc = 1, y_pc = 2,
                       labels = NULL, colors = NULL, filename = NULL) {

  ans <- list()

  if (class(pca_res) == 'prcomp') {
    score_mat <- pca_res$x
    r2 <- pca_res$sdev / sum(pca_res$sdev)
  } else if (class(pca_res) == 'pcaRes') {
    score_mat <- pca_res@scores
    r2 <- c(pca_res@R2cum[1], diff(pca_res@R2cum))
  } else {
    stop('Wrong class of pca_res. Only results of prcomp() or ppca() allowed.')
  }

  r2ratio <- round(r2 * 100, 2)
  if (is.null(labels)) labels <- as.character(1:nrow(score_mat))
  if (is.null(colors)) colors <- rep('a', nrow(score_mat))
  labels <- as.character(labels)
  collist <- rainbow(length(unique(colors)))

  for (i in 1:length(x_pc)) {

    x_pc0 <- x_pc[i]
    y_pc0 <- y_pc[i]
    x <- score_mat[, x_pc0]
    y <- score_mat[, y_pc0]
    xlab <- paste0('PC', x_pc0, ' (', r2ratio[x_pc0], '%)')
    ylab <- paste0('PC', y_pc0, ' (', r2ratio[y_pc0], '%)')

    p <- plotly::plot_ly(
      type = 'scatter', mode = 'markers', colors = collist
    )
    p <- plotly::add_trace(
      p, x = ~x, y = ~y, color = ~colors,
      marker = list(line = list(color = 'black', width = 1)),
      text = labels, hoverinfo = 'text'
    )
    p <- plotly::layout(
      p, title = paste0('PC', x_pc0, ' vs. PC', y_pc0),
      xaxis = list(title = xlab), yaxis = list(title = ylab)
    )

    if (!is.null(filename)) {
      filename_i <- paste0(filename, 'PC', x_pc0, '-', y_pc0, '.html')
      htmlwidgets::saveWidget(p, filename_i)
    }
    ans[[i]] <- p

  }
  return(ans)

}
