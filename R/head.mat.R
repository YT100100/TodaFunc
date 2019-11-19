head.mat <- function (mat, n.row = 6, n.col = 6) {
  disp.row <- pmin(nrow(mat), n.row)
  disp.col <- pmin(ncol(mat), n.col)
  mat[1:disp.row, 1:disp.col]
}
