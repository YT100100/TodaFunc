tail.mat <- function (mat, n.row = 6, n.col = 6) {
  disp.row <- pmin(nrow(mat), n.row)
  disp.col <- pmin(ncol(mat), n.col)
  mat[(nrow(mat) - disp.row + 1):nrow(mat), 1:disp.col]
}
