head.mat <- function (mat, n.row = 6, n.col = 6) {

  # If list is given, first element will be chosen
  while (class(mat) == 'list') {
    warning("List object was given. Only the first element was shown.")
    mat <- mat[[1]]
  }

  # if vector is given, use common head() function
  if (is.null(dim(mat))) {
    return(head(mat))
  }

  # show the first 6 columns and rows
  disp.row <- pmin(nrow(mat), n.row)
  disp.col <- pmin(ncol(mat), n.col)
  ans <- mat[1:disp.row, 1:disp.col]
  return(ans)

}
