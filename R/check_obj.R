

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


tail.mat <- function (mat, n.row = 6, n.col = 6) {

  # If list is given, first element will be chosen
  while (class(mat) == 'list') {
    warning("List object was given. Only the first element was shown.")
    mat <- mat[[1]]
  }

  # if vector is given, use common head() function
  if (is.null(dim(mat))) {
    return(tail(mat))
  }

  # show the first 6 columns and rows
  disp.row <- pmin(nrow(mat), n.row)
  disp.col <- pmin(ncol(mat), n.col)
  ans <- mat[(nrow(mat) - disp.row + 1):nrow(mat), 1:disp.col]
  return(ans)

}

check_dat <- function(obj) {

  cat('Class:', obj, '\n')

  if ('list' %in% class(obj)) {

    return(str(a, max.level = 1))

  } else if ('array' %in% class(obj)) {

    cat('Show head of first two axes: obj[1:4, 1:4, 1, 1, ...]')
    nr <- pmin(4, dim(obj)[[1]])
    nc <- pmin(4, dim(obj)[[2]])
    return(obj[nr, nc, ])

  }

}

