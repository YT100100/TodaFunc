#' Bind two expanded arrays in column direction
#'
#' @param exparray1 An expanded array.
#' @param exparray2 An expanded array.
#'
#' @importFrom R6 R6Class
#' @export
#'

cbind_expanded_array <- function(exparray1, exparray2) {

  # Check if two expanded arrays can be concatenated in row direction
  stopifnot('ExpandedArray' %in% class(exparray1))
  stopifnot('ExpandedArray' %in% class(exparray2))
  stopifnot(length(exparray1$dim) == 2)
  stopifnot(length(exparray2$dim) == 2)
  stopifnot(exparray1$dim[1] == exparray2$dim[1])
  stopifnot(identical(exparray1$diminfo[[1]], exparray2$diminfo[[1]]))

  # Stop if info of row direction is expanded array
  if ('ExpandedArray' %in% class(exparray1$diminfo[[2]])) {
    stop('diminfo should not be a expanded array.')
  }
  if ('ExpandedArray' %in% class(exparray2$diminfo[[2]])) {
    stop('diminfo should not be a expanded array.')
  }

  # Stop if either of info of row direction is a data frame
  # and the other is not
  if ('data.frame' %in% class(exparray1$diminfo[[2]])) {
    if (!'data.frame' %in% class(exparray2$diminfo[[2]])) {
      stop('both of diminfo should be data frames.')
    }
  }
  if ('data.frame' %in% class(exparray2$diminfo[[2]])) {
    if (!'data.frame' %in% class(exparray1$diminfo[[2]])) {
      stop('both of diminfo should be data frames.')
    }
  }

  # Combine array
  newarray <- cbind(exparray1$array, exparray2$array)

  # Combine row information
  info_row <- exparray1$diminfo[[1]]

  # Combine column information
  info_col <- if ('data.frame' %in% class(exparray1$diminfo[[2]])) {
    rbind(exparray1$diminfo[[2]], exparray2$diminfo[[2]])
  } else {
    c(exparray1$diminfo[[2]], exparray2$diminfo[[2]])
  }

  # create a merged expanded array
  newexparr <- expanded_array(newarray, list(info_row, info_col))
  return(newexparr)

}

# a <- expanded_array(
#   matrix(1:12, nrow = 3),
#   list(data.frame(X = 3:1, Y = 13:11),
#        data.frame(A = 1:4, B = 11:14)))
#
# b <- expanded_array(
#   matrix(1:21, nrow = 3),
#   list(data.frame(X = 3:1, Y = 13:11),
#        data.frame(A = 101:107, B = 111:117)))
#
# a
# b
#
# x <- cbind_expanded_array(a, b)
# x
