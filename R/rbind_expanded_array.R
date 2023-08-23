#' Bind two expanded arrays in row direction
#'
#' @param exparray1 An expanded array.
#' @param exparray2 An expanded array.
#'
#' @importFrom R6 R6Class
#' @export
#'

rbind_expanded_array <- function(exparray1, exparray2) {

  # Check if two expanded arrays can be concatenated in row direction
  stopifnot('ExpandedArray' %in% class(exparray1))
  stopifnot('ExpandedArray' %in% class(exparray2))
  stopifnot(length(exparray1$dim) == 2)
  stopifnot(length(exparray2$dim) == 2)
  stopifnot(exparray1$dim[2] == exparray2$dim[2])
  stopifnot(identical(exparray1$diminfo[[2]], exparray2$diminfo[[2]]))

  # Stop if info of row direction is expanded array
  if ('ExpandedArray' %in% class(exparray1$diminfo[[1]])) {
    stop('diminfo should not be a expanded array.')
  }
  if ('ExpandedArray' %in% class(exparray2$diminfo[[1]])) {
    stop('diminfo should not be a expanded array.')
  }

  # Stop if either of info of row direction is a data frame
  # and the other is not
  if ('data.frame' %in% class(exparray1$diminfo[[1]])) {
    if (!'data.frame' %in% class(exparray2$diminfo[[1]])) {
      stop('both of diminfo should be data frames.')
    }
  }
  if ('data.frame' %in% class(exparray2$diminfo[[1]])) {
    if (!'data.frame' %in% class(exparray1$diminfo[[1]])) {
      stop('both of diminfo should be data frames.')
    }
  }

  # Combine array
  newarray <- rbind(exparray1$array, exparray2$array)

  # Combine column information
  info_col <- exparray1$diminfo[[2]]

  # Combine row information
  info_row <- if ('data.frame' %in% class(exparray1$diminfo[[1]])) {
    rbind(exparray1$diminfo[[1]], exparray2$diminfo[[1]])
  } else {
    c(exparray1$diminfo[[1]], exparray2$diminfo[[1]])
  }

  # create a merged expanded array
  newexparr <- expanded_array(newarray, list(info_row, info_col))
  return(newexparr)

}

# a <- expanded_array(
#   matrix(1:12, ncol = 3),
#   list(data.frame(A = 1:4, B = 11:14),
#        data.frame(X = 3:1, Y = 13:11)))
#
# b <- expanded_array(
#   matrix(1:21, ncol = 3),
#   list(data.frame(A = 101:107, B = 111:117),
#        data.frame(X = 3:1, Y = 13:11)))
#
# a
# b
#
# x <- rbind_expanded_array(a, b)
# x
