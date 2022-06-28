#' Create empty matrix with dimnames
#'
#' @param data Data for array.
#' @param ... `dimnames`
#'
#' @export

empty_array <- function(..., data = NA) {

  dimnames <- list(...)
  dim <- sapply(dimnames, length)
  return(array(data = data, dim = dim, dimnames = dimnames))

}
