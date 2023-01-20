#' Create an expanded array with data and diminfo
#'
#' @param data An array.
#' @param diminfo A list of information of the array.
#'
#' @importFrom R6 R6Class
#' @export
#'

expanded_array <- function(data, diminfo) {

  ExpArray <- call_expanded_array()
  ans <- ExpArray$new(data = data, diminfo = diminfo)
  return(ans)

}
