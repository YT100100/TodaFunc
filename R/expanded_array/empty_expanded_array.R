#' Create an empty expanded array with diminfo only
#'
#' @param diminfo A list of information of the array.
#'
#' @importFrom R6 R6Class
#' @export
#'

empty_expanded_array <- function(diminfo) {

  dim <- sapply(diminfo, function(di) {

    diminfo_length_i <- if ('ExpandedArray' %in% class(di)) {
      di$length()
    } else if ('data.frame' %in% class(di)) {
      nrow(di)
    } else {
      length(di)
    }

  })

  ExpArray <- call_expanded_array()
  ans <- ExpArray$new(data = array(dim = dim), diminfo = diminfo)
  return(ans)

}

empty_exparray <- empty_expanded_array
