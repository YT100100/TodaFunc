#' Bind two expanded arrays in a column direction
#'
#' @param exparr1 An expanded array
#' @param exparr2 An expanded array
#'
#' @importFrom R6 R6Class
#' @export
#'

cbind_expanded_array <- function(exparr1, exparr2) {
  bind_expanded_array(exparr1, exparr2, axis = 2)
}
