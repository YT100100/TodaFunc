#' Bind two expanded arrays in a row direction
#'
#' @param exparr1 An expanded array
#' @param exparr2 An expanded array
#'
#' @importFrom R6 R6Class
#' @export
#'

rbind_expanded_array <- function(exparr1, exparr2) {
  bind_expanded_array(exparr1, exparr2, axis = 1)
}
