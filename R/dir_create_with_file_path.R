#' Create directory and output its path
#'
#' @param ... Arguments of `file.path()`.
#'
#' @export

dir_create_with_file_path <- function(...) {

  d <- file.path(...)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  return(d)

}
