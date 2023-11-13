#' tiff() with compression
#'
#' This equals to `tiff(..., compression = 'lzw')`.
#'
#' @seealso [tiff()]
#' @export

tiff_lzw <- function (...) {

  tiff(..., compression = 'lzw')

}

