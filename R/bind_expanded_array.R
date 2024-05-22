#' Bind two expanded arrays
#'
#' @param exparr1 An expanded array
#' @param exparr2 An expanded array
#' @param axis An axis to bind
#'
#' @importFrom R6 R6Class
#' @export
#'

bind_expanded_array <- function(exparr1, exparr2, axis) {
  # exparr1 <- TodaFunc::expanded_array(
  #   data = array(1:24, dim = c(4, 3, 2)),
  #   diminfo = list(data.frame(a = 1:4), 11:13, 21:22))
  # exparr2 <- TodaFunc::expanded_array(
  #   data = array(101:106, dim = c(1, 3, 2)),
  #   diminfo = list(data.frame(a = 9), 11:13, 21:22))
  # axis <- 1

  # exparr1 <- TodaFunc::expanded_array(
  #   data = array(1:24, dim = c(4, 3, 2)),
  #   diminfo = list(data.frame(a = 1:4), 11:13, 21:22))
  # exparr2 <- TodaFunc::expanded_array(
  #   data = array(101:108, dim = c(4, 1, 2)),
  #   diminfo = list(data.frame(a = 1:4), 19, 21:22))
  # axis <- 2

  # exparr1 <- TodaFunc::expanded_array(
  #   data = array(1:24, dim = c(4, 3, 2)),
  #   diminfo = list(data.frame(a = 1:4), 11:13, 21:22))
  # exparr2 <- TodaFunc::expanded_array(
  #   data = array(101:112, dim = c(4, 3, 1)),
  #   diminfo = list(data.frame(a = 1:4), 11:13, 29))
  # axis <- 3

  if (is.null(exparr1)) return(exparr2)
  if (is.null(exparr2)) return(exparr1)

  # exparr1, exparr2を確認
  stopifnot('ExpandedArray' %in% class(exparr1))
  stopifnot('ExpandedArray' %in% class(exparr2))
  stopifnot(identical(length(exparr1$dim), length(exparr2$dim)))

  # axisを確認
  axis <- as.integer(axis)
  stopifnot(length(axis) == 1)
  stopifnot(axis >= 1)
  stopifnot(axis <= length(exparr1$dim))

  # diminfoが一致しているか確認
  stopifnot(identical(exparr1$diminfo[-axis], exparr2$diminfo[-axis]))

  # axis方向のdiminfoが一致しているか確認
  if ('data.frame' %in% class(exparr1$info(axis))) {
    stopifnot(identical(colnames(exparr1$info(axis)), colnames(exparr2$info(axis))))
  }

  # 新しいアレイを作成
  dim_new <- exparr1$dim
  dim_new[axis] <- dim_new[axis] + exparr2$dim[axis]
  arr_new <- array(dim = dim_new)

  # 値を入れる
  nrep <- c(prod(exparr1$dim[1:axis]), prod(exparr2$dim[1:axis]))
  is_arr1 <- rep(rep(c(TRUE, FALSE), nrep), length.out = length(arr_new))
  arr_new[ is_arr1] <- c(exparr1$array)
  arr_new[!is_arr1] <- c(exparr2$array)

  # diminfoを作成
  diminfo <- exparr1$diminfo
  if ('data.frame' %in% class(diminfo[[axis]])) {
    diminfo[[axis]] <- rbind(diminfo[[axis]], exparr2$info(axis))
  } else {
    diminfo[[axis]] <- c(diminfo[[axis]], exparr2$info(axis))
  }
  newdat <- expanded_array(arr_new, diminfo)
  return(newdat)

}
