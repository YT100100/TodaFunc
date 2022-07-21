#' Replicate a data frame to row direction
#'
#' @param df Data frame.
#' @param n Number of replications.
#'
#' @export

rep_data_frame <- function(df, n) as.data.frame(lapply(df, rep, n))
