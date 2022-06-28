#' Calculating RMSE (root mean squared error)
#'
#' @param obs A numeric vector of observed values.
#' @param fit A numeric vector of fitted values.
#'
#' @export

rmse <- function (obs, fit) {

  selector <- (!is.na(obs)) & (!is.na(fit))
  if (sum(selector) == 0) stop('No pair of data point.')
  obs <- obs[selector]
  fit <- fit[selector]

  n <- length(obs)
  if (n != length(fit)) stop('Length of two vectors are not equal.')

  sse <- sum((obs - fit) ^ 2)
  rmse <- sqrt(sse / n)
  return(rmse)

}
