rmse <- function (obs, fit) {

  selector <- (!is.na(obs)) & (!is.na(fit))
  if (sum(selector) == 0) stop('No points available to plot.')

  n <- length(obs, fit)
  sse <- sum((obs - fit) ^ 2)
  rmse <- sqrt(sse / n)
  return(rmse)

}
