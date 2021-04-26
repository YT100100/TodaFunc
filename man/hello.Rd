\name{rmse}
\alias{hello}
\title{Calculating RMSE (root mean squared error )}
\usage{
rmse(obs, fit)
}
\description{
Calculation of RMSE.
}
\examples{
# creating data
x <- 1:10
y <- x + rnorm(10)
dat <- data.frame(x, y)

# fitting linear regression
res_fit <- lm(y ~ x, dat)
y_fit <- fitted(res_fit)
rmse(y, y_fit)
}
