\name{fit_lpa}
\alias{fit_lpa}
\title{Fit a latent profile model}
\description{
  Fit a latent profile model for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
fit_lpa(data, indicators, id = NULL, n_states = 2L, covariance = "VVV", family = NULL, ...)
}
\arguments{
  \item{data}{Argument \code{data}.}
  \item{indicators}{Argument \code{indicators}.}
  \item{id}{Argument \code{id}. Default: \code{NULL}.}
  \item{n_states}{Argument \code{n_states}. Default: \code{2L}.}
  \item{covariance}{Argument \code{covariance}. Default: \code{"VVV"}.}
  \item{family}{Argument \code{family}. Default: \code{NULL}.}
  \item{...}{Argument \code{...}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
\examples{
set.seed(1)
y <- data.frame(
  y1 = c(rnorm(15, -1), rnorm(15, 1)),
  y2 = c(rnorm(15, -1), rnorm(15, 1))
)
fit <- fit_lpa(y, c("y1", "y2"), n_states = 2, covariance = "EEI",
               n_starts = 4, screen_iter = 2, n_full = 2,
               max_iter = 20, seed = 1)
glance(fit)
tidy(fit)
}
