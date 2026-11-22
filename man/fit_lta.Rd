\name{fit_lta}
\alias{fit_lta}
\title{Fit a latent transition model}
\description{
  Fit a latent transition model for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
fit_lta(data, indicators, id, wave, n_states = 2L, family = NULL, covariance = "VVV", ...)
}
\arguments{
  \item{data}{Data frame of observations.}
  \item{indicators}{Indicator column names.}
  \item{id}{Subject identifier column.}
  \item{wave}{Required wave column.}
  \item{n_states}{Number of latent states. Default: \code{2L}.}
  \item{family}{Named families, or detection when omitted. Default: \code{NULL}.}
  \item{covariance}{Gaussian covariance structure. Default: \code{"VVV"}.}
  \item{...}{Further specification arguments, including the start schedule.}
}
\value{
  An object of class \code{profylr_fit}.
}
\examples{
set.seed(1)
y <- data.frame(
  id = rep(1:12, each = 2),
  wave = rep(1:2, times = 12),
  y1 = c(rnorm(12, -1), rnorm(12, 1)),
  y2 = c(rnorm(12, -1), rnorm(12, 1))
)
fit <- fit_lta(y, c("y1", "y2"), id = "id", wave = "wave",
               n_states = 2, covariance = "EEI", n_starts = 4,
               screen_iter = 2, n_full = 2, max_iter = 20,
               seed = 1, n_threads = 1)
glance(fit)
}
