\name{fit_lpa}
\alias{fit_lpa}
\title{Fit a latent profile model}
\description{
  Fit a latent profile model for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
fit_lpa(data, indicators, id = NULL, n_states = 2L,
        covariance = "VVV", family = NULL, ...)
}
\arguments{
  \item{data}{Data frame of observations.}
  \item{indicators}{Indicator column names.}
  \item{id}{Subject identifier column. One row per subject when omitted.
    Default: \code{NULL}.}
  \item{n_states}{Number of latent states. Default: \code{2L}.}
  \item{covariance}{Gaussian covariance structure. Default: \code{"VVV"}.}
  \item{family}{Named families, or detection when omitted. Default: \code{NULL}.}
  \item{...}{Further specification arguments, including the start schedule.}
}
\value{
  An object of class \code{profylr_fit}.
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
