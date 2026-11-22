\name{fit_grid}
\alias{fit_grid}
\title{fit grid}
\description{
  fit grid for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
fit_grid(data, indicators, id = NULL, wave = NULL,
         n_states = 1:3, covariance = c("EEI", "VVI", "EEE", "VVV"),
         model = c("detect", "lpa", "lca", "lta"),
         by_wave = FALSE, family = NULL, ...)
}
\arguments{
  \item{data}{Data frame of observations.}
  \item{indicators}{Indicator column names.}
  \item{id}{Subject identifier column. One row per subject when omitted.
    Default: \code{NULL}.}
  \item{wave}{Wave column. One occasion when omitted. Default: \code{NULL}.}
  \item{n_states}{Number of latent states. Default: \code{1:3}.}
  \item{covariance}{Gaussian covariance structure.
    Default: \code{c("EEI", "VVI", "EEE", "VVV")}.}
  \item{model}{Which model the grid fits.
    Default: \code{c("detect", "lpa", "lca", "lta")}.}
  \item{by_wave}{Fit each wave separately when true. Default: \code{FALSE}.}
  \item{family}{Named families, or detection when omitted. Default: \code{NULL}.}
  \item{...}{Further specification arguments, including the start schedule.}
}
\value{
  An object of class \code{profylr_grid}, with \code{fits} and
  \code{comparison}.
}
\examples{
set.seed(1)
y <- data.frame(
  y1 = c(rnorm(12, -1), rnorm(12, 1)),
  y2 = c(rnorm(12, -1), rnorm(12, 1))
)
fit_grid(y, c("y1", "y2"), n_states = 2, covariance = "EEI",
         model = "lpa", n_starts = 2, screen_iter = 0, n_full = 1,
         max_iter = 15, seed = 1, n_threads = 1)
}
