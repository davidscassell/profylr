\name{fit_lca}
\alias{fit_lca}
\title{Fit a latent class model}
\description{
  Fit a latent class model for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
fit_lca(data, indicators, id = NULL, wave = NULL, n_states = 2L, family = NULL, ...)
}
\arguments{
  \item{data}{Data frame of observations.}
  \item{indicators}{Indicator column names.}
  \item{id}{Subject identifier column. One row per subject when omitted.
    Default: \code{NULL}.}
  \item{wave}{Wave column. One occasion when omitted. Default: \code{NULL}.}
  \item{n_states}{Number of latent states. Default: \code{2L}.}
  \item{family}{Named families, or detection when omitted. Default: \code{NULL}.}
  \item{...}{Further specification arguments, including the start schedule.}
}
\value{
  An object of class \code{profylr_fit}.
}
\examples{
set.seed(1)
y <- data.frame(y = c(rep(0, 15), rep(1, 15)))
fit <- fit_lca(y, "y", n_states = 2, n_starts = 4, screen_iter = 2,
               n_full = 2, max_iter = 20, seed = 1, n_threads = 1)
glance(fit)
}
