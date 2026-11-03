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
  \item{data}{Argument \code{data}.}
  \item{indicators}{Argument \code{indicators}.}
  \item{id}{Argument \code{id}.}
  \item{wave}{Argument \code{wave}.}
  \item{n_states}{Argument \code{n_states}. Default: \code{2L}.}
  \item{family}{Argument \code{family}. Default: \code{NULL}.}
  \item{covariance}{Argument \code{covariance}. Default: \code{"VVV"}.}
  \item{...}{Argument \code{...}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
