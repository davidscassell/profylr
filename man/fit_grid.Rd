\name{fit_grid}
\alias{fit_grid}
\title{fit grid}
\description{
  fit grid for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
fit_grid(data, indicators, id = NULL, wave = NULL, n_states = 1:3, covariance = c("EEI", "VVI", "EEE", "VVV"), model = c("detect", "lpa", "lca", "lta"), by_wave = FALSE, family = NULL, ...)
}
\arguments{
  \item{data}{Argument \code{data}.}
  \item{indicators}{Argument \code{indicators}.}
  \item{id}{Argument \code{id}. Default: \code{NULL}.}
  \item{wave}{Argument \code{wave}. Default: \code{NULL}.}
  \item{n_states}{Argument \code{n_states}. Default: \code{1:3}.}
  \item{covariance}{Argument \code{covariance}. Default: \code{c("EEI", "VVI", "EEE", "VVV")}.}
  \item{model}{Argument \code{model}. Default: \code{c("detect", "lpa", "lca", "lta")}.}
  \item{by_wave}{Argument \code{by_wave}. Default: \code{FALSE}.}
  \item{family}{Argument \code{family}. Default: \code{NULL}.}
  \item{...}{Argument \code{...}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
