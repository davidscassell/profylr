\name{fiml_moments}
\alias{fiml_moments}
\title{fiml moments}
\description{
  fiml moments for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
fiml_moments(data, indicators, id = NULL, wave = NULL, covariance = c("VVV", "EEI"), family = NULL)
}
\arguments{
  \item{data}{Argument \code{data}.}
  \item{indicators}{Argument \code{indicators}.}
  \item{id}{Argument \code{id}. Default: \code{NULL}.}
  \item{wave}{Argument \code{wave}. Default: \code{NULL}.}
  \item{covariance}{Argument \code{covariance}. Default: \code{c("VVV", "EEI")}.}
  \item{family}{Argument \code{family}. Default: \code{NULL}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
