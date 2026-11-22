\name{fiml_moments}
\alias{fiml_moments}
\title{fiml moments}
\description{
  fiml moments for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
fiml_moments(data, indicators, id = NULL, wave = NULL,
             covariance = c("VVV", "EEI"), family = NULL)
}
\arguments{
  \item{data}{Data frame of observations.}
  \item{indicators}{Indicator column names.}
  \item{id}{Subject identifier column. Default: \code{NULL}.}
  \item{wave}{Wave column. One occasion when omitted. Default: \code{NULL}.}
  \item{covariance}{Gaussian covariance structure. Default: \code{c("VVV", "EEI")}.}
  \item{family}{Named families, or detection when omitted. Default: \code{NULL}.}
}
\value{
  A list with \code{mean}, \code{cov}, and \code{cor}.
}
