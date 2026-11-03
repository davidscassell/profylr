\name{correlate_indicators}
\alias{correlate_indicators}
\title{correlate indicators}
\description{
  correlate indicators for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
correlate_indicators(data, indicators, id = NULL, wave = NULL, family = NULL)
}
\arguments{
  \item{data}{Argument \code{data}.}
  \item{indicators}{Argument \code{indicators}.}
  \item{id}{Argument \code{id}. Default: \code{NULL}.}
  \item{wave}{Argument \code{wave}. Default: \code{NULL}.}
  \item{family}{Argument \code{family}. Default: \code{NULL}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
