\name{describe_indicators}
\alias{describe_indicators}
\title{describe indicators}
\description{
  describe indicators for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
describe_indicators(data, indicators, id, wave = NULL, by_wave = FALSE, family = NULL)
}
\arguments{
  \item{data}{Argument \code{data}.}
  \item{indicators}{Argument \code{indicators}.}
  \item{id}{Argument \code{id}.}
  \item{wave}{Argument \code{wave}. Default: \code{NULL}.}
  \item{by_wave}{Argument \code{by_wave}. Default: \code{FALSE}.}
  \item{family}{Argument \code{family}. Default: \code{NULL}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
