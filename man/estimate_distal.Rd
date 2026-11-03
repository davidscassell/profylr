\name{estimate_distal}
\alias{estimate_distal}
\title{estimate distal}
\description{
  estimate distal for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
estimate_distal(name, family, timing = NULL, wave = NULL, from_state = NULL, to_state = NULL, method = NULL)
}
\arguments{
  \item{name}{Argument \code{name}.}
  \item{family}{Argument \code{family}.}
  \item{timing}{Argument \code{timing}. Default: \code{NULL}.}
  \item{wave}{Argument \code{wave}. Default: \code{NULL}.}
  \item{from_state}{Argument \code{from_state}. Default: \code{NULL}.}
  \item{to_state}{Argument \code{to_state}. Default: \code{NULL}.}
  \item{method}{Argument \code{method}. Default: \code{NULL}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
