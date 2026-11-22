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
  \item{data}{Data frame of observations.}
  \item{indicators}{Indicator column names.}
  \item{id}{Subject identifier column. Default: \code{NULL}.}
  \item{wave}{Wave column. One occasion when omitted. Default: \code{NULL}.}
  \item{family}{Named families, or detection when omitted. Default: \code{NULL}.}
}
\value{
  A tibble of pairwise associations among the indicators.
}
