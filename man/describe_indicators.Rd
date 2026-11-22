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
  \item{data}{Data frame of observations.}
  \item{indicators}{Indicator column names.}
  \item{id}{Subject identifier column.}
  \item{wave}{Wave column. One occasion when omitted. Default: \code{NULL}.}
  \item{by_wave}{Repeat the summary on each wave. This does not fit a model.
    Default: \code{FALSE}.}
  \item{family}{Named families, or detection when omitted. Default: \code{NULL}.}
}
\value{
  A tibble of indicator summaries. \code{by_wave = TRUE} adds one
  block of rows per wave.
}
