\name{describe_missingness}
\alias{describe_missingness}
\title{describe missingness}
\description{
  describe missingness for a profylr fit. \code{n_complete} and \code{n_monotone}
  are wave-pattern counts: a wave counts as observed when it has at least one
  observed indicator, and \code{n_monotone} includes a leading run of missing
  waves. \code{n_complete_indicators} counts subjects with every indicator
  observed. \code{n_monotone_dropout} counts an observed prefix followed by a
  missing suffix. A leading-missing pattern is not monotone dropout.
  Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
describe_missingness(data, indicators, id, wave = NULL, family = NULL)
}
\arguments{
  \item{data}{Data frame of observations.}
  \item{indicators}{Indicator column names.}
  \item{id}{Subject identifier column.}
  \item{wave}{Wave column. One occasion when omitted. Default: \code{NULL}.}
  \item{family}{Named families, or detection when omitted. Default: \code{NULL}.}
}
\value{
  A list of wave-pattern counts, missingness patterns, and excluded ids.
}
