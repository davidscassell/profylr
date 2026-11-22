\name{plot_missingness}
\alias{plot_missingness}
\title{plot missingness}
\description{
  plot missingness for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
plot_missingness(data, indicators, id, wave = NULL, family = NULL)
}
\arguments{
  \item{data}{Data frame of observations.}
  \item{indicators}{Indicator column names.}
  \item{id}{Subject identifier column.}
  \item{wave}{Wave column. One occasion when omitted. Default: \code{NULL}.}
  \item{family}{Named families, or detection when omitted. Default: \code{NULL}.}
}
\value{
  A ggplot of missingness.
}
