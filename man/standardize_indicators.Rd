\name{standardize_indicators}
\alias{standardize_indicators}
\title{standardize indicators}
\description{
  standardize indicators for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
standardize_indicators(data, indicators, id, wave = NULL,
                       scope = c("pooled", "within_wave"))
}
\arguments{
  \item{data}{Data frame of observations.}
  \item{indicators}{Indicator column names.}
  \item{id}{Subject identifier column.}
  \item{wave}{Wave column. One occasion when omitted. Default: \code{NULL}.}
  \item{scope}{Standardize across the sample or within each wave.
    Default: \code{c("pooled", "within_wave")}.}
}
\value{
  The data frame with Gaussian indicators replaced by their
  standardized values. Center and scale are attributes.
}
