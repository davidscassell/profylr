\name{standardize_indicators}
\alias{standardize_indicators}
\title{standardize indicators}
\description{
  standardize indicators for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
standardize_indicators(data, indicators, id, wave = NULL, scope = c("pooled", "within_wave"))
}
\arguments{
  \item{data}{Argument \code{data}.}
  \item{indicators}{Argument \code{indicators}.}
  \item{id}{Argument \code{id}.}
  \item{wave}{Argument \code{wave}. Default: \code{NULL}.}
  \item{scope}{Argument \code{scope}. Default: \code{c("pooled", "within_wave")}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
