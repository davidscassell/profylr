\name{prepare_panel}
\alias{prepare_panel}
\title{Build a numeric panel}
\description{
  Build a numeric panel for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
prepare_panel(data, indicators, id, wave = NULL, family = NULL,
              schedule = NULL, schema = NULL)
}
\arguments{
  \item{data}{Argument \code{data}.}
  \item{indicators}{Argument \code{indicators}.}
  \item{id}{Argument \code{id}.}
  \item{wave}{Argument \code{wave}. Default: \code{NULL}.}
  \item{family}{Argument \code{family}. Default: \code{NULL}.}
  \item{schedule}{Argument \code{schedule}. Default: \code{NULL}.}
  \item{schema}{Argument \code{schema}. Default: \code{NULL}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
