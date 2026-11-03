\name{report_model}
\alias{report_model}
\title{report model}
\description{
  report model for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
report_model(x, procedure = NULL)
}
\arguments{
  \item{x}{Argument \code{x}.}
  \item{procedure}{Argument \code{procedure}. Default: \code{NULL}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
