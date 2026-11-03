\name{align_states}
\alias{align_states}
\title{align states}
\description{
  align states for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
align_states(fit, reference)
}
\arguments{
  \item{fit}{Argument \code{fit}.}
  \item{reference}{Argument \code{reference}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
