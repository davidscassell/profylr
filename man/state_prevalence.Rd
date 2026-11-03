\name{state_prevalence}
\alias{state_prevalence}
\title{state prevalence}
\description{
  state prevalence for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
state_prevalence(fit, kind = c("model", "posterior", "hard"))
}
\arguments{
  \item{fit}{Argument \code{fit}.}
  \item{kind}{Argument \code{kind}. Default: \code{c("model", "posterior", "hard")}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
