\name{posterior_states}
\alias{posterior_states}
\title{posterior states}
\description{
  posterior states for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
posterior_states(fit, kind = c("smoothed", "filtered", "modal"))
}
\arguments{
  \item{fit}{Argument \code{fit}.}
  \item{kind}{Argument \code{kind}. Default: \code{c("smoothed", "filtered", "modal")}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
