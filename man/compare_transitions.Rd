\name{compare_transitions}
\alias{compare_transitions}
\title{compare transitions}
\description{
  compare transitions for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
compare_transitions(equal_fit, free_fit)
}
\arguments{
  \item{equal_fit}{Fit with transitions held equal.}
  \item{free_fit}{Fit with transitions free across intervals.}
}
\value{
  The likelihood-ratio comparison of the equal and free transition fits.
}
