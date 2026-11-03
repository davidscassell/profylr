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
  \item{equal_fit}{Argument \code{equal_fit}.}
  \item{free_fit}{Argument \code{free_fit}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
