\name{add_starts}
\alias{add_starts}
\title{add starts}
\description{
  add starts for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
add_starts(fit, n_new, seed = 1)
}
\arguments{
  \item{fit}{Argument \code{fit}.}
  \item{n_new}{Argument \code{n_new}.}
  \item{seed}{Argument \code{seed}. Default: \code{1}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
