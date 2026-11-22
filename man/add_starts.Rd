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
  \item{fit}{A fitted model.}
  \item{n_new}{Number of additional starts.}
  \item{seed}{Random seed. Default: \code{1}.}
}
\value{
  A \code{profylr_fit} that includes the additional starts.
}
