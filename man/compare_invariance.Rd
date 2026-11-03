\name{compare_invariance}
\alias{compare_invariance}
\title{compare invariance}
\description{
  compare invariance for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
compare_invariance(restricted, unrestricted)
}
\arguments{
  \item{restricted}{Argument \code{restricted}.}
  \item{unrestricted}{Argument \code{unrestricted}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
