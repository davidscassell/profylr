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
  \item{restricted}{Fit with the restriction imposed.}
  \item{unrestricted}{Fit without that restriction.}
}
\value{
  The likelihood-ratio comparison returned by the core.
}
