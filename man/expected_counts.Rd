\name{expected_counts}
\alias{expected_counts}
\title{expected counts}
\description{
  expected counts for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
expected_counts(fit)
}
\arguments{
  \item{fit}{A fitted model.}
}
\value{
  A data frame of expected state membership and, when the fit has
  more than one wave, expected transition counts.
}
