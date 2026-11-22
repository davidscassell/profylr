\name{information_criteria}
\alias{information_criteria}
\title{information criteria}
\description{
  information criteria for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
information_criteria(fit)
}
\arguments{
  \item{fit}{A fitted model.}
}
\value{
  A named list of information criteria, sample size, and entropy.
}
