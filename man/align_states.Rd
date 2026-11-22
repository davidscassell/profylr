\name{align_states}
\alias{align_states}
\title{align states}
\description{
  align states for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
align_states(fit, reference)
}
\arguments{
  \item{fit}{A fitted model.}
  \item{reference}{Fit or parameter list the states are aligned to.}
}
\value{
  A list with \code{fit}, the aligned \code{profylr_fit}, and
  \code{alignment}.
}
