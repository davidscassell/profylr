\name{transition_matrix}
\alias{transition_matrix}
\title{transition matrix}
\description{
  transition matrix for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
transition_matrix(fit)
}
\arguments{
  \item{fit}{A fitted model.}
}
\value{
  The transition rows from \code{tidy(fit)}.
}
