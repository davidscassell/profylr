\name{augment}
\alias{augment}
\title{Subject-wave posteriors}
\description{
  Subject-wave posteriors for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
augment(x, ...)
}
\arguments{
  \item{x}{A fitted model.}
  \item{...}{Unused. Present for the generic.}
}
\value{
  A tibble with one row per person-wave and the state and posterior
  columns.
}
