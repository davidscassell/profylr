\name{tidy}
\alias{tidy}
\title{Parameter estimates}
\description{
  Parameter estimates for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
tidy(x, ...)
}
\arguments{
  \item{x}{A fitted model.}
  \item{...}{Unused. Present for the generic.}
}
\value{
  A tibble of parameter estimates.
}
