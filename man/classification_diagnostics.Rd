\name{classification_diagnostics}
\alias{classification_diagnostics}
\title{classification diagnostics}
\description{
  classification diagnostics for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
classification_diagnostics(fit)
}
\arguments{
  \item{fit}{A fitted model.}
}
\value{
  A list of entropy, average posterior, and odds of correct
  classification.
}
