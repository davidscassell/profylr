\name{posterior_transitions}
\alias{posterior_transitions}
\title{posterior transitions}
\description{
  posterior transitions for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
posterior_transitions(fit)
}
\arguments{
  \item{fit}{A fitted model.}
}
\value{
  A data frame of posterior transition probabilities for each person
  and interval.
}
