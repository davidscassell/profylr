\name{posterior_states}
\alias{posterior_states}
\title{posterior states}
\description{
  posterior states for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
posterior_states(fit, kind = c("smoothed", "filtered", "modal"))
}
\arguments{
  \item{fit}{A fitted model.}
  \item{kind}{Smoothed, filtered, or modal posterior.
    Default: \code{c("smoothed", "filtered", "modal")}.}
}
\value{
  A data frame. Modal returns the modal state. Smoothed and filtered
  return one probability row per state.
}
