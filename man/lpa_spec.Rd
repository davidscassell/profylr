\name{lpa_spec}
\alias{lpa_spec}
\title{lpa spec}
\description{
  lpa spec for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
lpa_spec(n_states, covariance = "VVV", ...)
}
\arguments{
  \item{n_states}{Number of latent states.}
  \item{covariance}{Gaussian covariance structure. Default: \code{"VVV"}.}
  \item{...}{Further specification arguments. See lta_spec.}
}
\value{
  A latent profile specification.
}
