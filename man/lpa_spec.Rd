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
  \item{n_states}{Argument \code{n_states}.}
  \item{covariance}{Argument \code{covariance}. Default: \code{"VVV"}.}
  \item{...}{Argument \code{...}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
