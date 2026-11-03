\name{blrt_null_size}
\alias{blrt_null_size}
\title{blrt null size}
\description{
  blrt null size for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
blrt_null_size(alpha, B)
}
\arguments{
  \item{alpha}{Argument \code{alpha}.}
  \item{B}{Argument \code{B}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
