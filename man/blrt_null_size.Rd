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
  \item{alpha}{Tail probability used by the bounds.}
  \item{B}{Number of bootstrap draws.}
}
\value{
  The null rejection probability \code{(floor(alpha * B) + 1) / (B + 1)}
  when every requested draw is kept.
}
