\name{decode_states}
\alias{decode_states}
\title{decode states}
\description{
  decode states for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
decode_states(fit, method = c("smoothed", "filtered", "viterbi"))
}
\arguments{
  \item{fit}{Argument \code{fit}.}
  \item{method}{Argument \code{method}. Default: \code{c("smoothed", "filtered", "viterbi")}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
