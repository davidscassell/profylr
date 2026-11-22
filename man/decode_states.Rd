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
  \item{fit}{A fitted model.}
  \item{method}{Smoothed, filtered, or Viterbi path.
    Default: \code{c("smoothed", "filtered", "viterbi")}.}
}
\value{
  A data frame of decoded states. Viterbi returns the path. Filtered
  drops the smoothed columns. Smoothed returns the full augment table.
}
