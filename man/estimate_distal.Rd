\name{estimate_distal}
\alias{estimate_distal}
\title{estimate distal}
\description{
  estimate distal for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
estimate_distal(name, family, timing = NULL, wave = NULL,
                from_state = NULL, to_state = NULL, method = NULL)
}
\arguments{
  \item{name}{Distal outcome name.}
  \item{family}{Required outcome family: continuous, binary, nominal,
    or ordinal.}
  \item{timing}{Initial, final, a wave, or a transition.
    Default: \code{NULL}.}
  \item{wave}{Positive wave index for a wave-specific outcome.
    Default: \code{NULL}.}
  \item{from_state}{Origin state for a transition outcome.
    Default: \code{NULL}.}
  \item{to_state}{Destination state for a transition outcome.
    Default: \code{NULL}.}
  \item{method}{Requested estimation method. Modal assignment is refused.
    Default: \code{NULL}.}
}
\value{
  No value. The call validates its inputs and then always stops.
  Distal outcomes are not available in Release 1.
}
