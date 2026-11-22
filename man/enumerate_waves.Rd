\name{enumerate_waves}
\alias{enumerate_waves}
\title{enumerate waves}
\description{
  enumerate waves for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
enumerate_waves(...)
}
\arguments{
  \item{...}{Arguments passed to the per-wave grid.}
}
\value{
  An object of class \code{profylr_wave_enumeration}.
  The per-wave tables do not choose the joint number of states.
}
