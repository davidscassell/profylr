\name{record_selection}
\alias{record_selection}
\title{record selection}
\description{
  record selection for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
record_selection(comparison, chosen, rationale)
}
\arguments{
  \item{comparison}{Argument \code{comparison}.}
  \item{chosen}{Argument \code{chosen}.}
  \item{rationale}{Argument \code{rationale}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
