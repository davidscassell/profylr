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
  \item{comparison}{Comparison table the rationale is attached to.}
  \item{chosen}{Model identifier the analyst selects.}
  \item{rationale}{Analyst's reason for the recorded choice.}
}
\value{
  A \code{profylr_selection} record of the analyst's choice.
}
