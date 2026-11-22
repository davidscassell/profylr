\name{glance}
\alias{glance}
\title{One-row fit summary}
\description{
  One-row fit summary for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
glance(x, ...)
}
\arguments{
  \item{x}{A fitted model.}
  \item{...}{Unused. Present for the generic.}
}
\value{
  A one-row tibble of fit summaries.
}
