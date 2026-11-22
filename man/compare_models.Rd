\name{compare_models}
\alias{compare_models}
\title{compare models}
\description{
  compare models for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
compare_models(fits, wave = NULL, role = NULL,
               min_proportion = NULL, min_expected_n = NULL)
}
\arguments{
  \item{fits}{Fitted models to compare or select among.}
  \item{wave}{One label per fit, stored on that comparison row.
    Default: \code{NULL}.}
  \item{role}{One role label per fit, stored on that comparison row.
    Default: \code{NULL}.}
  \item{min_proportion}{Threshold that flags a small class proportion.
    Default: \code{NULL}.}
  \item{min_expected_n}{Threshold that flags a small expected class count.
    Default: \code{NULL}.}
}
\value{
  A tibble with one row per fit.
}
