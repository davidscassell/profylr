\name{compare_models}
\alias{compare_models}
\title{compare models}
\description{
  compare models for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
compare_models(fits, wave = NULL, role = NULL, min_proportion = NULL, min_expected_n = NULL)
}
\arguments{
  \item{fits}{Argument \code{fits}.}
  \item{wave}{Argument \code{wave}. Default: \code{NULL}.}
  \item{role}{Argument \code{role}. Default: \code{NULL}.}
  \item{min_proportion}{Argument \code{min_proportion}. Default: \code{NULL}.}
  \item{min_expected_n}{Argument \code{min_expected_n}. Default: \code{NULL}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
