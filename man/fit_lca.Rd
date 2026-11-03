\name{fit_lca}
\alias{fit_lca}
\title{Fit a latent class model}
\description{
  Fit a latent class model for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
fit_lca(data, indicators, id = NULL, wave = NULL, n_states = 2L, family = NULL, ...)
}
\arguments{
  \item{data}{Argument \code{data}.}
  \item{indicators}{Argument \code{indicators}.}
  \item{id}{Argument \code{id}. Default: \code{NULL}.}
  \item{wave}{Argument \code{wave}. Default: \code{NULL}.}
  \item{n_states}{Argument \code{n_states}. Default: \code{2L}.}
  \item{family}{Argument \code{family}. Default: \code{NULL}.}
  \item{...}{Argument \code{...}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
