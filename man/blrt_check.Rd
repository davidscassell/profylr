\name{blrt_check}
\alias{blrt_check}
\title{Bootstrap likelihood-ratio test}
\description{
  Bootstrap likelihood-ratio test for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
blrt_check(h0, h1, B = 1000, max_retries = 3, alpha = 0.05, seed = NULL,
          max_iter = NULL, n_threads = 1L, experimental = FALSE)
}
\arguments{
  \item{h0}{Argument \code{h0}.}
  \item{h1}{Argument \code{h1}.}
  \item{B}{Argument \code{B}. Default: \code{1000}.}
  \item{max_retries}{Argument \code{max_retries}. Default: \code{3}.}
  \item{alpha}{Argument \code{alpha}. Default: \code{0.05}.}
  \item{seed}{Argument \code{seed}. Default: \code{NULL}.}
  \item{max_iter}{Argument \code{max_iter}. Default: \code{NULL}.}
  \item{n_threads}{Argument \code{n_threads}. Default: \code{1L}.}
  \item{experimental}{Argument \code{experimental}. Default: \code{FALSE}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
