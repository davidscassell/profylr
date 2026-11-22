\name{blrt_check}
\alias{blrt_check}
\title{Bootstrap likelihood-ratio test}
\description{
  Bootstrap likelihood-ratio test for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
blrt_check(h0, h1, B = 1000, max_retries = 3, alpha = 0.05,
           seed = NULL, max_iter = NULL, n_threads = 1L,
           experimental = FALSE, batch_size = NULL)
}
\arguments{
  \item{h0}{Fitted null model.}
  \item{h1}{Fitted alternative model.}
  \item{B}{Number of bootstrap draws. Default: \code{1000}.}
  \item{max_retries}{Extra attempts on the same simulated dataset. Default: \code{3}.}
  \item{alpha}{Tail probability used by the bounds. Default: \code{0.05}.}
  \item{seed}{Bootstrap seed. The null fit's seed is used when omitted.
    Default: \code{NULL}.}
  \item{max_iter}{Bootstrap EM cap. The selection rule's cap is used when omitted.
    Default: \code{NULL}.}
  \item{n_threads}{Worker count. Default: \code{1L}.}
  \item{experimental}{The bootstrap runs only when this is true. Default: \code{FALSE}.}
  \item{batch_size}{How many simulated datasets the pool holds at once.
    \code{NULL} uses \code{n_threads}.}
}
\value{
  A list. \code{validated} stays \code{FALSE}.
}
