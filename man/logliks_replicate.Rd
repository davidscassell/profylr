\name{logliks_replicate}
\alias{logliks_replicate}
\title{Same log-likelihood}
\description{
  Two log-likelihoods count as one solution when
  \code{abs(ll - best) <= replicate_tol}. The default \code{0.1} is
  chosen to be comparable to the approximately 0.1 tolerance Muthén
  described on the Mplus discussion board (2018); Mplus does not document
  an exact value. It does not scale with N or with the log-likelihood.
}
\usage{
logliks_replicate(ll, best, replicate_tol = 0.1)
}
\arguments{
  \item{ll}{Log-likelihood to test.}
  \item{best}{Reference log-likelihood, usually the best one.}
  \item{replicate_tol}{Absolute gap. Default \code{0.1}. A caller can set it.}
}
\value{
  Logical. Non-finite inputs are not a replication.
}
