\name{lta_spec}
\alias{lta_spec}
\title{lta spec}
\description{
  lta spec for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
lta_spec(n_states, covariance = "VVV", measurement_invariant = TRUE,
         means_invariant = TRUE, covariance_invariant = TRUE,
         transitions = c("equal", "free"), n_starts = 40L,
         screen_iter = 10L, n_full = 4L, max_iter = 200L,
         tol = 1e-07, replicate_tol = 0.1, n_threads = 1L,
         seed = 1, align_labels = TRUE, covariates = NULL,
         start = NULL, append_start = FALSE,
         estep = c("stored", "streaming"), posteriors = TRUE,
         estep_threads = 1L, patterns = "none")
}
\arguments{
  \item{n_states}{Number of latent states.}
  \item{covariance}{Gaussian covariance structure. Default: \code{"VVV"}.}
  \item{measurement_invariant}{Hold the measurement model equal across waves.
    Default: \code{TRUE}.}
  \item{means_invariant}{Hold Gaussian means equal across waves. Default: \code{TRUE}.}
  \item{covariance_invariant}{Hold the Gaussian covariance equal across waves.
    Default: \code{TRUE}.}
  \item{transitions}{Equal or free transition matrices.
    Default: \code{c("equal", "free")}.}
  \item{n_starts}{Number of random starts. Default: \code{40L}.}
  \item{screen_iter}{Iterations used to screen starts. Default: \code{10L}.}
  \item{n_full}{Number of starts run to the full iteration cap. Default: \code{4L}.}
  \item{max_iter}{EM iteration cap. Default: \code{200L}.}
  \item{tol}{Relative convergence tolerance. Default: \code{1e-07}.}
  \item{replicate_tol}{Absolute gap \code{abs(ll - best)} that still counts
    as the same solution. Default: \code{0.1}. A caller can set it.
    It does not scale with N or with the log-likelihood.}
  \item{n_threads}{Worker count. Default: \code{1L}.}
  \item{seed}{Random seed. Default: \code{1}.}
  \item{align_labels}{Align state labels across starts. Default: \code{TRUE}.}
  \item{covariates}{Refused in Release 1. Default: \code{NULL}.}
  \item{start}{Optional supplied start. Default: \code{NULL}.}
  \item{append_start}{Add the supplied start to the random starts. Default: \code{FALSE}.}
  \item{estep}{\code{stored} or \code{streaming}.
    Default: \code{c("stored", "streaming")}.}
  \item{posteriors}{Return posterior state probabilities. Default: \code{TRUE}.}
  \item{estep_threads}{Threads inside one E-step. Default: \code{1L}.}
  \item{patterns}{\code{none} or \code{compress}. Default: \code{"none"}.}
}
\value{
  A latent transition specification.
}
