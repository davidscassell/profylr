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
         transitions = c("equal", "free"), n_starts = 40L, screen_iter = 10L,
         n_full = 4L, max_iter = 200L, tol = 1e-07, replicate_tol = 0.001,
         n_threads = 1L, seed = 1, align_labels = TRUE, covariates = NULL,
         start = NULL, append_start = FALSE)
}
\arguments{
  \item{n_states}{Argument \code{n_states}.}
  \item{covariance}{Argument \code{covariance}. Default: \code{"VVV"}.}
  \item{measurement_invariant}{Argument \code{measurement_invariant}. Default: \code{TRUE}.}
  \item{means_invariant}{Argument \code{means_invariant}. Default: \code{TRUE}.}
  \item{covariance_invariant}{Argument \code{covariance_invariant}. Default: \code{TRUE}.}
  \item{transitions}{Argument \code{transitions}. Default: \code{c("equal", "free")}.}
  \item{n_starts}{Argument \code{n_starts}. Default: \code{40L}.}
  \item{screen_iter}{Argument \code{screen_iter}. Default: \code{10L}.}
  \item{n_full}{Argument \code{n_full}. Default: \code{4L}.}
  \item{max_iter}{Argument \code{max_iter}. Default: \code{200L}.}
  \item{tol}{Argument \code{tol}. Default: \code{1e-07}.}
  \item{replicate_tol}{Argument \code{replicate_tol}. Default: \code{0.001}.}
  \item{n_threads}{Argument \code{n_threads}. Default: \code{1L}.}
  \item{seed}{Argument \code{seed}. Default: \code{1}.}
  \item{align_labels}{Argument \code{align_labels}. Default: \code{TRUE}.}
  \item{covariates}{Argument \code{covariates}. Default: \code{NULL}.}
  \item{start}{Argument \code{start}. Default: \code{NULL}.}
  \item{append_start}{Argument \code{append_start}. Default: \code{FALSE}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
