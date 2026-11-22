\name{selection_rule}
\alias{selection_rule}
\title{selection rule}
\description{
  selection rule for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
selection_rule(min_class_prop = NULL, min_class_n = NULL, min_avepp = NULL,
               entropy_filter = FALSE, min_entropy = 0.8, primary = NULL,
               tie_zone = 2, second_seed_offset = 10007,
               require_second_seed = TRUE, blrt = FALSE, blrt_B = 1000,
               blrt_max_retries = 3, blrt_alpha = 0.05, blrt_check = FALSE,
               blrt_max_iter = 1000L, experimental_blrt = FALSE)
}
\arguments{
  \item{min_class_prop}{Smallest class proportion a candidate may have.
    Default: \code{NULL}.}
  \item{min_class_n}{Smallest class count a candidate may have. Default: \code{NULL}.}
  \item{min_avepp}{Smallest average posterior a candidate may have. Default: \code{NULL}.}
  \item{entropy_filter}{Apply the entropy threshold when true. Default: \code{FALSE}.}
  \item{min_entropy}{Smallest entropy a candidate may have. Default: \code{0.8}.}
  \item{primary}{Information criterion used to rank candidates. Default: \code{NULL}.}
  \item{tie_zone}{How close a runner-up must be to count as a tie. Default: \code{2}.}
  \item{second_seed_offset}{Seed offset for the replication fit. Default: \code{10007}.}
  \item{require_second_seed}{Require a second seed to replicate the best likelihood.
    Default: \code{TRUE}.}
  \item{blrt}{Run the bootstrap likelihood-ratio test.
    Default: \code{FALSE}.}
  \item{blrt_B}{Bootstrap draws requested by the selection rule. Default: \code{1000}.}
  \item{blrt_max_retries}{Retries on each simulated dataset. Default: \code{3}.}
  \item{blrt_alpha}{Tail probability for the bootstrap bounds. Default: \code{0.05}.}
  \item{blrt_check}{Request a bootstrap check of a tie or the chosen model.
    Default: \code{FALSE}.}
  \item{blrt_max_iter}{EM cap used inside the bootstrap. Default: \code{1000L}.}
  \item{experimental_blrt}{Allow a report-only bootstrap during selection.
    Default: \code{FALSE}.}
}
\value{
  A list of the selection thresholds passed to this call.
}
