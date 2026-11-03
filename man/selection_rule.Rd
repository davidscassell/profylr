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
  \item{min_class_prop}{Argument \code{min_class_prop}. Default: \code{NULL}.}
  \item{min_class_n}{Argument \code{min_class_n}. Default: \code{NULL}.}
  \item{min_avepp}{Argument \code{min_avepp}. Default: \code{NULL}.}
  \item{entropy_filter}{Argument \code{entropy_filter}. Default: \code{FALSE}.}
  \item{min_entropy}{Argument \code{min_entropy}. Default: \code{0.8}.}
  \item{primary}{Argument \code{primary}. Default: \code{NULL}.}
  \item{tie_zone}{Argument \code{tie_zone}. Default: \code{2}.}
  \item{second_seed_offset}{Argument \code{second_seed_offset}. Default: \code{10007}.}
  \item{require_second_seed}{Argument \code{require_second_seed}. Default: \code{TRUE}.}
  \item{blrt}{Argument \code{blrt}. Default: \code{FALSE}.}
  \item{blrt_B}{Argument \code{blrt_B}. Default: \code{1000}.}
  \item{blrt_max_retries}{Argument \code{blrt_max_retries}. Default: \code{3}.}
  \item{blrt_alpha}{Argument \code{blrt_alpha}. Default: \code{0.05}.}
  \item{blrt_check}{Argument \code{blrt_check}. Default: \code{FALSE}.}
  \item{blrt_max_iter}{Argument \code{blrt_max_iter}. Default: \code{1000L}.}
  \item{experimental_blrt}{Argument \code{experimental_blrt}. Default: \code{FALSE}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
