\name{select_model}
\alias{select_model}
\title{Apply the selection rule}
\description{
  Apply the selection rule for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
select_model(fits, rule = selection_rule(), auto = TRUE, second_seed_loglik = NULL, second_seed_n_best = NULL, waves = "auto")
}
\arguments{
  \item{fits}{Argument \code{fits}.}
  \item{rule}{Argument \code{rule}. Default: \code{selection_rule()}.}
  \item{auto}{Argument \code{auto}. Default: \code{TRUE}.}
  \item{second_seed_loglik}{Argument \code{second_seed_loglik}. Default: \code{NULL}.}
  \item{second_seed_n_best}{Argument \code{second_seed_n_best}. Default: \code{NULL}.}
  \item{waves}{Argument \code{waves}. Default: \code{"auto"}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
