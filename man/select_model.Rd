\name{select_model}
\alias{select_model}
\title{Apply the selection rule}
\description{
  Apply the selection rule for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
select_model(fits, rule = selection_rule(), auto = TRUE,
             second_seed_loglik = NULL, second_seed_n_best = NULL,
             waves = "auto")
}
\arguments{
  \item{fits}{Fitted models to compare or select among.}
  \item{rule}{Selection thresholds. See selection_rule. Default: \code{selection_rule()}.}
  \item{auto}{Apply the automatic rule when true. Default: \code{TRUE}.}
  \item{second_seed_loglik}{Log-likelihood of a second-seed replication.
    Default: \code{NULL}.}
  \item{second_seed_n_best}{How many starts hit that second-seed likelihood.
    Default: \code{NULL}.}
  \item{waves}{How per-wave diagnostics are included. Default: \code{"auto"}.}
}
\value{
  An object of class \code{profylr_model_selection}.
  An unvalidated bootstrap does not change the chosen model.
}
