\name{accept}
\alias{accept}
\title{accept}
\description{
  accept for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
accept(selection, model_id)
}
\arguments{
  \item{selection}{A selection result.}
  \item{model_id}{Candidate identifier to record.}
}
\value{
  The same \code{profylr_model_selection}, with \code{accepted} and
  \code{accepted_model_id} set.
}
