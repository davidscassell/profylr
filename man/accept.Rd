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
  \item{selection}{Argument \code{selection}.}
  \item{model_id}{Argument \code{model_id}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
