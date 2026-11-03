\name{save_model}
\alias{save_model}
\title{save model}
\description{
  save model for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
save_model(x, file)
}
\arguments{
  \item{x}{Argument \code{x}.}
  \item{file}{Argument \code{file}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
