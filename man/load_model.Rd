\name{load_model}
\alias{load_model}
\title{load model}
\description{
  load model for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
load_model(file)
}
\arguments{
  \item{file}{Argument \code{file}.}
}
\value{
  The object documented by the function name. Fitters return a
  \code{profylr_fit}.
}
