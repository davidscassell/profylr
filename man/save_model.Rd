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
  \item{x}{Object to archive.}
  \item{file}{Path of a saved model.}
}
\value{
  The path written, returned invisibly.
}
