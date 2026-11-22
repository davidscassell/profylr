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
  \item{file}{Path of a saved model.}
}
\value{
  The object stored in the version-1 archive.
}
