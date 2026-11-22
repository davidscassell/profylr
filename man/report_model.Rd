\name{report_model}
\alias{report_model}
\title{report model}
\description{
  report model for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
report_model(x, procedure = NULL)
}
\arguments{
  \item{x}{A fit, a selection, or an analyst record.}
  \item{procedure}{Analysis step the report is attached to. Default: \code{NULL}.}
}
\value{
  A character report. A model selection says it is automatic and not
  an analyst record.
}
