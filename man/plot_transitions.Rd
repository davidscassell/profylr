\name{plot_transitions}
\alias{plot_transitions}
\title{plot transitions}
\description{
  plot transitions for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
plot_transitions(fit)
}
\arguments{
  \item{fit}{A fitted model.}
}
\value{
  A ggplot of the transition matrix.
}
