\name{plot_prevalence}
\alias{plot_prevalence}
\title{plot prevalence}
\description{
  plot prevalence for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
plot_prevalence(fit, kind = c("posterior", "model", "hard"))
}
\arguments{
  \item{fit}{A fitted model.}
  \item{kind}{\code{posterior}, \code{model}, or \code{hard}.
    Default: \code{c("posterior", "model", "hard")}.}
}
\value{
  A ggplot of class proportion by wave.
}
