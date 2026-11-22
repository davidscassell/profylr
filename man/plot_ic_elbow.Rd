\name{plot_ic_elbow}
\alias{plot_ic_elbow}
\title{plot ic elbow}
\description{
  plot ic elbow for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
plot_ic_elbow(selection)
}
\arguments{
  \item{selection}{A selection result.}
}
\value{
  A ggplot of the information-criterion elbow.
}
