\name{prepare_panel}
\alias{prepare_panel}
\title{Build a numeric panel}
\description{
  Build a numeric panel for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
prepare_panel(data, indicators, id, wave = NULL, family = NULL,
              schedule = NULL, schema = NULL)
}
\arguments{
  \item{data}{Data frame of observations.}
  \item{indicators}{Indicator column names.}
  \item{id}{Subject identifier column.}
  \item{wave}{Wave column. One occasion when omitted. Default: \code{NULL}.}
  \item{family}{Named families, or detection when omitted. Default: \code{NULL}.}
  \item{schedule}{Explicit integer wave codes. With no schedule or schema,
    numeric waves are filled to \code{seq(min, max)}, so waves 1 and 3
    become 1, 2, 3. Nonnumeric waves keep first-appearance order.
    A supplied schema takes precedence over both.
    Default: \code{NULL}.}
  \item{schema}{Training schema reused for a later panel. Default: \code{NULL}.}
}
\value{
  A list holding the numeric panel passed to the C++ core.
}
\examples{
y <- data.frame(
  id = c(1, 1, 2, 2),
  wave = c(1, 2, 1, 2),
  y = c(0, 1, 1, 0)
)
panel <- prepare_panel(y, "y", id = "id", wave = "wave")
panel$N
}
