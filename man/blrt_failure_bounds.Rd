\name{blrt_failure_bounds}
\alias{blrt_failure_bounds}
\title{Experimental bootstrap decision bounds}
\description{
  Bounds for one experimental bootstrap likelihood-ratio result.
  \code{validated} stays \code{FALSE}. The bounds do not select a model.
}
\usage{
blrt_failure_bounds(b, f, B, alpha = 0.05, ran = TRUE, n_not_run = 0L)
}
\arguments{
  \item{b}{Successful draws with an exceedance.}
  \item{f}{Failed attempted draws.}
  \item{B}{Scheduled draws.}
  \item{alpha}{Decision level. Default: \code{0.05}.}
  \item{ran}{Whether the check ran. Default: \code{TRUE}.}
  \item{n_not_run}{Unattempted draws. Default: \code{0L}.}
}
\value{
  A list with \code{p_lo}, \code{p_hi}, \code{bounds_decision}, \code{decision},
  and \code{validated = FALSE}.
}
