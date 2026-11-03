\name{blrt_last_telemetry}
\alias{blrt_last_telemetry}
\title{Telemetry from the most recent BLRT}
\description{
  Timing and worker counts from the most recent \code{blrt_check} in this
  process. The record is not an element of that result and is not stored as
  an attribute of it. A later \code{blrt_check} replaces it. Before any call,
  the value is an empty list.
}
\usage{
blrt_last_telemetry()
}
\value{
  A named list. After a completed run it contains \code{n_threads_used} and
  \code{seconds}. A pooled run also contains \code{worker_jobs},
  \code{worker_busy_s}, \code{seconds_prepare}, \code{seconds_convert},
  \code{seconds_engine}, \code{seconds_export}, and \code{seconds_assemble}.
  \code{n_threads} on the \code{blrt_check} result remains the requested count.
  \code{n_threads_used} is the number of workers the draw pool actually ran.
}
\seealso{
  \code{\link{blrt_check}}, \code{\link{blrt_hardware_threads}}
}
