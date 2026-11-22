# The effective worker cap for this process.
# _R_CHECK_LIMIT_CORES_ set to a true value means 2.
# A numeric OMP_THREAD_LIMIT is that count, not a switch that disables
# every multi-thread case. CRAN (NOT_CRAN unset) is also capped at 2.
# The tightest active cap wins. No cap means every requested count may run.
thread_limit <- function() {
  caps <- numeric()
  if (!identical(Sys.getenv("NOT_CRAN", unset = ""), "true")) caps <- c(caps, 2)
  flag <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = "")
  if (nzchar(flag) && !identical(toupper(flag), "FALSE")) caps <- c(caps, 2)
  omp <- Sys.getenv("OMP_THREAD_LIMIT", unset = "")
  if (nzchar(omp)) {
    n <- suppressWarnings(as.integer(omp))
    if (!is.na(n) && n >= 1L) caps <- c(caps, n)
  }
  if (!length(caps)) return(Inf)
  min(caps)
}
