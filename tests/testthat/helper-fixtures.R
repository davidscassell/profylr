# Fixtures travel with the tests so R CMD check can see them. The monorepo
# copies under fixtures/ stay for the Python tests and for a checkout that
# runs testthat from the repository root.
profylr_fixture <- function(...) {
  local <- testthat::test_path("fixtures", ...)
  if (file.exists(local)) return(local)
  mono <- file.path(testthat::test_path(), "..", "..", "..", "fixtures", ...)
  if (file.exists(mono)) return(normalizePath(mono))
  stop("fixture not found: ", paste(c(...), collapse = "/"), call. = FALSE)
}
