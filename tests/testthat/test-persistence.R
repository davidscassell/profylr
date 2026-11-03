library(profylr)

fresh_roundtrip <- function(fit) {
  model <- tempfile(fileext = ".rds")
  ref <- tempfile(fileext = ".rds")
  save_model(fit, model)
  saveRDS(list(
    fit = fit,
    posterior = posterior_states(fit),
    decoded = decode_states(fit),
    tidy = tidy(fit),
    glance = glance(fit),
    augment = augment(fit)
  ), ref)
  script <- tempfile(fileext = ".R")
  writeLines(c(
    "suppressPackageStartupMessages(library(profylr))",
    sprintf("fit <- load_model(%s)", shQuote(normalizePath(model))),
    sprintf("ref <- readRDS(%s)", shQuote(normalizePath(ref))),
    "stopifnot(identical(fit, ref$fit))",
    "stopifnot(identical(fit$spec, ref$fit$spec))",
    "stopifnot(identical(fit$model, ref$fit$model))",
    "stopifnot(identical(fit$engine, ref$fit$engine))",
    "stopifnot(identical(fit$draws, ref$fit$draws))",
    "stopifnot(identical(fit$interval_type, ref$fit$interval_type))",
    "stopifnot(identical(posterior_states(fit), ref$posterior))",
    "stopifnot(identical(decode_states(fit), ref$decoded))",
    "stopifnot(identical(tidy(fit), ref$tidy))",
    "stopifnot(identical(glance(fit), ref$glance))",
    "stopifnot(identical(augment(fit), ref$augment))",
    "cat('fresh-process identical\\n')"
  ), script)
  # R CMD check installs the package on a temporary library path. A bare
  # Rscript does not see that path, so the child is given the parent's search
  # path and no site or user library.
  libs <- paste(.libPaths(), collapse = .Platform$path.sep)
  rscript <- file.path(R.home("bin"), "Rscript")
  status <- system2(
    rscript,
    c("--vanilla", script),
    stdout = TRUE,
    stderr = TRUE,
    env = c(
      paste0("R_LIBS=", libs),
      "R_LIBS_USER=NULL",
      "R_LIBS_SITE=NULL"
    )
  )
  expect_null(attr(status, "status"), info = paste(status, collapse = "\n"))
  expect_true(any(grepl("fresh-process identical", status, fixed = TRUE)),
              info = paste(status, collapse = "\n"))
}

test_that("LPA, LCA, and LTA fits reload every stored field in a fresh process", {
  lpa <- fit_lpa(
    data.frame(id = c("a", "b", "c", "d"), y = c(-1.2, NA, 0.8, 1.1)),
    "y", id = "id", n_states = 2, covariance = "EEI",
    n_starts = 2, screen_iter = 1, n_full = 1, max_iter = 8,
    seed = 7, align_labels = FALSE, tol = 1e-7
  )
  lca <- fit_lca(
    data.frame(id = c("a", "a", "b", "b"), wave = c(1, 2, 1, 2), u = c(1, 0, 1, NA)),
    "u", id = "id", wave = "wave", n_states = 2,
    family = list(u = "bernoulli"),
    n_starts = 2, screen_iter = 1, n_full = 1, max_iter = 8,
    seed = 7, align_labels = FALSE, tol = 1e-7
  )
  lta_integer <- fit_lta(
    data.frame(id = c("a", "a", "b", "b"), wave = c(1, 2, 1, 2), y = c(-1.4, -1.1, 1.2, NA)),
    "y", id = "id", wave = "wave", n_states = 2, covariance = "EEI",
    family = list(y = "gaussian"),
    n_starts = 2, screen_iter = 1, n_full = 1, max_iter = 8,
    seed = 7, align_labels = FALSE, tol = 1e-7
  )
  lta_float_wave <- fit_lta(
    data.frame(id = c("a", "a", "b", "b"), wave = c(1.0, 2.0, 1.0, 2.0), y = c(-1.3, NA, 0.9, 1.4)),
    "y", id = "id", wave = "wave", n_states = 2, covariance = "EEI",
    family = list(y = "gaussian"),
    n_starts = 2, screen_iter = 1, n_full = 1, max_iter = 8,
    seed = 8, align_labels = FALSE, tol = 1e-7
  )
  lta_text_wave <- fit_lta(
    data.frame(id = c("a", "a", "b", "b"), wave = c("t1", "t2", "t1", "t2"), y = c(-1.5, -0.9, NA, 1.3)),
    "y", id = "id", wave = "wave", n_states = 2, covariance = "EEI",
    family = list(y = "gaussian"),
    n_starts = 2, screen_iter = 1, n_full = 1, max_iter = 8,
    seed = 9, align_labels = FALSE, tol = 1e-7
  )
  fresh_roundtrip(lpa)
  fresh_roundtrip(lca)
  fresh_roundtrip(lta_integer)
  fresh_roundtrip(lta_float_wave)
  fresh_roundtrip(lta_text_wave)
})

test_that("the binding recovers separated Gaussian means", {
  # One dataset through the installed package. This is not the eight-replicate
  # C0 rate, which stays in the core suite.
  set.seed(4101)
  y <- c(rnorm(40, -3, 0.4), rnorm(40, 3, 0.4))
  data <- data.frame(id = seq_along(y), y = y)
  fit <- fit_lpa(data, "y", n_states = 2, covariance = "EEI",
                 n_starts = 10, screen_iter = 5, n_full = 4, max_iter = 50,
                 seed = 7001, align_labels = FALSE, tol = 1e-7)
  means <- sort(fit$result$params$means)
  expect_lt(abs(means[[1]] - (-3)), 0.50)
  expect_lt(abs(means[[2]] - 3), 0.50)
  expect_true(isTRUE(fit$result$admissible))
})
