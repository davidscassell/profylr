library(profylr)

skip_reference <- function() {
  missing <- c("lavaan", "semTools")[!vapply(c("lavaan", "semTools"), requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("required reference packages are not installed: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

# Several within-row missingness patterns. The data are generated here.
# lavaan is called for its numbers only. Its source is not copied.
mask_patterns <- function(Y, every, patterns) {
  for (i in seq_len(nrow(Y))) {
    if (i %% every != 0) next
    code <- patterns[((i / every - 1) %% length(patterns)) + 1]
    digits <- if (code == 0) integer(0) else as.integer(strsplit(as.character(code), "")[[1]])
    if (length(digits)) Y[i, digits] <- NA_real_
  }
  Y
}

gaussian_frame <- function(seed) {
  set.seed(seed)
  n <- 160
  sigma <- matrix(c(1, .35, .15, .05,
                    .35, 1, .25, .1,
                    .15, .25, 1, .2,
                    .05, .1, .2, 1), 4, 4)
  Y <- matrix(rnorm(n * 4), n, 4) %*% chol(sigma)
  Y <- sweep(Y, 2, c(0.2, -0.4, 0.8, 0.1), "+")
  colnames(Y) <- paste0("y", 1:4)
  as.data.frame(Y)
}

lavaan_models <- function(df) {
  vars <- names(df)
  pairs <- utils::combn(vars, 2)
  pair_free <- apply(pairs, 2, function(ab) paste(ab[[1]], "~~", ab[[2]]))
  pair_zero <- apply(pairs, 2, function(ab) paste(ab[[1]], "~~ 0*", ab[[2]]))
  saturated <- paste(c(paste(vars, "~~", vars), pair_free, paste(vars, "~ 1")), collapse = "\n")
  diagonal <- paste(c(paste(vars, "~~", vars), pair_zero, paste(vars, "~ 1")), collapse = "\n")
  list(
    full = lavaan::sem(saturated, data = df, missing = "ml", meanstructure = TRUE),
    diagonal = lavaan::sem(diagonal, data = df, missing = "ml", meanstructure = TRUE)
  )
}

expect_moments <- function(df, label) {
  vars <- grep("^y", names(df), value = TRUE)
  df <- df[vars]
  fits <- lavaan_models(df)
  full <- fiml_moments(df, vars, covariance = "VVV")
  diagonal <- fiml_moments(df, vars, covariance = "EEI")
  expect_true(full$converged, label)
  expect_equal(full$loglik, as.numeric(lavaan::fitMeasures(fits$full, "logl")), tolerance = 1e-6, label = label)
  expect_equal(as.numeric(full$n_parameters), as.numeric(lavaan::fitMeasures(fits$full, "npar")), label = label)
  expect_equal(as.numeric(full$mean), as.numeric(lavaan::lavInspect(fits$full, "mean.ov")), tolerance = 1e-5)
  expect_equal(as.numeric(full$cov), as.numeric(lavaan::lavInspect(fits$full, "cov.ov")), tolerance = 1e-5)
  expect_equal(diagonal$loglik, as.numeric(lavaan::fitMeasures(fits$diagonal, "logl")), tolerance = 1e-6)
  expect_equal(as.numeric(diagonal$cov), as.numeric(lavaan::lavInspect(fits$diagonal, "cov.ov")), tolerance = 1e-5)
  ml_cor <- lavaan::lavCor(df, missing = "ml")
  expect_equal(as.numeric(full$cor), as.numeric(ml_cor[names(df), names(df)]), tolerance = 1e-5)
  pair <- correlate_indicators(df, vars)
  pair_ref <- lavaan::lavCor(df, missing = "pairwise")
  for (i in seq_len(nrow(pair))) {
    if (pair$variable_a[[i]] == pair$variable_b[[i]]) next
    expect_equal(pair$correlation[[i]], pair_ref[pair$variable_a[[i]], pair$variable_b[[i]]], tolerance = 1e-8)
  }
  described <- describe_indicators(cbind(df, id = seq_len(nrow(df))), vars, id = "id")
  for (name in vars) {
    x <- df[[name]]
    x <- x[!is.na(x)]
    row <- described[described$variable == name, ]
    expect_equal(row$mean, mean(x), tolerance = 1e-10)
    expect_equal(row$sd, stats::sd(x), tolerance = 1e-10)
    expect_equal(row$skew, as.numeric(semTools::skew(x, population = TRUE)), tolerance = 1e-6)
    expect_equal(row$kurtosis, as.numeric(semTools::kurtosis(x, population = TRUE)), tolerance = 1e-6)
  }
  direct <- fit_lpa(df, vars, n_states = 1, covariance = "VVV",
                    n_starts = 1, max_iter = 500, tol = 1e-12, screen_iter = 0, seed = 1)
  expect_equal(direct$result$loglik, as.numeric(lavaan::fitMeasures(fits$full, "logl")), tolerance = 1e-6)
}

test_that("one-state Gaussian FIML matches lavaan under several missingness patterns", {
  skip_reference()
  scattered <- as.data.frame(mask_patterns(as.matrix(gaussian_frame(11)), 5, c(0, 1, 2, 3, 12, 13, 23, 123)))
  expect_moments(scattered, "scattered patterns")

  grouped <- as.data.frame(mask_patterns(as.matrix(gaussian_frame(19)), 4, c(4, 14, 23, 1)))
  expect_moments(grouped, "grouped patterns")
})
