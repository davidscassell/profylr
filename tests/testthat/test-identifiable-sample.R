test_that("fewer analyzed subjects than states is refused before the core fit", {
  one <- data.frame(y = 0.2)
  expect_error(
    fit_lpa(one, "y", n_states = 2, covariance = "EEI", n_starts = 1,
            screen_iter = 0, n_full = 1, max_iter = 5, seed = 1),
    "too few analyzed subjects to identify 2 states"
  )
  missing <- data.frame(id = c(1, 2), y = c(NA_real_, NA_real_))
  expect_error(
    fit_lpa(missing, "y", id = "id", n_states = 1, covariance = "EEI",
            n_starts = 1, screen_iter = 0, n_full = 1, max_iter = 5, seed = 1),
    "too few analyzed subjects to identify 1 states: 0 subject"
  )
  longitudinal <- data.frame(id = c(1, 1), wave = c(1, 2), y = c(-1, 1))
  expect_error(
    fit_lta(longitudinal, "y", id = "id", wave = "wave", n_states = 2,
            covariance = "EEI", n_starts = 1, screen_iter = 0, n_full = 1,
            max_iter = 5, seed = 1),
    "too few analyzed subjects to identify 2 states: 1 subject"
  )
  two <- data.frame(y = c(-1, 1))
  fit <- fit_lpa(two, "y", n_states = 2, covariance = "EEI", n_starts = 1,
                 screen_iter = 0, n_full = 1, max_iter = 5, seed = 1)
  expect_s3_class(fit, "profylr_fit")
  expect_equal(fit$result$n_analyzed, 2L)
})
