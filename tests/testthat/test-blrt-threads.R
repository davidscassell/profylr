library(profylr)

test_that("bootstrap draws agree for 1, 2, 4, and the hardware thread count", {
  expect_gte(blrt_hardware_threads(), 1L)
  set.seed(19)
  n <- 40
  group <- rep(c(-1.5, 1.5), each = n / 2)
  data <- data.frame(y1 = group + rnorm(n, 0, 0.4), y2 = group + rnorm(n, 0, 0.4))
  controls <- list(
    covariance = "EEI", n_starts = 4L, screen_iter = 0L, n_full = 2L,
    max_iter = 40L, seed = 19L, n_threads = 1L
  )
  h0 <- do.call(fit_lpa, c(list(data, c("y1", "y2"), n_states = 1L), controls))
  h1 <- do.call(fit_lpa, c(list(data, c("y1", "y2"), n_states = 2L), controls))
  run <- function(threads) {
    suppressMessages(blrt_check(experimental = TRUE, 
      h0, h1, B = 4L, max_retries = 1L, seed = 19, max_iter = 40L, n_threads = threads
    ))
  }
  limit <- thread_limit()
  hw <- blrt_hardware_threads()
  one <- if (1 <= limit) run(1L) else NULL
  two <- if (2 <= limit) run(2L) else NULL
  if (!is.null(one)) expect_equal(one$n_threads, 1L)
  compared <- list()
  if (!is.null(two)) {
    two_tel <- blrt_last_telemetry()
    expect_equal(two$n_threads, 2L)
    expect_false("telemetry" %in% names(two))
    expect_false(any(c("seconds", "n_threads_used") %in% names(two)))
    expect_equal(two_tel$n_threads_used, min(2L, hw, 4L))
    if (hw >= 2L) expect_gt(two_tel$n_threads_used, 1L)
    compared <- list(two)
  }
  if (4 <= limit) {
    four <- run(4L)
    expect_equal(four$n_threads, 4L)
    compared <- c(compared, list(four))
  }
  if (hw <= limit) {
    cap <- run(hw)
    expect_equal(cap$n_threads, hw)
    compared <- c(compared, list(cap))
  }
  drop_threads <- function(result) {
    expect_false("telemetry" %in% names(result))
    result$n_threads <- NULL
    result
  }
  expect_false(is.null(one))
  for (other in compared) {
    expect_equal(drop_threads(other), drop_threads(one))
    expect_equal(other$p, one$p)
    expect_equal(other$b, one$b)
    expect_equal(other$lrs, one$lrs)
    expect_equal(other$lr_draws, one$lr_draws)
    expect_equal(other$n_ok, one$n_ok)
    expect_equal(other$n_failed, one$n_failed)
    expect_equal(other$n_failed_not_converged, one$n_failed_not_converged)
    expect_equal(other$n_failed_inadmissible, one$n_failed_inadmissible)
    expect_equal(other$n_failed_error, one$n_failed_error)
    expect_equal(other$n_not_replicated, one$n_not_replicated)
    expect_equal(other$observed_h0_replicated, one$observed_h0_replicated)
    expect_equal(other$observed_h1_replicated, one$observed_h1_replicated)
  }
  expect_equal(one$n_ok + one$n_failed, 4L)
})
