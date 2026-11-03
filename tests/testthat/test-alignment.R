library(profylr)

find_fixture <- function() {
  profylr_fixture("parity", "shared.txt")
}

read_fixture <- function() {
  lines <- readLines(find_fixture())
  out <- list()
  for (line in lines) {
    parts <- strsplit(line, " ", fixed = TRUE)[[1]]
    out[[parts[[1]]]] <- paste(parts[-1], collapse = " ")
  }
  out
}

test_that("align_states and add_starts match the shared fixture", {
  locked <- read_fixture()
  frame <- data.frame(
    id = seq_len(40),
    y1 = c(rep(0L, 20), rep(1L, 20)),
    y2 = c(rep(0L, 20), rep(1L, 20))
  )
  fit <- fit_lca(
    frame, c("y1", "y2"), id = "id", n_states = 2, n_starts = 3, n_full = 2,
    screen_iter = 5, max_iter = 50, tol = 1e-8, seed = 5, replicate_tol = 1e-6,
    align_labels = TRUE
  )
  expect_equal(fit$result$n_starts, as.integer(locked$n_starts))
  expect_equal(fit$result$n_parameters, as.integer(locked$n_parameters))
  expect_lt(abs(fit$result$loglik - as.numeric(locked$loglik)), 1e-8)
  reference <- list(
    delta = c(0.2, 0.8),
    rho = c(0.1, 0.9, 0.9, 0.1, 0.1, 0.9, 0.9, 0.1)
  )
  aligned <- align_states(fit, reference)
  permutation <- paste(aligned$alignment$permutation, collapse = " ")
  expect_equal(permutation, locked$permutation)
  expect_equal(as.integer(aligned$alignment$ambiguous), as.integer(locked$ambiguous))
  expect_lt(abs(aligned$alignment$cost - as.numeric(locked$cost)), 1e-8)
  rho <- aligned$fit$result$params$rho
  p1 <- rho[seq(2, length(rho), by = 2)]
  expected <- as.numeric(strsplit(locked$aligned_p1, " ", fixed = TRUE)[[1]])
  expect_equal(p1, expected, tolerance = 1e-8)
  more <- add_starts(fit, n_new = 1, seed = 2)
  expect_equal(more$result$n_starts, as.integer(locked$add_n_starts))
  expect_equal(more$result$n_best, as.integer(locked$add_n_best))
  expect_lt(abs(more$result$loglik - as.numeric(locked$add_loglik)), 1e-8)
  expect_gte(more$result$loglik + 1e-8, fit$result$loglik)
})
