library(profylr)

ctrl <- list(
  n_starts = 40L, n_full = 4L, screen_iter = 10L, max_iter = 200L, tol = 1e-8,
  replicate_tol = 1e-3, seed = 1, n_threads = 1L, align_labels = FALSE
)

read_num <- function(name) {
  path <- profylr_fixture("mplus", name)
  lines <- readLines(path)
  rows <- lapply(lines[nzchar(lines)], function(line) {
    toks <- strsplit(line, " ", fixed = TRUE)[[1]]
    vapply(toks, function(tok) if (tok %in% c(".", "nan")) NA_real_ else as.numeric(tok), numeric(1))
  })
  as.data.frame(do.call(rbind, rows))
}

test_that("the default start budget is 40", {
  expect_equal(lpa_spec(2)$n_starts, 40L)
})

test_that("the missing-data VVV fixture replicates the Mplus mode", {
  frame <- read_num("lpa_vvv_k2_missing.dat")
  names(frame) <- c("y1", "y2")
  frame$id <- seq_len(nrow(frame))
  fit <- do.call(fit_lpa, c(list(frame, c("y1", "y2"), id = "id", n_states = 2L, covariance = "VVV"), ctrl))
  expect_equal(fit$result$loglik, -132.902976, tolerance = 1e-4)
  expect_gte(fit$result$n_best, 2L)
  expect_equal(fit$result$status, "ok")
  expect_equal(fit$result$n_parameters, 11L)
  expect_equal(fit$result$bic, 308.838, tolerance = 1e-3)
  expect_true(fit$result$cov_positive_definite)
  expect_false(fit$result$cov_ill_conditioned)
  expect_false(fit$result$cov_collapsed_component)
  expect_false(fit$result$regularized)
  expect_equal(fit$result$n_boundary, 0L)
  expect_lt(fit$result$condition_number, 1e6)
  small <- which.min(fit$result$params$delta)
  expect_equal(fit$result$params$delta[[small]], 0.257, tolerance = 2e-3)
  expect_equal(fit$result$params$cov[[(small - 1) * 3 + 2]], -0.25, tolerance = 2e-3)
})

test_that("max_iter is an exact cap", {
  set.seed(1)
  y1 <- c(rnorm(20, -3, 0.6), rnorm(20, 3, 0.6))
  y2 <- c(rnorm(20, -3, 0.6), rnorm(20, 3, 0.6))
  frame <- data.frame(y1 = y1, y2 = y2)
  fit <- fit_lpa(frame, c("y1", "y2"), n_states = 2L, covariance = "VVI",
                 n_starts = 1L, screen_iter = 0L, n_full = 1L, max_iter = 2L,
                 tol = 0, seed = 1, align_labels = FALSE)
  expect_false(fit$result$converged)
  expect_equal(fit$result$n_iter, 2L)
  expect_equal(fit$result$trace_iter, c(1L, 2L))
  screened <- fit_lpa(frame, c("y1", "y2"), n_states = 2L, covariance = "VVI",
                      n_starts = 4L, screen_iter = 2L, n_full = 3L, max_iter = 5L,
                      tol = 0, seed = 1, align_labels = FALSE)
  expect_lte(screened$result$n_iter, 5L)
  expect_equal(length(screened$result$trace_ll), screened$result$n_iter)
  expect_lte(max(screened$result$trace_iter), 5L)
})

test_that("selection returns the usable VVV solution and reports the singular one", {
  frame <- read_num("lpa_vvv_k2_missing.dat")
  names(frame) <- c("y1", "y2")
  fit <- fit_lpa(frame, c("y1", "y2"), n_states = 2L, covariance = "VVV",
                 n_starts = 500L, screen_iter = 0L, n_full = 500L, max_iter = 200L,
                 tol = 1e-8, replicate_tol = 1e-3, seed = 1, n_threads = 1L,
                 align_labels = FALSE)
  expect_equal(fit$result$loglik, -132.902976, tolerance = 1e-3)
  expect_true(fit$result$admissible)
  expect_false(fit$result$regularized)
  expect_false(fit$result$status == "unusable")
  expect_gte(fit$result$n_best, 2L)
  expect_true(length(fit$result$rejected_loglik) >= 1L)
  hit <- which(abs(fit$result$rejected_loglik - (-129.595734)) < 5e-3)
  expect_true(length(hit) == 1L)
  expect_equal(fit$result$rejected_level[[hit]], "blocking")
  expect_true(grepl("variance_floor", fit$result$rejected_reason[[hit]], fixed = TRUE))
  expect_equal(fit$result$rejected_n[[hit]], 1L)
  expect_gt(fit$result$rejected_loglik[[hit]], fit$result$loglik)
})

test_that("Hungarian alignment of the latent transition matches Mplus at printed precision", {
  wide <- read_num("lta_bernoulli_k2_t2.dat")
  rows <- lapply(seq_len(nrow(wide)), function(i) {
    rbind(
      data.frame(id = i, wave = 1L, u1 = wide[i, 1], u2 = wide[i, 2]),
      data.frame(id = i, wave = 2L, u1 = wide[i, 3], u2 = wide[i, 4])
    )
  })
  frame <- do.call(rbind, rows)
  fit <- do.call(fit_lta, c(list(frame, c("u1", "u2"), id = "id", wave = "wave", n_states = 2L), ctrl))
  expect_equal(fit$result$loglik, -360.432800, tolerance = 1e-4)
  expect_equal(fit$result$n_parameters, 7L)
  K <- 2L
  R <- 2L
  buf <- numeric(2 * K * R)
  ps <- list(c(0.838, 0.054), c(0.804, 0.140))
  for (j in 0:1) for (k in 0:1) {
    p1 <- ps[[j + 1]][k + 1]
    buf[((j) * K + k) * R + 1] <- 1 - p1
    buf[((j) * K + k) * R + 2] <- p1
  }
  reference <- list(delta = c(0.507, 0.493), tau = c(0.860, 0.140, 0.041, 0.959),
                    rho = buf, means = numeric(0), cov = numeric(0))
  aligned <- align_states(fit, reference)
  expect_equal(aligned$alignment$permutation, c(0L, 1L))
  expect_lt(max(abs(aligned$fit$result$params$delta - c(0.507, 0.493))), 5e-4)
  tau <- matrix(aligned$fit$result$params$tau, 2, 2, byrow = TRUE)
  target <- matrix(c(0.860, 0.140, 0.041, 0.959), 2, 2, byrow = TRUE)
  expect_lt(max(abs(tau - target)), 5e-4)
})
