library(profylr)

# cpp_evaluate applies the connected near-tie rule and does not run an M-step,
# so the supplied weights stay the weights that are ordered. These tests do
# not use the fit thread pool. The numbers are the core fixture in
# test_connected_near_tie_groups. A separate test fits with more starts than
# threads, which is what gives the worker pool more than one worker.

gaussian_panel <- function() {
  data <- data.frame(id = c(1, 1), wave = c(1, 2), y = c(-1.2, 0.7))
  prepare_panel(data, "y", "id", "wave", family = list(y = "gaussian"))
}

categorical_panel <- function() {
  data <- data.frame(id = rep(1:3, each = 2), wave = rep(1:2, 3), u = rep(0:2, each = 2))
  prepare_panel(data, "u", "id", "wave", family = list(u = "nominal"))
}

next_perm <- function(p) {
  n <- length(p)
  i <- n - 1L
  while (i >= 1L && p[[i]] >= p[[i + 1L]]) i <- i - 1L
  if (i < 1L) return(NULL)
  j <- n
  while (p[[j]] <= p[[i]]) j <- j - 1L
  tmp <- p[[i]]
  p[[i]] <- p[[j]]
  p[[j]] <- tmp
  if (i < n) p[(i + 1L):n] <- rev(p[(i + 1L):n])
  p
}

all_perms <- function(n) {
  p <- seq_len(n)
  out <- list(p)
  repeat {
    p <- next_perm(p)
    if (is.null(p)) break
    out[[length(out) + 1L]] <- p
  }
  out
}

row_major <- function(x) as.vector(t(x))

chain_delta <- function() {
  offsets <- c(1.35e-8, 0.45e-8, -0.45e-8, -1.35e-8)
  delta <- numeric(4)
  delta[1:3] <- 0.25 + offsets[1:3]
  delta[4] <- 1 - sum(delta[1:3])
  delta
}

chain_tau <- function() {
  raw <- matrix(0, 4, 4)
  for (from in 1:4) for (to in 1:4) raw[from, to] <- from * 10 + to
  raw / rowSums(raw)
}

test_that("a boundary join and split keep the expected order", {
  panel <- gaussian_panel()
  spec <- lta_spec(2, covariance = "VVI", align_labels = TRUE)
  join_lo <- 0.5 - 1e-8 / 2
  join_hi <- 0.5 + 1e-8 / 2
  split_lo <- 0.5 - (1e-8 + 1e-12) / 2
  split_hi <- 0.5 + (1e-8 + 1e-12) / 2
  expect_true(abs(join_hi - join_lo) <= 1e-8)
  expect_true(abs(split_hi - split_lo) > 1e-8)
  expect_equal(join_lo + join_hi, 1)
  expect_equal(split_lo + split_hi, 1)
  means <- c(1, -1)
  cov <- c(0.2, 0.4)
  tau <- matrix(c(0.8, 0.2, 0.3, 0.7), nrow = 2, byrow = TRUE)
  params_for <- function(low, high, perm) {
    list(
      delta = c(low, high)[perm],
      means = means[perm],
      cov = cov[perm],
      tau = row_major(tau[perm, perm, drop = FALSE])
    )
  }
  joined <- profylr:::cpp_evaluate(panel, spec, params_for(join_lo, join_hi, 1:2))$params
  expect_equal(joined$means, c(1, -1), tolerance = 0)
  expect_equal(joined$delta, c(join_lo, join_hi), tolerance = 0)
  expect_equal(joined$cov, c(0.2, 0.4), tolerance = 0)
  expect_equal(joined$tau, c(0.8, 0.2, 0.3, 0.7), tolerance = 0)
  expect_lt(joined$delta[[1]], joined$delta[[2]])
  for (perm in list(1:2, 2:1)) {
    got <- profylr:::cpp_evaluate(panel, spec, params_for(join_lo, join_hi, perm))$params
    expect_equal(got$delta, joined$delta, tolerance = 0)
    expect_equal(got$means, joined$means, tolerance = 0)
    expect_equal(got$cov, joined$cov, tolerance = 0)
    expect_equal(got$tau, joined$tau, tolerance = 0)
  }
  split <- profylr:::cpp_evaluate(panel, spec, params_for(split_lo, split_hi, 1:2))$params
  expect_equal(split$means, c(-1, 1), tolerance = 0)
  expect_equal(split$delta, c(split_hi, split_lo), tolerance = 0)
  expect_equal(split$cov, c(0.4, 0.2), tolerance = 0)
  expect_equal(split$tau, c(0.7, 0.3, 0.2, 0.8), tolerance = 0)
  for (perm in list(1:2, 2:1)) {
    got <- profylr:::cpp_evaluate(panel, spec, params_for(split_lo, split_hi, perm))$params
    expect_equal(got$delta, split$delta, tolerance = 0)
    expect_equal(got$tau, split$tau, tolerance = 0)
  }
})

test_that("the long chain uses the core fixture and profile order", {
  panel <- gaussian_panel()
  spec <- lta_spec(4, covariance = "VVI", align_labels = TRUE)
  delta <- chain_delta()
  means <- c(0, 1, 2, 3)
  cov <- c(0.1, 0.2, 0.3, 0.4)
  tau <- chain_tau()
  order <- c(4L, 3L, 2L, 1L)
  expected_delta <- delta[order]
  expected_tau <- row_major(tau[order, order, drop = FALSE])
  expect_lt(expected_delta[[1]], expected_delta[[4]])
  expect_gt(max(delta) - min(delta), 1e-8)
  expect_lte(max(delta) - min(delta), 3 * 1e-8)
  for (perm in all_perms(4)) {
    got <- profylr:::cpp_evaluate(panel, spec, list(
      delta = delta[perm], means = means[perm], cov = cov[perm],
      tau = row_major(tau[perm, perm, drop = FALSE])
    ))$params
    expect_equal(got$means, c(3, 2, 1, 0), tolerance = 0)
    expect_equal(got$cov, c(0.4, 0.3, 0.2, 0.1), tolerance = 0)
    expect_equal(got$delta, expected_delta, tolerance = 0)
    expect_equal(got$tau, expected_tau, tolerance = 0)
  }
})

test_that("equal profiles keep the incoming index", {
  panel <- gaussian_panel()
  spec <- lta_spec(3, covariance = "EEI", align_labels = TRUE)
  hi <- 1 / 3 + 0.75e-8
  mid <- 1 / 3
  lo <- 1 / 3 - 0.75e-8
  delta <- c(hi, mid, lo)
  tau <- matrix(c(
    0.50, 0.30, 0.20,
    0.20, 0.50, 0.30,
    0.10, 0.20, 0.70
  ), nrow = 3, byrow = TRUE)
  same <- profylr:::cpp_evaluate(panel, spec, list(
    delta = delta, means = c(0, 0, 0), cov = 1, tau = row_major(tau)
  ))$params
  expect_equal(same$delta, delta, tolerance = 0)
  expect_equal(same$tau, row_major(tau), tolerance = 0)
  rev_perm <- 3:1
  flipped <- profylr:::cpp_evaluate(panel, spec, list(
    delta = delta[rev_perm], means = c(0, 0, 0), cov = 1,
    tau = row_major(tau[rev_perm, rev_perm, drop = FALSE])
  ))$params
  expect_equal(flipped$delta, delta[rev_perm], tolerance = 0)
  expect_false(isTRUE(all.equal(flipped$delta, same$delta, tolerance = 0)))
  mixed <- profylr:::cpp_evaluate(panel, spec, list(
    delta = delta, means = c(0, 0, 5), cov = 1, tau = row_major(tau)
  ))$params
  expect_equal(mixed$means, c(5, 0, 0), tolerance = 0)
  expect_equal(mixed$delta, c(lo, hi, mid), tolerance = 0)
})

test_that("categorical permutations follow the highest category", {
  panel <- categorical_panel()
  spec <- lca_spec(3, align_labels = TRUE)
  hi <- 1 / 3 + 0.75e-8
  mid <- 1 / 3
  lo <- 1 / 3 - 0.75e-8
  delta <- c(hi, mid, lo)
  rho <- matrix(c(
    0.20, 0.70, 0.10,
    0.20, 0.30, 0.50,
    0.05, 0.05, 0.90
  ), nrow = 3, byrow = TRUE)
  tau <- matrix(c(
    0.50, 0.30, 0.20,
    0.20, 0.50, 0.30,
    0.10, 0.20, 0.70
  ), nrow = 3, byrow = TRUE)
  order <- c(3L, 2L, 1L)
  expected_rho <- row_major(rho[order, , drop = FALSE])
  expected_tau <- row_major(tau[order, order, drop = FALSE])
  expect_equal(expected_rho[c(3, 6, 9)], c(0.90, 0.50, 0.10))
  for (perm in all_perms(3)) {
    got <- profylr:::cpp_evaluate(panel, spec, list(
      delta = delta[perm],
      rho = row_major(rho[perm, , drop = FALSE]),
      tau = row_major(tau[perm, perm, drop = FALSE])
    ))$params
    expect_equal(got$delta, delta[order], tolerance = 0)
    expect_equal(got$rho, expected_rho, tolerance = 0)
    expect_equal(got$tau, expected_tau, tolerance = 0)
  }
})

test_that("multiple fit workers match when starts exceed threads", {
  set.seed(3)
  group <- rep(c(-1.5, 1.5), each = 16)
  data <- data.frame(y = group + rnorm(32, 0, 0.35))
  controls <- list(
    covariance = "EEI", n_starts = 6L, screen_iter = 0L, n_full = 4L,
    max_iter = 4L, seed = 3, align_labels = TRUE
  )
  fits <- lapply(c(1L, 2L, 4L), function(threads) {
    do.call(fit_lpa, c(list(data, "y", n_states = 2L, n_threads = threads), controls))
  })
  reference <- fits[[1]]$result
  for (i in seq_along(fits)) {
    threads <- c(1L, 2L, 4L)[[i]]
    out <- fits[[i]]$result
    expect_lt(threads, 6L)
    expect_equal(out$n_threads, threads)
    expect_equal(out$n_starts, 6L)
    expect_equal(out$params$means, reference$params$means, tolerance = 0)
    expect_equal(out$params$delta, reference$params$delta, tolerance = 0)
    expect_equal(out$params$cov, reference$params$cov, tolerance = 0)
    expect_equal(out$loglik, reference$loglik, tolerance = 0)
  }
})
