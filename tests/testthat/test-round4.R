test_that("fractional ids stay distinct and text waves keep first appearance", {
  data <- data.frame(id = c(1.1, 1.2), y = c(10, 20))
  panel <- prepare_panel(data, "y", "id")
  expect_equal(panel$N, 2L)
  expect_equal(panel$gauss[1:2], c(10, 20))
  waves <- data.frame(id = c(1, 1), wave = c("z", "a"), y = c(1, 2))
  wpanel <- prepare_panel(waves, "y", "id", "wave")
  expect_equal(as.character(wpanel$waves), c("z", "a"))
})

test_that("identity and inverse alignment preserve the complete result", {
  y <- c(rnorm(20, -2, 0.2), rnorm(20, 2, 0.2))
  fit <- fit_lpa(
    data.frame(y = y), "y", n_states = 2, covariance = "EEI",
    n_starts = 2, screen_iter = 0, n_full = 2, max_iter = 20,
    seed = 4, align_labels = FALSE, n_threads = 1
  )
  out <- align_states(fit, fit)
  expect_equal(as.integer(out$alignment$permutation), c(0L, 1L))
  expect_equal(out$fit$result, fit$result)
  swapped <- fit$result$params
  swapped$delta <- rev(swapped$delta)
  swapped$means <- rev(swapped$means)
  moved <- align_states(fit, swapped)
  expect_equal(as.integer(moved$alignment$permutation), c(1L, 0L))
  restored <- align_states(moved$fit, fit)
  expect_equal(restored$fit$result, fit$result)
})

test_that("one two and four threads keep the requested count and the same result", {
  y <- c(rnorm(16, -1.5, 0.3), rnorm(16, 1.5, 0.3))
  data <- data.frame(y = y)
  fits <- lapply(c(1L, 2L, 4L), function(threads) {
    fit_lpa(data, "y", n_states = 2, covariance = "EEI", n_starts = 2,
            screen_iter = 0, n_full = 2, max_iter = 12, seed = 8,
            align_labels = FALSE, n_threads = threads)
  })
  for (i in seq_along(fits)) {
    expect_equal(fits[[i]]$result$n_threads, c(1L, 2L, 4L)[[i]])
    left <- fits[[i]]$result
    right <- fits[[1]]$result
    left$n_threads <- NULL
    right$n_threads <- NULL
    expect_equal(left, right)
  }
})

test_that("bindings reject a bad code, a short start, and invalid controls", {
  data <- data.frame(y = c(-1, 1))
  expect_error(
    fit_lpa(data, "y", n_states = 1, covariance = "EEI", n_starts = 1,
            screen_iter = 0, n_full = 1, max_iter = 0),
    "max_iter"
  )
  expect_error(
    fit_lpa(data, "y", n_states = 1, covariance = "EEI", n_starts = 1,
            screen_iter = 0, n_full = 1, max_iter = 1, tol = NaN),
    "tol"
  )
  panel <- list(
    N = 1L, T = 1L, J = 1L, measurement = "categorical", family = "bernoulli",
    ncat = 2L, Rmax = 2L, cat = 2L
  )
  spec <- lca_spec(1, n_starts = 1, screen_iter = 0, n_full = 1, max_iter = 1,
                   start = list(delta = 1, rho = c(0.5, 0.5)))
  expect_error(profylr:::cpp_fit(panel, spec), "ncat")
  panel$cat <- 0L
  spec$start <- list(delta = numeric(), rho = c(0.5, 0.5))
  expect_error(profylr:::cpp_fit(panel, spec), "delta")
})

test_that("string categories run through serial and pooled BLRT", {
  data <- data.frame(id = 1:24, y = rep(c("a", "b", "c"), each = 8))
  controls <- list(n_starts = 2L, screen_iter = 0L, n_full = 2L, max_iter = 20L,
                   seed = 5, align_labels = FALSE)
  h0 <- do.call(fit_lca, c(list(data, "y", id = "id", n_states = 1L), controls))
  h1 <- do.call(fit_lca, c(list(data, "y", id = "id", n_states = 2L), controls))
  for (threads in c(1L, 2L)) {
    out <- suppressMessages(blrt_check(
      h0, h1, B = 2L, max_retries = 0L, seed = 5, max_iter = 20L,
      n_threads = threads, experimental = TRUE
    ))
    expect_gte(out$n_ok, 1L)
    expect_equal(out$n_ok + out$n_failed, 2L)
    expect_equal(out$n_threads, threads)
  }
})

test_that("an alternative below the null refits the stored panel", {
  data <- data.frame(y = c(
    0.1257302210933933, -0.1321048632913019, 0.6404226504432821, 0.1049001171530397,
    -0.535669373161111, 0.3615950549094847, 1.304000045130137, 0.9470809631292422,
    -0.7037352358069926, -1.265421471046053, -0.6232744625373522, 0.0413259793472436,
    -2.325030774638834, -0.2187916639325457, -1.245910947253065, -0.7322673547034516,
    -0.5442589828573099, -0.3163001563691545, 0.4116305363741328, 1.042513369442678,
    -0.1285346629440343, 1.366463470549686, -0.6651946734866135, 0.3515100700930197,
    0.9034701816518086, 0.09401229776087457, -0.7434992493538084, -0.9217253762584194,
    -0.4577258256673392, 0.2201951234700494, -1.009618183538736, -0.2091755748717131,
    -0.1592250099144777, 0.5408455846858077, 0.2146591225063409, 0.3553727090399214,
    -0.6538286094183394, -0.1296136336927695, 0.7839754700613295, 1.493431145220761
  ))
  h0 <- fit_lpa(data, "y", n_states = 1, covariance = "EEI", n_starts = 1,
                screen_iter = 0, n_full = 1, max_iter = 25, tol = 1e-8, seed = 1,
                align_labels = FALSE)
  bad <- list(delta = c(0.5, 0.5), means = c(-20, 20), cov = 1)
  h1 <- fit_lpa(data, "y", n_states = 2, covariance = "EEI", n_starts = 1,
                screen_iter = 0, n_full = 1, max_iter = 2, tol = 0.1, seed = 1,
                align_labels = FALSE, start = bad)
  expect_lt(h1$result$loglik, h0$result$loglik - 1e-4)
  out <- blrt_check(h0, h1, B = 1, max_retries = 0, seed = 1, max_iter = 6,
                    n_threads = 1, experimental = TRUE)
  expect_true(is.character(out$reason))
  expect_true(out$ran || grepl("materially below", out$reason))
})

test_that("bootstrap preparation keeps the generating category count", {
  data <- data.frame(id = 1:6, y = c("a", "b", "c", "a", "b", "c"))
  fit <- fit_lca(data, "y", id = "id", n_states = 1, n_starts = 1, screen_iter = 0,
                 n_full = 1, max_iter = 3, seed = 1, align_labels = FALSE)
  order <- fit$panel$level_order[[1]]
  expect_length(order, 3)
  only <- data.frame(id = c(1, 2), y = c(order[[1]], order[[1]]))
  panel <- profylr:::.prepare_bootstrap_panel(fit, only)
  expect_equal(as.integer(panel$ncat), 3L)
  ends <- data.frame(id = c(1, 2), y = c(order[[1]], order[[3]]))
  kept <- profylr:::.prepare_bootstrap_panel(fit, ends)
  expect_equal(as.integer(kept$ncat), 3L)
  expect_equal(kept$cat[[2]], 2L)
})
