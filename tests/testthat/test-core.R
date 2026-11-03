library(profylr)

test_that("the core pins Eigen and leaves draws empty", {
  info <- build_info()
  expect_equal(info$eigen_version, "3.4.0")
  expect_true(info$eigen_dont_parallelize)
  expect_false(info$external_blas)
  expect_false(grepl("ffast-math", info$flags, fixed = TRUE))
  expect_true(grepl("fast-math=off", info$flags, fixed = TRUE))
})

test_that("the hand likelihood is log 0.2100", {
  data <- data.frame(id = c(1, 1), wave = c(1, 2), u = c(1, 1))
  panel <- prepare_panel(data, "u", "id", "wave", family = list(u = "bernoulli"))
  spec <- lca_spec(2, n_starts = 1, align_labels = FALSE)
  ll <- profylr:::cpp_loglik(panel, spec, list(
    delta = c(0.6, 0.4), tau = c(0.7, 0.3, 0.2, 0.8), rho = c(0.8, 0.2, 0.3, 0.7)
  ))
  expect_equal(ll, log(0.2100), tolerance = 1e-10)
})

test_that("a two-state Gaussian likelihood matches the hand sum", {
  data <- data.frame(id = c(1, 1), wave = c(1, 2), y = c(0, 0.5))
  panel <- prepare_panel(data, "y", "id", "wave", family = list(y = "gaussian"))
  spec <- lpa_spec(2, covariance = "EEI", align_labels = FALSE)
  ll <- profylr:::cpp_loglik(panel, spec, list(
    delta = c(0.7, 0.3), tau = c(0.8, 0.2, 0.4, 0.6),
    means = c(-1, 1), cov = 1
  ))
  expect_equal(ll, -3.024718794919309, tolerance = 1e-10)
})

test_that("free transitions through a missing wave match the hand sum", {
  data <- data.frame(id = c(1, 1, 1), wave = c(1, 2, 3), u = c(1, NA, 0))
  panel <- prepare_panel(data, "u", "id", "wave", family = list(u = "bernoulli"))
  spec <- lca_spec(2, transitions = "free", align_labels = FALSE)
  ll <- profylr:::cpp_loglik(panel, spec, list(
    delta = c(0.6, 0.4),
    tau = c(0.9, 0.1, 0.2, 0.8, 0.3, 0.7, 0.4, 0.6),
    rho = c(0.8, 0.2, 0.3, 0.7)
  ))
  expect_equal(ll, -1.6513021165327992, tolerance = 1e-10)
})

test_that("a missing integer wave is not collapsed", {
  data <- data.frame(id = c(1, 1), wave = c(1, 3), u = c(1, 1))
  panel <- prepare_panel(data, "u", "id", "wave", family = list(u = "bernoulli"))
  expect_equal(panel$T, 3)
  spec <- lca_spec(2, align_labels = FALSE)
  params <- list(delta = c(0.6, 0.4), tau = c(0.7, 0.3, 0.2, 0.8), rho = c(0.8, 0.2, 0.3, 0.7))
  gapped <- profylr:::cpp_loglik(panel, spec, params)
  adjacent <- data
  adjacent$wave <- c(1, 2)
  other <- profylr:::cpp_loglik(prepare_panel(adjacent, "u", "id", "wave", family = list(u = "bernoulli")), spec, params)
  expect_gt(abs(gapped - other), 1e-6)
})

test_that("ordinal, count, covariate, and distal requests fail clearly", {
  expect_error(profylr:::cpp_parse_family("ordinal"), "Release 2")
  expect_error(profylr:::cpp_parse_family("poisson"), "not available")
  data <- data.frame(y = 0.2)
  expect_error(fit_lpa(data, "y", n_states = 1, covariates = "x"), "Release 1")
  expect_error(estimate_distal("outcome", "binary", timing = "final"), "Release 3")
  expect_error(estimate_distal("outcome", "continuous", timing = "initial", method = "modal"), "Modal-assignment")
  expect_error(estimate_distal("outcome", "ordinal", timing = "wave"), "wave")
  expect_error(estimate_distal("outcome", "nominal", timing = "transition"), "from_state")
})

test_that("a mixed Gaussian and Bernoulli panel fits", {
  set.seed(1)
  n <- 80
  state <- rep(c(TRUE, FALSE), each = n / 2)
  data <- data.frame(
    y = ifelse(state, rnorm(n, -1.5, 0.4), rnorm(n, 1.5, 0.4)),
    u = as.integer(runif(n) < ifelse(state, 0.2, 0.8))
  )
  fit <- fit_lpa(data, c("y", "u"), n_states = 2, covariance = "EEI",
                 family = list(y = "gaussian", u = "bernoulli"),
                 n_starts = 3, screen_iter = 5, n_full = 2, max_iter = 40, seed = 1)
  expect_equal(fit$engine, "cpp-em")
  expect_null(fit$draws)
  expect_equal(fit$interval_type, "none")
  expect_equal(fit$result$parameter_uncertainty, "none")
  expect_equal(fit$result$state_uncertainty, "plugin_posterior")
  expect_equal(fit$result$n_gaussian, 1)
  expect_equal(nrow(glance(fit)), 1)
  expect_true("mean" %in% tidy(fit)$parameter)
  expect_true("response" %in% tidy(fit)$parameter)
  expect_equal(nrow(augment(fit)), 80)
  plot <- plot_profiles(fit)
  expect_s3_class(plot, "ggplot")
})
