library(profylr)

recorded <- list(
  c(-19133.760102125754, -19133.767117735992),
  c(-191327.57629785853, -191327.58358255043)
)

test_that("the recorded 0.007 pairs replicate and a gap of 0.5 does not", {
  spec <- lta_spec(2)
  expect_equal(spec$replicate_tol, 0.1)
  for (pair in recorded) {
    expect_true(abs(pair[[1]] - pair[[2]]) > 1e-3)
    expect_true(logliks_replicate(pair[[1]], pair[[2]]))
  }
  expect_false(logliks_replicate(-10, -10.5))
  expect_false(logliks_replicate(NA_real_, -10))
  expect_false(logliks_replicate(-10, -10.05, replicate_tol = 0.01))
  expect_equal(lca_spec(2)$replicate_tol, 0.1)
  expect_equal(lta_spec(2, replicate_tol = 0.01)$replicate_tol, 0.01)
})

test_that("BLRT acceptance ignores status and the flag follows n_best", {
  stub <- function(n_best, status) {
    list(result = list(
      loglik = -10, converged = TRUE, admissible = TRUE,
      n_best = as.integer(n_best), status = status
    ))
  }
  unreplicated <- profylr:::.blrt_solution(stub(1L, "unusable"))
  replicated <- profylr:::.blrt_solution(stub(2L, "ok"))
  expect_true(unreplicated$usable)
  expect_true(replicated$usable)
  expect_false(unreplicated$replicated)
  expect_true(replicated$replicated)
  expect_equal(unreplicated$loglik, replicated$loglik)
})
