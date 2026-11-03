library(profylr)

# The 36 shared-dataset draws whose Mplus mode is a small class.
# The bootstrap comparison budget (20 starts, screen 10, 4 full) must reach
# that mode. Package defaults stay 40/10/4. Replication is not required:
# reaching the likelihood is the check. n_best can be 1.

manifest_path <- profylr_fixture("blrt-small-class", "manifest.csv")

read_manifest <- function() {
  utils::read.csv(manifest_path, stringsAsFactors = FALSE)
}

fit_draw <- function(row) {
  path <- file.path(dirname(manifest_path), row$source, row$fixture, sprintf("draw_%03d.csv", row$draw))
  frame <- utils::read.csv(path, na.strings = "NA", check.names = FALSE, stringsAsFactors = FALSE)
  controls <- list(
    n_starts = 20L, screen_iter = 10L, n_full = 4L, max_iter = 200L,
    tol = 1e-7, replicate_tol = 1e-3, seed = as.numeric(row$seed),
    align_labels = FALSE, n_threads = 1L
  )
  if (row$fixture == "lca_bernoulli_k2") {
    do.call(fit_lca, c(list(frame, c("u1", "u2", "u3", "u4"), n_states = 2L), controls))
  } else {
    do.call(fit_lpa, c(list(frame, c("y1", "y2", "y3"), n_states = 2L, covariance = "EEI"), controls))
  }
}

test_that("the 36 small-class draws reach the Mplus mode at the bootstrap start budget", {
  manifest <- read_manifest()
  expect_equal(nrow(manifest), 36L)
  expect_equal(sum(manifest$fixture == "lpa_eei_k2"), 30L)
  expect_equal(sum(manifest$fixture == "lca_bernoulli_k2"), 6L)
  for (i in seq_len(nrow(manifest))) {
    row <- manifest[i, ]
    fit <- fit_draw(row)
    label <- paste(row$source, row$fixture, row$draw)
    expect_true(isTRUE(fit$result$admissible), info = label)
    expect_true(isTRUE(fit$result$converged), info = label)
    expect_gte(fit$result$loglik, row$seed_ll - 0.01)
    expect_gte(fit$result$loglik, row$mplus_h1_ll - 0.01)
    expect_equal(fit$result$loglik, row$core_ll, tolerance = 1e-6, info = label)
  }
})
