library(profylr)

train_frame <- function() {
  data.frame(
    id = seq_len(12),
    wave = 1L,
    u = rep(c("a", "b", "c"), 4),
    stringsAsFactors = FALSE
  )
}

test_that("prediction reuses the training category order", {
  data <- train_frame()
  trained <- prepare_panel(data, "u", "id", "wave", family = list(u = "nominal"))
  expect_equal(as.character(trained$level_order[[1]]), c("a", "b", "c"))
  subset <- data[data$u != "b", ]
  subset <- subset[rev(seq_len(nrow(subset))), ]
  predicted <- prepare_panel(subset, "u", "id", "wave", family = list(u = "nominal"), schema = trained)
  codes <- predicted$cat[predicted$cat >= 0]
  expect_equal(sort(unique(codes)), c(0L, 2L))
  one <- data[data$u == "b", ]
  single <- prepare_panel(one, "u", "id", "wave", family = list(u = "nominal"), schema = trained)
  expect_equal(unique(single$cat[single$cat >= 0]), 1L)
  unseen <- data
  unseen$u[1] <- "z"
  expect_error(
    prepare_panel(unseen, "u", "id", "wave", family = list(u = "nominal"), schema = trained),
    "unseen category"
  )
})

test_that("duplicate keys, missing keys, and bad schedules are rejected", {
  data <- train_frame()
  dup <- rbind(data[1, ], data[1, ])
  expect_error(prepare_panel(dup, "u", "id", "wave"), "duplicate subject-wave key")
  missing <- data
  missing$id[1] <- NA
  expect_error(prepare_panel(missing, "u", "id", "wave"), "missing key")
  expect_error(prepare_panel(data, "u", "person", "wave"), "missing key column")
  waves <- data.frame(id = c(1L, 1L), wave = c(1, 3), u = c(0, 1))
  expect_error(prepare_panel(waves, "u", "id", "wave", schedule = c(1, 2, 2)), "duplicate schedule entry")
  expect_error(prepare_panel(waves, "u", "id", "wave", schedule = c(1, 1.5)), "fractional schedules are rejected")
  fractional <- waves
  fractional$wave <- c(1, 1.5)
  expect_error(prepare_panel(fractional, "u", "id", "wave"), "fractional schedules are rejected")
  outside <- waves
  outside$wave <- c(1, 4)
  expect_error(prepare_panel(outside, "u", "id", "wave", schedule = c(1, 2, 3)), "not in the schedule")
})

test_that("integer wave codes expand across the closed interval", {
  data <- data.frame(id = c(1L, 1L), wave = c(1, 3), u = c(0, 1))
  panel <- prepare_panel(data, "u", "id", "wave", family = list(u = "bernoulli"))
  expect_equal(as.integer(panel$waves), 1:3)
  expect_equal(panel$T, 3L)
})

test_that("an impossible subject keeps the total log-likelihood at -Inf", {
  panel <- list(
    N = 3L, T = 1L, J = 1L, measurement = "categorical",
    family = "bernoulli", ncat = 2L, Rmax = 2L,
    cat = c(0L, -1L, 1L), gauss = NULL,
    ids = 1:3, waves = 1L, indicators = "u"
  )
  spec <- lca_spec(1L, n_starts = 1L, align_labels = FALSE)
  params <- list(delta = 1, tau = numeric(), rho = c(1, 0))
  ll <- profylr:::cpp_loglik(panel, spec, params)
  expect_true(is.infinite(ll) && ll < 0)
  finite_panel <- panel
  finite_panel$cat <- c(0L, 0L, 0L)
  finite_ll <- profylr:::cpp_loglik(finite_panel, spec, params)
  expect_true(is.finite(finite_ll))
})
