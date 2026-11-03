library(profylr)

comparison_columns <- c(
  "model_id", "role", "wave", "model", "K", "covariance",
  "measurement_invariant", "means_invariant", "covariance_invariant", "transitions",
  "loglik", "n_parameters", "bic", "sabic", "aic", "ic_supported", "ic_reason",
  "n_best", "n_starts", "n_converged_starts", "converged", "status",
  "min_expected_n", "min_expected_proportion", "small_state_flag", "small_state_rule",
  "min_expected_transitions", "transition_note",
  "entropy_wave", "entropy_sequence", "mean_max_posterior", "assignment_note",
  "blrt", "blrt_reason", "lmr", "lmr_reason",
  "n_boundary", "regularized", "min_eigenvalue", "boundary_warning", "regularization_warning",
  "reliable", "unreliable_reason", "inference_warning", "criterion_note", "selected", "selection_rationale",
  "comparable", "incomparable"
)

gaussian_sample <- function() {
  set.seed(1)
  n <- 100
  y1 <- rnorm(n)
  data.frame(y1 = y1, y2 = 0.6 * y1 + rnorm(n, 0.2))
}

fit_args <- list(n_starts = 2L, max_iter = 80L, tol = 1e-8, screen_iter = 0L, seed = 1)

one_state_grid <- function() {
  do.call(fit_grid, c(list(gaussian_sample(), c("y1", "y2"), n_states = 1L, covariance = c("EEI", "VVV")), fit_args))
}

test_that("compare_models stays a report and select_model applies the rule", {
  for (fn in c("fit_lpa", "fit_lca", "fit_lta", "fit_grid", "compare_models", "enumerate_waves")) {
    args <- names(formals(fn))
    expect_false("select_best" %in% args)
    expect_false("auto" %in% args)
    expect_false(any(grepl("recommended", args)))
  }
  expect_true("auto" %in% names(formals(select_model)))
  grid <- one_state_grid()
  tab <- grid$comparison
  expect_equal(names(tab), comparison_columns)
  expect_false(any(tab$selected))
  expect_true(all(tab$selection_rationale == ""))
  expect_false(any(grepl("recommended|select_best|best_k", names(tab))))
  expect_false("recommended_k" %in% names(grid))
  sel <- select_model(grid)
  expect_false(any(sel$comparison$selected))
  expect_equal(names(sel$comparison), comparison_columns)
  expect_false(any(sel$candidates$selected & !sel$candidates$hard_pass))
  expect_false(any(sel$candidates$selected & sel$candidates$n_best < 2))
  if (any(sel$candidates$selected)) {
    hit <- sel$candidates[sel$candidates$selected, ]
    expect_true(hit$second_seed_abs_gap <= hit$replicate_tol)
    expect_equal(hit$rejection_reason, "")
  }
  expect_false(any(sel$disagreement$selects))
  expect_false(any(grepl("n_criteria|favoring", names(sel$disagreement))))
  expect_false(any(grepl("n_criteria|favoring", names(sel$comparison))))
})

test_that("indices are not counted as votes", {
  tab <- one_state_grid()$comparison
  expect_false(any(grepl("n_criteria|favoring", names(tab))))
  expect_false(any(grepl("criteria favoring|[0-9]+ criteria", tab$criterion_note)))
  expect_true(all(tab$criterion_note == tab$criterion_note[[1]]))
})

test_that("a small class is reported and flagged only when a threshold is passed", {
  grid <- one_state_grid()
  tab <- grid$comparison
  expect_true(all(is.finite(tab$min_expected_n)))
  expect_true(all(is.finite(tab$min_expected_proportion)))
  expect_false(any(tab$small_state_flag))
  expect_true(all(tab$small_state_rule == ""))
  expect_false(any(grepl("small_class", tab$unreliable_reason)))
  flagged <- compare_models(grid$fits, min_proportion = 2)
  expect_equal(nrow(flagged), nrow(tab))
  expect_true(all(flagged$small_state_flag))
  expect_match(flagged$small_state_rule[[1]], "min_proportion=2")
})

test_that("changing entropy leaves eligibility, order, and the other columns unchanged", {
  grid <- one_state_grid()
  base <- grid$comparison
  expect_equal(base$model_id, seq_len(nrow(base)))
  expect_equal(base$covariance, c("EEI", "VVV"))
  expect_match(base$assignment_note[[1]], "not used to choose")
  clones <- grid$fits
  clones[[1]]$result$entropy_wave <- 0.01
  clones[[1]]$result$entropy_sequence <- 0.02
  clones[[2]]$result$entropy_wave <- 0.99
  clones[[2]]$result$entropy_sequence <- 0.98
  again <- compare_models(clones)
  expect_equal(again$entropy_wave, c(0.01, 0.99))
  expect_equal(again$model_id, base$model_id)
  keep <- setdiff(names(base), c("entropy_wave", "entropy_sequence"))
  expect_equal(again[keep], base[keep])
  flipped <- compare_models(list(clones[[2]], clones[[1]]))
  expect_equal(flipped$entropy_wave, c(0.99, 0.01))
  expect_equal(flipped$model_id, c(1, 2))
  expect_equal(flipped$reliable, again$reliable)
  expect_equal(flipped$unreliable_reason, again$unreliable_reason)
  expect_equal(flipped$inference_warning, again$inference_warning)
  injected <- base
  injected$entropy_wave <- c(0.01, 0.99)
  injected$entropy_sequence <- c(0.02, 0.98)
  expect_equal(injected[keep], base[keep])
  expect_false(any(base$selected))
})

test_that("record_selection stores the spec, rationale, timestamp and snapshot", {
  tab <- one_state_grid()$comparison
  text <- "Equal diagonal covariance matches the study design."
  selection <- record_selection(tab, chosen = tab$model_id[[which.max(tab$bic)]], rationale = text)
  expect_s3_class(selection, "profylr_selection")
  expect_equal(selection$format_version, 1L)
  expect_equal(selection$rationale, text)
  expect_match(selection$timestamp, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
  expect_equal(selection$chosen$covariance, tab$covariance[[which.max(tab$bic)]])
  expect_equal(selection$chosen$K, 1)
  expect_equal(nrow(selection$comparison), nrow(tab))
  expect_equal(sum(selection$comparison$selected), 1)
  expect_false(any(tab$selected))
  expect_true(all(tab$selection_rationale == ""))
  path <- tempfile(fileext = ".rds")
  save_model(selection, path)
  loaded <- load_model(path)
  expect_equal(loaded$rationale, text)
  expect_equal(loaded$timestamp, selection$timestamp)
  expect_equal(loaded$chosen$K, selection$chosen$K)
  expect_equal(loaded$chosen$covariance, selection$chosen$covariance)
  report <- report_model(loaded)
  expect_match(report, text, fixed = TRUE)
  expect_match(report, selection$timestamp, fixed = TRUE)
  expect_match(report, "K=1")
  expect_true(isTRUE(selection$eligible_for_downstream$model_comparison))
  expect_true(isTRUE(selection$eligible_for_downstream$measurement))
  expect_equal(selection$eligibility_detail, "")
  expect_match(report_model(selection, procedure = "measurement"), "Eligible for measurement")
  expect_match(report_model(tab), "No model has been selected")
  expect_error(record_selection(tab, tab$model_id[[1]], "   "), "rationale")
})

test_that("an unreplicated start is recorded and is not validated", {
  fit <- fit_lpa(gaussian_sample(), c("y1", "y2"), n_states = 1, covariance = "VVV",
                 n_starts = 1, max_iter = 40, seed = 1)
  tab <- compare_models(fit)
  expect_true(tab$reliable[[1]])
  expect_equal(tab$unreliable_reason[[1]], "")
  expect_match(tab$inference_warning[[1]], "replication_not_assessed")
  selection <- record_selection(tab, 1, "A single start is not enough.")
  expect_equal(selection$rationale, "A single start is not enough.")
  expect_false(selection$eligible_for_downstream$model_comparison)
  expect_true(selection$eligible_for_downstream$measurement)
  expect_match(selection$eligibility_detail, "replication_not_assessed")
  expect_match(report_model(selection), "not an endorsement")
  expect_error(report_model(selection, procedure = "model_comparison"), "not eligible")
})

test_that("the default grid crosses the four Release 1 covariance structures", {
  expect_equal(eval(formals(fit_grid)$covariance), c("EEI", "VVI", "EEE", "VVV"))
  expect_error(
    fit_grid(gaussian_sample(), c("y1", "y2"), n_states = 1, covariance = "EII", n_starts = 1, max_iter = 5),
    "not in Release 1"
  )
})

test_that("a categorical grid is not repeated once per covariance", {
  data <- data.frame(u = rep(0:1, 40), v = rep(c(0, 0, 1, 1), 20))
  grid <- fit_grid(data, c("u", "v"), model = "lca", n_states = 1:2,
                   n_starts = 2, max_iter = 25, screen_iter = 5, n_full = 2, seed = 1)
  expect_equal(nrow(grid$comparison), 2)
  expect_true(all(is.na(grid$comparison$covariance)))
  expect_false(any(grid$comparison$selected))
})

test_that("per-wave enumeration is a diagnostic table per wave", {
  set.seed(2)
  long <- rbind(
    data.frame(id = 1:50, wave = 1, y = rnorm(50, -0.4)),
    data.frame(id = 1:50, wave = 2, y = rnorm(50, 0.5))
  )
  enum <- enumerate_waves(long, "y", id = "id", wave = "wave", n_states = 1:2, covariance = "EEI",
                          n_starts = 2, max_iter = 30, screen_iter = 5, n_full = 2, seed = 2)
  expect_s3_class(enum, "profylr_wave_enumeration")
  expect_false(any(grepl("recommended", names(enum))))
  expect_equal(names(enum$tables), c("1", "2"))
  expect_true(all(vapply(enum$tables, nrow, integer(1)) == 2L))
  expect_true(all(vapply(enum$tables, function(table) all(table$role == "wave_diagnostic"), logical(1))))
  expect_match(enum$note, "Extra states can compensate for wrongly imposed invariance", fixed = TRUE)
  expect_match(enum$tables[[1]]$diagnostic_note[[1]], "Extra states can compensate for wrongly imposed invariance", fixed = TRUE)
  recorded <- record_selection(enum, 1, "Wave 1 looks like one profile.")
  expect_false(recorded$eligible_for_downstream$model_comparison)
  expect_match(recorded$eligibility_detail, "wave_diagnostic")
  expect_equal(recorded$rationale, "Wave 1 looks like one profile.")
  still <- record_selection(enum$tables[[1]], enum$tables[[1]]$model_id[[1]], "Still not joint.")
  expect_false(still$eligible_for_downstream$model_comparison)
  expect_match(still$eligibility_detail, "wave_diagnostic")
})

test_that("the fit-criterion plot is labelled as descriptive", {
  grid <- fit_grid(gaussian_sample(), c("y1", "y2"), n_states = 1:2, covariance = "VVV",
                   n_starts = 2, max_iter = 40, screen_iter = 5, n_full = 2, seed = 1)
  plot <- plot_fit_criteria(grid)
  expect_s3_class(plot, "ggplot")
  expect_match(plot$labels$title, "Descriptive")
  expect_match(plot$labels$subtitle, "not a selection")
  expect_match(plot$labels$subtitle, "Entropy")
})

stub_fit <- function(overrides = list()) {
  result <- modifyList(list(
    status_reason = "ok", converged = TRUE, loglik = -10, n_starts = 2L, n_best = 2L,
    regularized = FALSE, n_boundary = 0L, min_eigenvalue = NA_real_,
    expected_membership = c(10, 10), expected_transitions = numeric(),
    ic_supported = TRUE, covariance = "", K = 2L,
    measurement_invariant = TRUE, means_invariant = TRUE, covariance_invariant = TRUE,
    n_waves = 1L, transition_equal = TRUE, n_parameters = 3L,
    bic = 1, sabic = 1, aic = 1, n_converged_starts = 2L, status = "ok",
    n_analyzed = 20L, entropy_wave = 0.4, entropy_sequence = 0.4, gamma = c(0.8, 0.2)
  ), overrides)
  structure(list(result = result, model = "lca"), class = "profylr_fit")
}

test_that("numerical degeneracy blocks downstream inference", {
  nonfinite <- compare_models(stub_fit(list(
    loglik = NaN, status = "unusable", status_reason = "numerical_failure", ic_supported = FALSE
  )))
  expect_false(nonfinite$reliable[[1]])
  expect_match(nonfinite$unreliable_reason[[1]], "numerical_failure")
  kept <- record_selection(nonfinite, 1, "The likelihood is not finite and the choice stays on the record.")
  expect_false(kept$eligible_for_downstream$assignment)
  expect_match(kept$eligibility_detail, "numerical_failure")
  expect_error(report_model(kept, procedure = "assignment"), "not eligible")

  near <- compare_models(stub_fit(list(min_eigenvalue = 1e-9, covariance = "VVV")))
  expect_true(near$reliable[[1]])
  expect_equal(near$unreliable_reason[[1]], "")
  expect_false(grepl("near_singular", near$unreliable_reason[[1]]))
  expect_false(grepl("variance_floor", near$inference_warning[[1]]))
})

test_that("a numerically empty state warns and limits procedures", {
  tab <- compare_models(stub_fit(list(
    status = "warned", status_reason = "numerically_empty_state", ic_supported = FALSE,
    expected_membership = c(20, 1e-12)
  )))
  expect_true(tab$reliable[[1]])
  expect_match(tab$inference_warning[[1]], "numerically_empty_state")
  expect_false(grepl("boundary", tab$inference_warning[[1]]))
  kept <- record_selection(tab, 1, "One state has essentially no mass. The choice stays on the record.")
  expect_equal(kept$chosen$model_id, 1)
  expect_true(kept$eligible_for_downstream$assignment)
  expect_false(kept$eligible_for_downstream$measurement)
  expect_false(kept$eligible_for_downstream$model_comparison)
  expect_match(kept$eligibility_detail, "numerically_empty_state")
})

test_that("a boundary probability warns and does not fail the fit", {
  tab <- compare_models(stub_fit(list(
    n_boundary = 2L, status = "warned", status_reason = "boundary_parameters"
  )))
  expect_true(tab$reliable[[1]])
  expect_equal(tab$status[[1]], "warned")
  expect_equal(tab$unreliable_reason[[1]], "")
  expect_match(tab$inference_warning[[1]], "boundary_parameters")
  expect_match(tab$boundary_warning[[1]], "boundary parameter estimates")
  kept <- record_selection(tab, 1, "A response probability sits on the boundary. I am keeping this class solution.")
  expect_equal(kept$eligibility_detail, "")
  expect_match(kept$eligibility_notes, "boundary_parameters")
  expect_true(all(unlist(kept$eligible_for_downstream)))
  expect_match(report_model(kept), "not an endorsement")
  expect_match(report_model(kept, procedure = "measurement"), "Eligible for measurement")
})

test_that("a variance floor warns and does not fail the fit", {
  tab <- compare_models(stub_fit(list(
    regularized = TRUE, min_eigenvalue = 1e-12, covariance = "EEI",
    ic_supported = FALSE, status = "warned", status_reason = "variance_floor"
  )))
  expect_true(tab$reliable[[1]])
  expect_false(grepl("near_singular", tab$unreliable_reason[[1]]))
  expect_match(tab$inference_warning[[1]], "variance_floor")
  kept <- record_selection(tab, 1, "The covariance was ridged onto the variance floor.")
  expect_false(kept$eligible_for_downstream$model_comparison)
  expect_true(kept$eligible_for_downstream$measurement)
  expect_match(kept$eligibility_detail, "variance_floor")
})

test_that("problematic choices are recorded with their own reasons", {
  fits <- list(
    stub_fit(),
    stub_fit(list(n_starts = 1L, n_best = 1L, status = "warned", status_reason = "replication_not_assessed")),
    stub_fit(list(converged = FALSE, n_best = 0L, status = "unusable", status_reason = "not_converged", ic_supported = FALSE)),
    stub_fit(list(status = "warned", status_reason = "numerically_empty_state", ic_supported = FALSE)),
    stub_fit(list(n_boundary = 1L, status = "warned", status_reason = "boundary_parameters"))
  )
  tab <- compare_models(fits, role = c(rep("candidate", 4), "wave_diagnostic"))
  expected <- list(
    list(id = 1, ok = TRUE, reason = ""),
    list(id = 2, ok = FALSE, reason = "replication_not_assessed"),
    list(id = 3, ok = FALSE, reason = "not_converged"),
    list(id = 4, ok = FALSE, reason = "numerically_empty_state"),
    list(id = 5, ok = FALSE, reason = "wave_diagnostic")
  )
  for (item in expected) {
    selection <- record_selection(tab, item$id, paste("Keeping model", item$id, "for the record."))
    expect_equal(selection$chosen$model_id, item$id)
    expect_equal(isTRUE(selection$eligible_for_downstream$model_comparison), item$ok)
    if (nzchar(item$reason)) expect_match(selection$eligibility_detail, item$reason, fixed = TRUE)
    if (!item$ok) expect_error(report_model(selection, procedure = "model_comparison"), "not eligible")
  }
  boundary <- record_selection(tab, 5, "Boundary and diagnostic, still recorded.")
  expect_match(boundary$eligibility_detail, "wave_diagnostic")
  expect_true(boundary$eligible_for_downstream$measurement)
  expect_match(boundary$eligibility_notes, "boundary_parameters")
  clean <- record_selection(tab, 1, "This one replicated and did not hit a boundary.")
  expect_match(report_model(clean, procedure = "measurement"), "Eligible for measurement")
})
