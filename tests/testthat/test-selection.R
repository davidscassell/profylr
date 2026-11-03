library(profylr)

candidate_columns <- c(
  "model_id", "model", "K", "covariance", "n_parameters", "n_waves", "n_analyzed", "loglik",
  "aic", "aic3", "bic", "sabic", "caic", "awe", "icl_bic",
  "delta_aic", "delta_aic3", "delta_bic", "delta_sabic", "delta_caic", "delta_awe", "delta_icl_bic",
  "bic_bf_vs_best", "bic_cmp",
  "min_class_prop", "min_class_n", "min_avepp",
  "entropy_wave", "entropy_sequence", "entropy_raw",
  "converged", "finite", "regularized", "positive_definite", "collapsed",
  "n_starts", "n_best", "replicate_tol",
  "second_seed_loglik", "second_seed_n_best", "second_seed_abs_gap", "second_seed_status",
  "hard_pass", "soft_pass", "eligible", "in_tie_zone",
  "rejection_reason", "selected"
)

bias_note <- c(
  aic = "AIC tends to over-select the number of states (Nylund, Asparouhov, and Muthen 2007).",
  aic3 = "AIC3 penalizes more than AIC and can still over-select relative to BIC (Bozdogan 1987; Nylund-Gibson and Choi 2018).",
  bic = "BIC can under-select in small samples and for latent transition models (Nylund, Asparouhov, and Muthen 2007; Edelsbrunner, Flaig, and Schneider 2022).",
  sabic = "SABIC can over-select relative to BIC in large cross-sectional samples (Nylund-Gibson and Choi 2018; Edelsbrunner, Flaig, and Schneider 2022).",
  caic = "CAIC is more conservative than BIC and can under-select (Bozdogan 1987; Nylund-Gibson and Choi 2018).",
  awe = "AWE is conservative and can under-select the number of states (Banfield and Raftery 1993; Nylund-Gibson and Choi 2018).",
  icl_bic = "ICL-BIC adds a classification-entropy penalty and can under-select when classes overlap (Biernacki, Celeux, and Govaert 2000). It is a diagnostic and is not used for automatic ranking."
)

published <- list(
  n_starts = 30L, n_full = 8L, screen_iter = 12L, max_iter = 250L,
  tol = 1e-8, seed = 20261025, replicate_tol = 1e-4, n_threads = 1L
)

ll_for_bic <- function(bic, p, n) (p * log(n) - bic) / 2

stub_fit <- function(K = 2L, ll = -100, p = 8L, n = 200L, covariance = "EEI",
                     model = "lpa", n_waves = 1L, converged = TRUE, regularized = FALSE,
                     pd = TRUE, collapsed = FALSE, n_starts = 4L, n_best = 2L,
                     entropy_wave = 0.9, entropy_sequence = 0.9,
                     entropy_wave_raw = 0, entropy_sequence_raw = 0,
                     high = 0.95, n_subjects = 8L, ic_supported = TRUE,
                     loglik_finite = TRUE, membership = NULL) {
  if (is.null(membership)) membership <- rep(n / K, n_waves * K)
  gamma <- numeric(n_subjects * n_waves * K)
  idx <- 0L
  for (i in seq_len(n_subjects)) for (t in seq_len(n_waves)) {
    modal <- ((i - 1L) %% K) + 1L
    rest <- if (K == 1L) 0 else (1 - high) / (K - 1L)
    for (state in seq_len(K)) {
      idx <- idx + 1L
      gamma[[idx]] <- if (K == 1L) 1 else if (state == modal) high else rest
    }
  }
  result <- list(
    K = as.integer(K),
    loglik = if (loglik_finite) as.numeric(ll) else NA_real_,
    n_parameters = as.integer(p),
    n_analyzed = as.integer(n),
    n_subjects = as.integer(n_subjects),
    n_waves = as.integer(n_waves),
    covariance = if (is.null(covariance) || !nzchar(covariance)) "" else covariance,
    ic_supported = isTRUE(ic_supported),
    aic = NA_real_, bic = NA_real_, sabic = NA_real_,
    converged = isTRUE(converged),
    regularized = isTRUE(regularized),
    cov_positive_definite = pd,
    cov_collapsed_component = isTRUE(collapsed),
    n_starts = as.integer(n_starts),
    n_best = as.integer(n_best),
    n_converged_starts = as.integer(n_best),
    status = if (isTRUE(converged)) "ok" else "unusable",
    status_reason = "",
    entropy_wave = entropy_wave,
    entropy_sequence = entropy_sequence,
    entropy_wave_raw = entropy_wave_raw,
    entropy_sequence_raw = entropy_sequence_raw,
    expected_membership = as.numeric(membership),
    expected_transitions = numeric(),
    gamma = gamma,
    n_boundary = 0L,
    min_eigenvalue = 1,
    measurement_invariant = TRUE,
    means_invariant = TRUE,
    covariance_invariant = TRUE,
    transition_equal = TRUE
  )
  structure(list(
    result = result, panel = NULL,
    spec = list(replicate_tol = 1e-3, seed = 1, n_starts = as.integer(n_starts)),
    model = model
  ), class = "profylr_fit")
}

choose <- function(fits, rule = selection_rule(), auto = TRUE, second_seed_loglik = NULL,
                   second_seed_n_best = NULL, waves = "auto") {
  if (is.null(second_seed_loglik)) {
    second_seed_loglik <- vapply(fits, function(fit) {
      ll <- fit$result$loglik
      if (length(ll) != 1L || !is.finite(ll)) 0 else ll
    }, numeric(1))
    if (is.null(second_seed_n_best)) second_seed_n_best <- rep(2L, length(fits))
  }
  select_model(fits, rule = rule, auto = auto, second_seed_loglik = second_seed_loglik,
               second_seed_n_best = second_seed_n_best, waves = waves)
}

picked <- function(sel) sel$candidates[sel$candidates$selected, ]

test_that("AvePP, OCC, and raw entropy match a brute-force posterior", {
  gamma <- c(0.9, 0.1, 0.8, 0.2, 0.3, 0.7, 0.1, 0.9)
  fit <- stub_fit(K = 2L, n = 4L, n_subjects = 4L, p = 4L, ll = -10, membership = c(2, 2),
                  entropy_wave_raw = NA_real_, entropy_sequence_raw = NA_real_)
  fit$result$gamma <- gamma
  diag <- classification_diagnostics(fit)
  expect_equal(diag$avepp, c(0.85, 0.80))
  expect_equal(diag$avepp_matrix[1, ], c(0.85, 0.15))
  expect_equal(diag$avepp_matrix[2, ], c(0.20, 0.80))
  expect_equal(diag$min_avepp, 0.80)
  expect_equal(diag$occ[[1]], (0.85 / 0.15) / (0.5 / 0.5))
  ic <- information_criteria(fit)
  expect_equal(ic$entropy_raw, 1.761432672375978, tolerance = 1e-12)
  expect_equal(ic$icl_bic, ic$bic + 2 * ic$entropy_raw, tolerance = 1e-10)

  zeros <- stub_fit(K = 2L, n = 2L, n_subjects = 2L, membership = c(1, 1),
                    entropy_wave_raw = NA_real_, entropy_sequence_raw = NA_real_)
  zeros$result$gamma <- c(1, 0, 0, 1)
  zero_diag <- classification_diagnostics(zeros)
  expect_equal(information_criteria(zeros)$entropy_raw, 0, tolerance = 1e-12)
  expect_equal(zero_diag$occ[[1]], Inf)

  wave <- stub_fit(K = 2L, n = 2L, n_subjects = 2L, n_waves = 2L, model = "lta",
                   membership = c(1, 1, 1, 1),
                   entropy_wave_raw = NA_real_, entropy_sequence_raw = NA_real_)
  wave$result$gamma <- c(0.9, 0.1, 0.6, 0.4, 0.2, 0.8, 0.3, 0.7)
  by <- classification_diagnostics(wave)$by_wave
  expect_equal(by$avepp[by$wave == 1], c(0.9, 0.8))
  expect_equal(by$avepp[by$wave == 2], c(0.6, 0.7))
  expect_equal(classification_diagnostics(wave)$min_avepp, 0.75)

  lta <- stub_fit(K = 2L, n = 200L, n_waves = 2L, model = "lta", p = 5L, ll = -100,
                  entropy_wave_raw = 1, entropy_sequence_raw = 5)
  ic_lta <- information_criteria(lta)
  expect_equal(ic_lta$entropy_raw, 5)
  expect_equal(ic_lta$icl_bic, ic_lta$bic + 10, tolerance = 1e-10)
  expect_gt(abs(ic_lta$icl_bic - (ic_lta$bic + 2)), 1)
})

test_that("AIC3, CAIC, AWE, and ICL match the Mplus printed information criteria", {
  check_printed <- function(ll, p, n, aic, bic, sabic) {
    fit <- stub_fit(K = 2L, ll = ll, p = p, n = n, covariance = "", model = "lca",
                    entropy_wave_raw = 0, entropy_sequence_raw = 0)
    ic <- information_criteria(fit)
    dev <- -2 * ll
    expect_equal(ic$aic, aic, tolerance = 1e-6)
    expect_equal(ic$bic, bic, tolerance = 1e-3)
    expect_equal(ic$sabic, sabic, tolerance = 1e-3)
    expect_equal(ic$aic3, dev + 3 * p, tolerance = 1e-8)
    expect_equal(ic$caic, dev + p * (log(n) + 1), tolerance = 1e-8)
    expect_equal(ic$awe, dev + 2 * p * (log(n) + 1.5), tolerance = 1e-8)
    expect_equal(ic$icl_bic, ic$bic, tolerance = 1e-8)
    blank <- fit
    blank$result$ic_supported <- FALSE
    expect_true(all(is.na(unlist(information_criteria(blank)[c("aic", "bic", "icl_bic")]))))
  }
  check_printed(-965.244, 9, 500, 1948.488, 1986.420, 1957.853)
  check_printed(-3177.162, 13, 500, 6380.324, 6435.114, 6393.851)
  check_printed(-3054.963, 24, 500, 6157.926, 6259.077, 6182.899)
})

test_that("the rule rejects inadmissible and unreplicated cells", {
  eligible <- stub_fit(ll = -50, p = 4L)
  worse <- stub_fit(ll = -30, p = 8L)
  fits <- list(
    eligible,
    stub_fit(ll = -30, p = 8L, converged = FALSE),
    stub_fit(ll = -30, p = 8L, collapsed = TRUE),
    stub_fit(ll = -30, p = 8L, n_best = 1L),
    stub_fit(ll = -30, p = 8L, regularized = TRUE),
    stub_fit(ll = -30, p = 8L, pd = FALSE),
    stub_fit(ll = -30, p = 8L, loglik_finite = FALSE),
    worse
  )
  seeds <- c(-50, -30, -30, -30, -30, -30, -30, -10)
  sel <- choose(fits, second_seed_loglik = seeds, second_seed_n_best = rep(2L, 8L))
  expect_equal(names(sel$candidates), candidate_columns)
  expect_equal(picked(sel)$loglik, -50)
  expect_true(picked(sel)$hard_pass)
  expect_equal(picked(sel)$rejection_reason, "")
  reasons <- sel$candidates$rejection_reason
  expect_equal(reasons[[2]], "hard:not_converged")
  expect_equal(reasons[[3]], "hard:collapsed")
  expect_equal(reasons[[4]], "hard:best_loglik_not_replicated")
  expect_equal(reasons[[5]], "hard:regularized")
  expect_equal(reasons[[6]], "hard:not_positive_definite")
  expect_equal(reasons[[7]], "hard:non_finite,hard:information_criterion_undefined")
  expect_equal(reasons[[8]], "hard:second_seed_not_reproduced")
  expect_false(any(grepl("second_seed", reasons[2:7])))
  expect_false(any(sel$comparison$selected))
  expect_equal(sum(sel$candidates$bic_cmp, na.rm = TRUE), 1, tolerance = 1e-8)
  best_delta <- is.finite(sel$candidates$delta_bic) & abs(sel$candidates$delta_bic) < 1e-8
  expect_true(all(abs(sel$candidates$bic_bf_vs_best[best_delta] - 1) < 1e-8))
  expect_gt(sel$candidates$delta_bic[[1]], 1)
  expect_match(sel$reason, "lowest BIC")
  expect_match(sel$reason, "positive definite")
  expect_match(sel$reason, "second seed")
  expect_match(sel$reason, "is not a filter")
  expect_match(sel$reason, "chosen jointly")
  expect_match(report_model(sel), "not an analyst record")
})

test_that("tie zone, primary criterion, and soft filters follow the documented rule", {
  small <- stub_fit(K = 1L, p = 5L, ll = ll_for_bic(101.5, 5, 200), membership = 200)
  large <- stub_fit(K = 2L, p = 10L, ll = ll_for_bic(100, 10, 200))
  tied <- choose(list(small, large), rule = selection_rule(tie_zone = 2))
  expect_equal(picked(tied)$K, 2L)
  expect_false(tied$ambiguous)
  expect_equal(tied$blrt$ambiguous_kind, "")
  expect_match(tied$reason, "lowest BIC")
  exact <- choose(list(small, large), rule = selection_rule(tie_zone = 0))
  expect_equal(picked(exact)$K, 2L)
  expect_equal(picked(exact)$model_id, picked(tied)$model_id)

  k2 <- stub_fit(K = 2L, p = 10L, n = 500L, ll = -100, membership = c(250, 250))
  k3 <- stub_fit(K = 3L, p = 20L, n = 500L, ll = -80, membership = c(200, 150, 150))
  vote <- choose(list(k2, k3))
  expect_equal(vote$primary, "bic")
  expect_equal(picked(vote)$K, 2L)
  by <- stats::setNames(vote$disagreement$K, vote$disagreement$index)
  expect_equal(as.integer(by[c("aic", "aic3", "sabic")]), c(3L, 3L, 3L))
  expect_equal(as.integer(by[c("bic", "caic", "awe", "icl_bic")]), c(2L, 2L, 2L, 2L))
  expect_false(any(vote$disagreement$selects))
  expect_false(any(grepl("n_criteria|favoring", names(vote$disagreement))))
  expect_equal(vote$disagreement$bias_note, unname(bias_note[vote$disagreement$index]))
  aic <- choose(list(k2, k3), rule = selection_rule(primary = "aic"))
  expect_equal(picked(aic)$K, 3L)
  expect_match(aic$reason, "lowest AIC")

  eei <- stub_fit(K = 2L, p = 5L, covariance = "EEI", ll = ll_for_bic(101, 5, 200))
  vvv <- stub_fit(K = 2L, p = 11L, covariance = "VVV", ll = ll_for_bic(100, 11, 200))
  joint <- choose(list(vvv, eei), rule = selection_rule(tie_zone = 2))
  expect_equal(picked(joint)$covariance, "VVV")
  expect_false(joint$ambiguous)
  expect_match(joint$reason, "lowest BIC")
  expect_match(joint$reason, "chosen jointly")

  low <- stub_fit(ll = ll_for_bic(100, 8, 200), entropy_wave = 0.4)
  high <- stub_fit(ll = ll_for_bic(110, 8, 200), entropy_wave = 0.9)
  kept <- choose(list(low, high))
  expect_equal(picked(kept)$entropy_wave, 0.4)
  filtered <- choose(list(low, high), rule = selection_rule(entropy_filter = TRUE, min_entropy = 0.8))
  expect_equal(picked(filtered)$entropy_wave, 0.9)
  expect_match(filtered$candidates$rejection_reason[[1]], "soft:entropy")

  sharp <- stub_fit(ll = ll_for_bic(90, 8, 200), high = 0.95)
  flat <- stub_fit(ll = ll_for_bic(80, 8, 200), high = 0.5)
  avepp <- choose(list(flat, sharp))
  expect_equal(picked(avepp)$loglik, flat$result$loglik)
  expect_false(grepl("soft:min_avepp", avepp$candidates$rejection_reason[1]))
  held <- choose(list(flat, sharp), rule = selection_rule(min_avepp = 0.70))
  expect_equal(picked(held)$min_avepp, classification_diagnostics(sharp)$min_avepp)
  expect_gte(picked(held)$min_avepp, 0.70)
  expect_match(held$candidates$rejection_reason[held$candidates$loglik == flat$result$loglik], "soft:min_avepp")
  expect_match(held$reason, "analyst threshold")

  tiny <- stub_fit(ll = ll_for_bic(80, 8, 200), membership = c(2, 198))
  big <- stub_fit(ll = ll_for_bic(100, 8, 200), membership = c(100, 100))
  sized <- choose(list(tiny, big))
  expect_equal(picked(sized)$loglik, tiny$result$loglik)
  expect_false(grepl("soft:", sized$candidates$rejection_reason[[1]]))
  free <- choose(list(tiny, big), rule = selection_rule(min_class_prop = 0.05, min_class_n = 30))
  expect_match(free$candidates$rejection_reason[[1]], "soft:min_class_prop")
  expect_match(free$candidates$rejection_reason[[1]], "soft:min_class_n")
  expect_equal(picked(free)$loglik, big$result$loglik)

  gap <- choose(list(stub_fit(ll = -40, p = 4L)), rule = selection_rule(require_second_seed = FALSE),
                second_seed_loglik = -1)
  expect_true(picked(gap)$hard_pass)
  expect_true(is.na(picked(gap)$second_seed_loglik))

  quiet <- choose(list(small, large), auto = FALSE)
  expect_equal(quiet$decision, "auto_disabled")
  expect_false(any(quiet$candidates$selected))
  expect_false(any(quiet$comparison$selected))
  expect_match(quiet$reason, "auto is FALSE")
  expect_true(all(is.na(quiet$candidates$second_seed_loglik)))

  none <- choose(list(stub_fit(converged = FALSE)))
  expect_equal(none$decision, "no_selection")
  expect_match(none$reason, "No selection")
  expect_match(none$reason, "no fallback")
  expect_true(is.na(none$selected_model_id))

  one <- stub_fit(K = 1L, p = 5L, ll = -100, n_waves = 2L, model = "lta", membership = c(200, 200))
  two <- stub_fit(K = 2L, p = 12L, ll = -90, n_waves = 2L, model = "lta")
  joint_lta <- choose(list(one, two))
  expect_equal(joint_lta$primary, "bic")
  expect_equal(picked(joint_lta)$K, 1L)
  bic <- choose(list(one, two), rule = selection_rule(primary = "bic"))
  expect_equal(picked(bic)$K, 1L)
  expect_error(choose(list(stub_fit(model = "lpa"), two)), "one model family")
  expect_error(selection_rule(primary = "waic"), "primary")
  expect_error(selection_rule(tie_zone = -1), "tie_zone")
  enum <- structure(list(), class = "profylr_wave_enumeration")
  expect_error(select_model(enum), "does not choose the joint")
})

test_that("plots mark the selected cell and do not present a vote", {
  fit <- stub_fit()
  sel <- choose(list(fit, stub_fit(K = 1L, p = 4L, ll = -80, membership = 200)))
  elbow <- plot_ic_elbow(sel)
  expect_s3_class(elbow, "ggplot")
  expect_match(elbow$labels$y, "ringed")
  dots <- plot_index_disagreement(sel)
  expect_s3_class(dots, "ggplot")
  expect_match(dots$labels$title, "not a vote")
  expect_s3_class(plot_avepp(fit), "ggplot")
  expect_s3_class(plot_max_posterior(fit), "ggplot")
  expect_s3_class(plot_prevalence(fit), "ggplot")
})

test_that("selection recovers a known two-class profile and leaves max_iter at 200", {
  expect_equal(lpa_spec(2)$max_iter, 200L)
  set.seed(11)
  n <- 240
  group <- rep(c(-2, 2), each = n / 2)
  data <- data.frame(y1 = group + rnorm(n, 0, 0.4), y2 = group + rnorm(n, 0, 0.4))
  fits <- lapply(1:3, function(k) {
    fit_lpa(data, c("y1", "y2"), n_states = k, covariance = "EEI",
            n_starts = 4L, screen_iter = 0L, n_full = 4L, max_iter = 80L, seed = 11)
  })
  sel <- select_model(fits)
  expect_equal(picked(sel)$K, 2L)
  expect_true(picked(sel)$hard_pass)
  expect_equal(picked(sel)$rejection_reason, "")
  expect_lte(picked(sel)$second_seed_abs_gap, picked(sel)$replicate_tol)
  expect_false(sel$blrt$ran)
  expect_false(sel$blrt$validated)
  expect_match(sel$blrt$reason, "Mplus TECH14")
  expect_match(sel$blrt$reason, "off by default")
  expect_false(grepl("digit match", sel$blrt$reason, fixed = TRUE))
  expect_false(grepl("bootstrap draws differ", sel$blrt$reason, fixed = TRUE))
  expect_match(sel$blrt$reason, "Independent bootstrap p-values are not required to match", fixed = TRUE)
  expect_match(sel$blrt$reason, "diagnostic", fixed = TRUE)
  expect_match(plot_profiles(fits[[2]])$labels$title, "Gaussian profiles")
  elbow <- plot_ic_elbow(sel)
  expect_s3_class(elbow, "ggplot")
  expect_match(elbow$labels$title, "Information criteria")
  expect_match(elbow$labels$y, "ringed")
  expect_match(plot_avepp(fits[[2]])$labels$title, "Average posterior")
  expect_match(plot_max_posterior(fits[[2]])$labels$title, "Assignment certainty")
  expect_match(plot_index_disagreement(sel)$labels$title, "not a vote")
  expect_error(plot_transitions(fits[[2]]), "one-wave")
})

test_that("a joint transition model warns when per-wave state counts differ", {
  set.seed(3)
  n <- 120
  id <- rep(seq_len(n), each = 2)
  wave <- rep(1:2, n)
  y1 <- numeric(2 * n)
  y2 <- numeric(2 * n)
  y1[wave == 1] <- rnorm(n, 0, 0.4)
  y2[wave == 1] <- rnorm(n, 0, 0.4)
  shift <- ifelse(id[wave == 2] <= n / 2, -2.5, 2.5)
  y1[wave == 2] <- shift + rnorm(n, 0, 0.4)
  y2[wave == 2] <- shift + rnorm(n, 0, 0.4)
  data <- data.frame(id = id, wave = wave, y1 = y1, y2 = y2)
  fits <- lapply(1:2, function(k) {
    fit_lta(data, c("y1", "y2"), id = "id", wave = "wave", n_states = k, covariance = "EEI",
            n_starts = 4L, screen_iter = 0L, n_full = 4L, max_iter = 60L, seed = 3)
  })
  sel <- select_model(fits)
  expect_true("configural_noninvariance_possible" %in% sel$warnings)
  expect_match(sel$reason, "configural noninvariance")
  expect_match(plot_transitions(fits[[2]])$labels$title, "Transition probabilities")
  expect_match(plot_profiles(fits[[2]])$labels$title, "Gaussian profiles")
  expect_match(plot_ic_elbow(sel)$labels$title, "Information criteria")
  expect_match(plot_avepp(fits[[2]])$labels$title, "Average posterior")
  expect_match(plot_max_posterior(fits[[2]])$labels$title, "Assignment certainty")
  expect_match(plot_index_disagreement(sel)$labels$title, "not a vote")
})

test_that("the Wilson interval carries the uncertainty of a small bootstrap count", {
  expect_equal(profylr:::.wilson_ci(0, 100), c(0, 0.03699349820698568))
  expect_equal(profylr:::.wilson_ci(1, 100), c(0.00176743206414065, 0.05448619617870532))
  expect_equal(profylr:::.wilson_ci(2, 100), c(0.00550196755016235, 0.07001179072854388))
})

test_that("BLRT prints its cost and stays unvalidated", {
  set.seed(4)
  n <- 40
  group <- rep(c(-2, 2), each = n / 2)
  data <- data.frame(y1 = group + rnorm(n, 0, 0.3), y2 = group + rnorm(n, 0, 0.3))
  controls <- list(covariance = "EEI", n_starts = 4L, screen_iter = 0L, n_full = 4L, max_iter = 30L, seed = 4)
  h0 <- do.call(fit_lpa, c(list(data, c("y1", "y2"), n_states = 1L), controls))
  h1 <- do.call(fit_lpa, c(list(data, c("y1", "y2"), n_states = 2L), controls))
  out <- NULL
  expect_message(out <- blrt_check(experimental = TRUE, h0, h1, B = 1L, max_retries = 1L, seed = 4), "estimated cost")
  expect_false(out$validated)
  expect_false(selection_rule()$blrt)
  expect_equal(lpa_spec(2)$max_iter, 200L)
  expect_match(out$cost_note, "validated stays false until Monte Carlo agreement")
  expect_false(grepl("digit match", out$reason, fixed = TRUE))
  expect_false(grepl("digit match", out$cost_note, fixed = TRUE))
  expect_match(out$reason, "Independent bootstrap p-values are not required to match", fixed = TRUE)
  expect_match(out$reason, "Unreplicated kept solutions are a diagnostic", fixed = TRUE)
  if (out$n_ok > 0L) {
    expect_equal(out$mc_se, sqrt(out$p * (1 - out$p) / out$n_ok))
    expect_lte(out$p_ci_low, out$p)
    expect_gte(out$p_ci_high, out$p)
    expect_equal(out$p_ci_method, "wilson_95")
  }
  expect_match(out$reason, "Dropped")
  expect_match(out$reason, "Mplus TECH14 p=b/B")
  expect_match(out$reason, "(b+1)/(B+1)", fixed = TRUE)
  expect_match(out$reason, "p_plus", fixed = TRUE)
  expect_match(out$reason, "secondary")
  expect_false("p_mplus" %in% names(out))
  expect_equal(out$n_ok + out$n_failed, 1L)
  expect_equal(out$n_failed, out$n_failed_not_converged + out$n_failed_error + out$n_failed_inadmissible)
  expect_lte(out$n_not_replicated, out$n_ok)
  if (out$n_not_replicated > 0L || !isTRUE(out$observed_h0_replicated) || !isTRUE(out$observed_h1_replicated)) {
    expect_match(
      out$reason,
      "THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED. THE SOLUTION MAY NOT BE TRUSTWORTHY DUE TO LOCAL MAXIMA. INCREASE THE NUMBER OF RANDOM STARTS.",
      fixed = TRUE
    )
  }
  expect_equal(length(out$lrs), out$n_ok)
  expect_equal(length(out$lr_draws), out$n_ok)
  if (out$n_ok > 0L) {
    expect_equal(out$p, out$b / out$n_ok)
    expect_equal(out$p_plus, (1 + out$b) / (1 + out$n_ok))
    expect_equal(as.integer(out$b), sum(out$lrs >= out$lr_observed - 1e-8))
  }
  ok_p <- is.finite(out$p) && out$p >= 0 && out$p <= 1
  expect_true(ok_p || out$n_ok == 0L)
  sel <- select_model(list(h0, h1), rule = selection_rule(blrt = TRUE))
  expect_false(sel$blrt$ran)
  expect_match(sel$blrt$reason, "only in the tie zone")
})

test_that("selection defaults rank by BIC and leave substantive filters off", {
  rule <- selection_rule()
  expect_null(rule$min_class_prop)
  expect_null(rule$min_class_n)
  expect_null(rule$min_avepp)
  expect_false(rule$entropy_filter)
  expect_equal(rule$min_entropy, 0.80)
  expect_null(rule$primary)
  expect_false(rule$blrt)
  expect_equal(lpa_spec(2)$max_iter, 200L)
  sel <- choose(list(stub_fit()))
  expect_equal(sel$primary, "bic")
  expect_equal(sel$decision, "selected")
  expect_match(sel$reason, "lowest BIC among eligible candidates tested", fixed = TRUE)
  expect_match(sel$reason, "Search grid:", fixed = TRUE)
  expect_match(sel$reason, "Class size")
  expect_match(sel$reason, "AvePP")
  expect_match(sel$reason, "Entropy")
  expect_match(sel$reason, "min_class_prop=NULL", fixed = TRUE)
  expect_match(sel$reason, "min_class_n=NULL", fixed = TRUE)
  expect_match(sel$reason, "min_avepp=NULL", fixed = TRUE)
  expect_match(sel$reason, "entropy_filter=FALSE", fixed = TRUE)
  expect_match(sel$reason, "no analyst threshold")
  expect_null(sel$accepted)
  expect_true(is.na(sel$accepted_model_id))
  expect_equal(sel$automatic_model_id, sel$selected_model_id)
  expect_equal(sel$automatic_decision, "selected")
  expect_match(report_model(sel), "not an analyst record")
  expect_match(report_model(sel), "accepted starts unset", fixed = TRUE)
})

test_that("a higher unreplicated admissible solution blocks the candidate", {
  hidden <- stub_fit(ll = -100, n_best = 2L)
  hidden$result$rejected_loglik <- -90
  hidden$result$rejected_reason <- "admissible"
  blocked <- choose(list(hidden))
  expect_false(blocked$candidates$eligible[[1]])
  expect_match(blocked$candidates$rejection_reason[[1]], "hard:higher_admissible_not_replicated", fixed = TRUE)
  expect_equal(blocked$decision, "no_selection")
  expect_true(is.na(blocked$selected_model_id))
  expect_match(blocked$reason, "No selection")
  expect_match(blocked$reason, "no fallback")

  visible <- stub_fit(ll = -100, n_best = 2L)
  visible$result$rejected_loglik <- -90
  visible$result$rejected_reason <- "not_converged"
  kept <- choose(list(visible))
  expect_true(kept$candidates$eligible[[1]])
  expect_false(grepl("higher_admissible", kept$candidates$rejection_reason[[1]], fixed = TRUE))
  expect_equal(kept$decision, "selected")
})

test_that("no eligible candidate returns no selection and no fallback", {
  none <- choose(list(stub_fit(converged = FALSE), stub_fit(n_best = 1L)))
  expect_equal(none$decision, "no_selection")
  expect_equal(none$automatic_decision, "no_selection")
  expect_true(is.na(none$selected_model_id))
  expect_true(is.na(none$automatic_model_id))
  expect_false(any(none$candidates$selected))
  expect_match(none$reason, "No selection")
  expect_match(none$reason, "no fallback")
  expect_match(none$reason, "Search grid:", fixed = TRUE)
})

test_that("analyst thresholds are applied and recorded", {
  tiny <- stub_fit(ll = ll_for_bic(80, 8, 200), membership = c(2, 198), entropy_wave = 0.4)
  big <- stub_fit(ll = ll_for_bic(100, 8, 200), membership = c(100, 100), entropy_wave = 0.9)
  rule <- selection_rule(
    min_class_prop = 0.05, min_class_n = 30, min_avepp = 0.70,
    entropy_filter = TRUE, min_entropy = 0.80
  )
  sel <- choose(list(tiny, big), rule = rule)
  expect_equal(picked(sel)$loglik, big$result$loglik)
  expect_match(sel$candidates$rejection_reason[[1]], "soft:min_class_prop")
  expect_match(sel$candidates$rejection_reason[[1]], "soft:min_class_n")
  expect_match(sel$candidates$rejection_reason[[1]], "soft:entropy")
  expect_match(sel$reason, "analyst threshold 0.05 applied", fixed = TRUE)
  expect_match(sel$reason, "analyst threshold 30 applied", fixed = TRUE)
  expect_match(sel$reason, "analyst threshold 0.7 applied", fixed = TRUE)
  expect_match(sel$reason, "analyst entropy threshold 0.8 applied", fixed = TRUE)
  expect_match(sel$reason, "min_class_prop=0.05", fixed = TRUE)
  expect_match(sel$reason, "entropy_filter=TRUE", fixed = TRUE)
})

test_that("accept records the analyst without changing the automatic selection", {
  low <- stub_fit(K = 1L, p = 4L, ll = ll_for_bic(110, 4, 200), membership = 200)
  high <- stub_fit(K = 2L, p = 8L, ll = ll_for_bic(100, 8, 200))
  sel <- choose(list(low, high))
  expect_equal(sel$automatic_model_id, 2L)
  expect_null(sel$accepted)
  expect_true(is.na(sel$accepted_model_id))
  taken <- accept(sel, 1L)
  expect_equal(taken$automatic_model_id, 2L)
  expect_equal(taken$selected_model_id, 2L)
  expect_equal(taken$automatic_decision, "selected")
  expect_true(isTRUE(taken$accepted))
  expect_equal(taken$accepted_model_id, 1L)
  expect_error(accept(sel, 9L), "not one of the tested")
})

test_that("ICL-BIC is reported and does not rank the automatic selection", {
  bic_best <- stub_fit(K = 2L, p = 8L, ll = ll_for_bic(100, 8, 200), entropy_wave_raw = 25, entropy_sequence_raw = 25)
  icl_best <- stub_fit(K = 1L, p = 4L, ll = ll_for_bic(120, 4, 200), membership = 200,
                       entropy_wave_raw = 0, entropy_sequence_raw = 0)
  sel <- choose(list(bic_best, icl_best))
  expect_equal(picked(sel)$K, 2L)
  icl <- sel$disagreement[sel$disagreement$index == "icl_bic", ]
  expect_equal(icl$K, 1L)
  expect_false(icl$selects)
  expect_match(sel$reason, "not used for automatic ranking")
  expect_error(selection_rule(primary = "icl_bic"), "icl_bic is a diagnostic and is not used for automatic ranking")
})

test_that("BLRT uses the admissible likelihood and ignores a higher rejected start", {
  h0 <- stub_fit(K = 1L, ll = -100, p = 4L, membership = 200)
  h0$result$rejected_loglik <- -50
  h0$result$rejected_reason <- "not_converged"
  h0$panel <- list(N = 2L)
  h1 <- stub_fit(K = 2L, ll = -80, p = 8L)
  h1$result$rejected_loglik <- -10
  h1$result$rejected_reason <- "not_positive_definite"
  h1$panel <- list(N = 2L)
  out <- suppressMessages(blrt_check(experimental = TRUE, h0, h1, B = 0L, seed = 1))
  expect_equal(out$observed_h0$loglik, -100)
  expect_equal(out$observed_h1$loglik, -80)
  expect_equal(out$lr_observed, 40)
  expect_false(out$validated)
  expect_true(out$report_only)
})

test_that("a BLRT retry keeps one dataset and adds starts", {
  h0 <- stub_fit(K = 1L, ll = -100, p = 4L, n_starts = 4L, membership = 200)
  h1 <- stub_fit(K = 2L, ll = -80, p = 8L, n_starts = 4L)
  h0$panel <- list(N = 2L)
  h1$panel <- list(N = 2L)
  frame <- data.frame(id = 1:2, y = c(0, 1))
  calls <- new.env(parent = emptyenv())
  calls$simulate <- 0L
  calls$starts <- integer()
  calls$frames <- list()
  local_mocked_bindings(
    .simulate_fit = function(fit, donors) {
      calls$simulate <- calls$simulate + 1L
      frame
    },
    .fit_like = function(template, frame, seed, n_starts = NULL, n_full = NULL, max_iter = NULL) {
      calls$starts <- c(calls$starts, as.integer(n_starts))
      calls$frames[[length(calls$frames) + 1L]] <- frame
      out <- template
      ok <- length(calls$starts) > 2L
      out$result$converged <- ok
      out$result$n_best <- if (ok) 2L else 0L
      out$result$loglik <- if (identical(template$result$K, 1L)) -100 else -80
      out$result$rejected_loglik <- NULL
      out
    },
    .package = "profylr"
  )
  out <- suppressMessages(blrt_check(experimental = TRUE, h0, h1, B = 1L, max_retries = 1L, seed = 1))
  expect_equal(calls$simulate, 1L)
  expect_equal(calls$starts, c(4L, 4L, 8L, 8L))
  expect_true(all(vapply(calls$frames, function(item) identical(item, frame), logical(1))))
  expect_equal(out$attempts$n_starts, c(4L, 8L))
  expect_length(unique(out$attempts$dataset_hash), 1L)
  expect_equal(out$n_ok, 1L)
  expect_equal(nrow(out$failures), 0L)
  expect_match(out$reason, "Unresolved failures: none.", fixed = TRUE)
})

test_that("an unresolved BLRT failure is reported by draw index", {
  h0 <- stub_fit(K = 1L, ll = -100, p = 4L, n_starts = 4L, membership = 200)
  h1 <- stub_fit(K = 2L, ll = -80, p = 8L, n_starts = 4L)
  h0$panel <- list(N = 2L)
  h1$panel <- list(N = 2L)
  frame <- data.frame(id = 1:2, y = c(0, 1))
  calls <- new.env(parent = emptyenv())
  calls$simulate <- 0L
  calls$starts <- integer()
  local_mocked_bindings(
    .simulate_fit = function(fit, donors) {
      calls$simulate <- calls$simulate + 1L
      frame
    },
    .fit_like = function(template, frame, seed, n_starts = NULL, n_full = NULL, max_iter = NULL) {
      calls$starts <- c(calls$starts, as.integer(n_starts))
      out <- template
      out$result$converged <- FALSE
      out$result$n_best <- 0L
      out
    },
    .package = "profylr"
  )
  failed <- suppressMessages(blrt_check(experimental = TRUE, h0, h1, B = 1L, max_retries = 1L, seed = 1))
  expect_equal(calls$simulate, 1L)
  expect_equal(calls$starts, c(4L, 4L, 8L, 8L))
  expect_equal(failed$failures$draw, 1L)
  expect_equal(failed$failures$reason, "not_converged")
  expect_match(failed$reason, "draw 1 not_converged", fixed = TRUE)
  expect_equal(failed$n_ok, 0L)
})

test_that("an unvalidated BLRT does not change the selected model", {
  small <- stub_fit(K = 1L, p = 5L, ll = ll_for_bic(101.5, 5, 200), membership = 200)
  large <- stub_fit(K = 2L, p = 10L, ll = ll_for_bic(100, 10, 200))
  small$panel <- list(N = 2L)
  large$panel <- list(N = 2L)
  local_mocked_bindings(
    blrt_check = function(h0, h1, ...) {
      list(
        ran = TRUE, validated = FALSE, switched = TRUE, report_only = FALSE,
        p = 0, p_plus = 0.5, B = 1L, b = 0L, reason = "mocked p=0",
        n_ok = 1L, seconds = 0
      )
    },
    .package = "profylr"
  )
  off <- choose(list(small, large), rule = selection_rule(blrt = FALSE))
  blocked <- choose(list(small, large), rule = selection_rule(blrt = TRUE, blrt_check = TRUE))
  expect_equal(blocked$selected_model_id, off$selected_model_id)
  expect_false(isTRUE(blocked$blrt$ran))
  expect_identical(blocked$blrt$decision, "not_run")
  expect_false(isTRUE(blocked$blrt$validated))
  expect_match(blocked$blrt$reason, "excluded from automatic selection", fixed = TRUE)
  expect_match(blocked$blrt$reason, "blrt validated=false", fixed = TRUE)
  on <- choose(list(small, large), rule = selection_rule(blrt = TRUE, blrt_check = TRUE, experimental_blrt = TRUE))
  expect_equal(on$selected_model_id, off$selected_model_id)
  expect_equal(on$automatic_model_id, 2L)
  expect_false(isTRUE(on$blrt$switched))
  expect_false(isTRUE(on$blrt$validated))
  expect_match(on$reason, "does not change the selected model")
  expect_match(on$blrt$reason, "does not change the selected model")
})

test_that("blrt_max_iter changes only the bootstrap fits", {
  expect_equal(lpa_spec(2)$max_iter, 200L)
  rule <- selection_rule()
  expect_equal(rule$blrt_max_iter, 1000L)
  expect_false(rule$blrt)
  sel <- choose(list(stub_fit()))
  expect_match(sel$reason, "blrt_max_iter=1000", fixed = TRUE)

  set.seed(4)
  n <- 40
  group <- rep(c(-2, 2), each = n / 2)
  data <- data.frame(y1 = group + rnorm(n, 0, 0.3), y2 = group + rnorm(n, 0, 0.3))
  controls <- list(covariance = "EEI", n_starts = 4L, screen_iter = 0L, n_full = 4L, max_iter = 200L, seed = 4)
  h0 <- do.call(fit_lpa, c(list(data, c("y1", "y2"), n_states = 1L), controls))
  h1 <- do.call(fit_lpa, c(list(data, c("y1", "y2"), n_states = 2L), controls))
  observed_iter <- c(h0$result$n_iter, h1$result$n_iter)
  box <- new.env(parent = emptyenv())
  box$caps <- integer()
  local_mocked_bindings(
    .simulate_fit = function(fit, donors) data.frame(id = seq_len(nrow(data)), y1 = data$y1, y2 = data$y2),
    .fit_like = function(template, frame, seed, n_starts = NULL, n_full = NULL, max_iter = NULL) {
      box$caps <- c(box$caps, as.integer(max_iter))
      template
    },
    .package = "profylr"
  )
  out <- suppressMessages(blrt_check(experimental = TRUE, h0, h1, B = 1L, max_retries = 0L, max_iter = 750L, seed = 4))
  expect_equal(box$caps, c(750L, 750L))
  expect_equal(out$max_iter, 750L)
  expect_equal(h0$spec$max_iter, 200L)
  expect_equal(h1$spec$max_iter, 200L)
  expect_equal(c(h0$result$n_iter, h1$result$n_iter), observed_iter)
  expect_equal(lpa_spec(2)$max_iter, 200L)
  expect_match(out$cost_note, "Bootstrap fits use max_iter=750", fixed = TRUE)
  expect_match(out$cost_note, "Observed H0 and H1 fits keep their own max_iter", fixed = TRUE)
  box$caps <- integer()
  plain <- suppressMessages(blrt_check(experimental = TRUE, h0, h1, B = 1L, max_retries = 0L, seed = 4))
  expect_equal(box$caps, c(1000L, 1000L))
  expect_equal(plain$max_iter, 1000L)
  expect_equal(h0$spec$max_iter, 200L)
  expect_equal(h1$spec$max_iter, 200L)
  expect_equal(c(h0$result$n_iter, h1$result$n_iter), observed_iter)
})

test_that("the default bootstrap size is 1000 and the null rejection probability is (floor(alpha * B) + 1) / (B + 1)", {
  expect_equal(formals(blrt_check)$B, 1000)
  expect_equal(selection_rule()$blrt_B, 1000L)
  expect_false(selection_rule()$blrt)
  expect_equal(selection_rule()$blrt_max_iter, 1000L)
  expect_equal(lpa_spec(2)$n_starts, 40L)
  expect_equal(lpa_spec(2)$screen_iter, 10L)
  expect_equal(lpa_spec(2)$n_full, 4L)
  expect_equal(lpa_spec(2)$max_iter, 200L)
  expect_equal(blrt_null_size(0.01, 100), 2 / 101)
  expect_equal(blrt_null_size(0.01, 1000), 11 / 1001)
  expect_equal(blrt_null_size(0.05, 100), 6 / 101)
  expect_equal(blrt_null_size(0.05, 1000), 51 / 1001)
  for (alpha in c(0.01, 0.05)) {
    for (B in c(100, 1000)) {
      crit <- floor(alpha * B)
      expect_equal(blrt_null_size(alpha, B), (crit + 1) / (B + 1))
      b <- 0:B
      rejects <- (b / B) <= alpha
      expect_equal(sum(rejects), crit + 1)
      expect_equal(rejects, b <= crit)
    }
  }
})

load_published <- function(name, n_use, cols) {
  path <- file.path("/tmp/mplus-ug", name)
  lines <- readLines(path)
  lines <- lines[nzchar(trimws(lines))]
  mat <- do.call(rbind, lapply(strsplit(trimws(lines), "\\s+"), as.numeric))
  frame <- as.data.frame(mat[, seq_len(n_use), drop = FALSE])
  names(frame) <- cols
  frame$id <- seq_len(nrow(frame))
  frame
}

test_that("fitted Mplus examples keep the new information criteria on the observed likelihood", {
  ug <- c("/tmp/mplus-ug/ex7.3.dat", "/tmp/mplus-ug/ex7.9.dat", "/tmp/mplus-ug/ex7.22.dat")
  if (!all(file.exists(ug))) {
    skip(paste(
      "Optional external reproduction. Mplus User's Guide files are not redistributed",
      "with this package. Missing:", paste(ug[!file.exists(ug)], collapse = ", ")
    ))
  }
  check_fit <- function(fit, printed_ll, printed_aic, printed_bic, printed_sabic, printed_entropy) {
    ic <- information_criteria(fit)
    ll <- fit$result$loglik
    p <- fit$result$n_parameters
    n <- fit$result$n_analyzed
    dev <- -2 * ll
    expect_equal(ic$aic, dev + 2 * p, tolerance = 1e-8)
    expect_equal(ic$aic3, dev + 3 * p, tolerance = 1e-8)
    expect_equal(ic$caic, dev + p * (log(n) + 1), tolerance = 1e-8)
    expect_equal(ic$awe, dev + 2 * p * (log(n) + 1.5), tolerance = 1e-8)
    expect_equal(ic$icl_bic, ic$bic + 2 * ic$entropy_raw, tolerance = 1e-6)
    expect_equal(ic$aic, printed_aic, tolerance = 0.01)
    expect_equal(ic$bic, printed_bic, tolerance = 0.01)
    expect_equal(ic$sabic, printed_sabic, tolerance = 0.01)
    expect_equal(fit$result$entropy_wave, printed_entropy, tolerance = 0.03)
    expect_equal(ic$entropy_raw, fit$result$entropy_wave_raw, tolerance = 1e-8)
    expect_equal(ll, printed_ll, tolerance = 0.05)
    clone <- fit
    stored <- clone$result$entropy_wave_raw
    clone$result$entropy_wave_raw <- NULL
    clone$result$entropy_sequence_raw <- NULL
    expect_equal(information_criteria(clone)$entropy_raw, stored, tolerance = 1e-6)
  }
  ex73 <- load_published("ex7.3.dat", 4, c("u1", "u2", "u3", "u4"))
  fit73 <- do.call(fit_lca, c(list(ex73, c("u1", "u2", "u3", "u4"), id = "id", n_states = 2L), published))
  check_fit(fit73, -965.244, 1948.488, 1986.420, 1957.853, 0.904)
  ex79 <- load_published("ex7.9.dat", 4, c("y1", "y2", "y3", "y4"))
  fit79 <- do.call(fit_lpa, c(list(ex79, c("y1", "y2", "y3", "y4"), id = "id", n_states = 2L, covariance = "EEI"), published))
  check_fit(fit79, -3177.162, 6380.324, 6435.114, 6393.851, 0.909)
  ex722 <- load_published("ex7.22.dat", 4, c("y1", "y2", "y3", "y4"))
  fit722 <- do.call(fit_lpa, c(list(ex722, c("y1", "y2", "y3", "y4"), id = "id", n_states = 3L, covariance = "EEE"), published))
  check_fit(fit722, -3054.963, 6157.926, 6259.077, 6182.899, 0.812)
})

read_tech14 <- function(name) {
  path <- profylr_fixture("mplus", name)
  row <- utils::read.csv(path, stringsAsFactors = FALSE)
  list(
    h0 = as.numeric(row$h0[[1]]),
    h1 = as.numeric(row$h1[[1]]),
    lr = as.numeric(row$lr[[1]]),
    p = as.numeric(row$p[[1]]),
    successful = as.integer(row$successful[[1]])
  )
}

test_that("TECH14 fixtures match the likelihood ratio and the p-value within Monte Carlo error", {
  expect_equal(lpa_spec(2)$max_iter, 200L)
  expect_false(selection_rule()$blrt)
  # Guard that p is b/B. Independent runs are not required to match. 0.06 is wider
  # than one Monte Carlo SE at B=100 and rejects a different statistic.
  tolerance <- 0.06
  controls <- list(
    n_starts = 20L, screen_iter = 10L, n_full = 4L, max_iter = 200L,
    tol = 1e-7, replicate_tol = 1e-3, seed = 20261026
  )
  cases <- list(
    list(name = "lca_bernoulli_k2", indicators = c("u1", "u2", "u3", "u4"), covariance = NULL),
    list(name = "lpa_eei_k2", indicators = c("y1", "y2", "y3"), covariance = "EEI"),
    list(name = "lpa_eee_k2", indicators = c("y1", "y2"), covariance = "EEE")
  )
  for (case in cases) {
    printed <- read_tech14(paste0(case$name, "_tech14.csv"))
    dat <- profylr_fixture("mplus", paste0(case$name, ".dat"))
    frame <- utils::read.table(dat, col.names = case$indicators)
    fit_one <- if (is.null(case$covariance)) {
      function(k) do.call(fit_lca, c(list(frame, case$indicators, n_states = k), controls))
    } else {
      function(k) do.call(fit_lpa, c(list(frame, case$indicators, n_states = k, covariance = case$covariance), controls))
    }
    h0 <- fit_one(1L)
    h1 <- fit_one(2L)
    lr <- 2 * (h1$result$loglik - h0$result$loglik)
    expect_lt(abs(h0$result$loglik - printed$h0), 5e-4)
    expect_lt(abs(h1$result$loglik - printed$h1), 5e-4)
    expect_lt(abs(lr - printed$lr), 5e-4)
    expect_equal(printed$successful, 100L)
    out <- blrt_check(experimental = TRUE, h0, h1, B = 100L, max_retries = 3L, seed = 20261026)
    expect_equal(h0$spec$max_iter, 200L)
    expect_equal(h1$spec$max_iter, 200L)
    expect_equal(h0$spec$tol, 1e-7)
    expect_equal(out$max_iter, 1000L)
    expect_equal(out$max_iter, selection_rule()$blrt_max_iter)
    expect_equal(out$lr_observed, lr)
    expect_equal(out$n_ok + out$n_failed, 100L)
    expect_equal(out$n_failed, out$n_failed_not_converged + out$n_failed_error + out$n_failed_inadmissible)
    expect_lte(out$n_not_replicated, out$n_ok)
    expect_false("p_mplus" %in% names(out))
    expect_match(out$reason, paste0("Dropped ", out$n_failed, " draws"), fixed = TRUE)
    expect_match(out$reason, paste0(out$n_not_replicated, " kept draws did not replicate"), fixed = TRUE)
    expect_match(out$reason, "Mplus TECH14 p=b/B")
    expect_match(out$reason, "p_plus=(b+1)/(B+1)", fixed = TRUE)
    expect_equal(out$p, out$b / out$n_ok)
    expect_equal(out$p_plus, (out$b + 1) / (out$n_ok + 1))
    expect_lte(abs(out$p - printed$p), tolerance)
    if (out$n_not_replicated > 0L || !isTRUE(out$observed_h0_replicated) || !isTRUE(out$observed_h1_replicated)) {
      expect_match(
        out$reason,
        "THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED. THE SOLUTION MAY NOT BE TRUSTWORTHY DUE TO LOCAL MAXIMA. INCREASE THE NUMBER OF RANDOM STARTS.",
        fixed = TRUE
      )
    } else {
      expect_false(grepl("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED", out$reason, fixed = TRUE))
    }
    expect_false(out$validated)
    expect_false(selection_rule()$blrt)
  }
})

same_panel <- function(value = 0) {
  list(
    N = 2L, T = 1L, J = 1L, Rmax = 1L, indicators = "y", family = "gaussian",
    ids = 1:2, waves = 1L, gauss = c(value, value + 0.1), cat = NULL,
    ncat = 0L, level_order = list(NULL)
  )
}

test_that("split_null_state copies state zero", {
  src <- list(
    delta = c(0.25, 0.75),
    tau = c(0.8, 0.2, 0.3, 0.7),
    means = c(1, -1),
    cov = c(0.4, 0.5),
    rho = c(0.2, 0.8, 0.6, 0.4)
  )
  out <- profylr:::cpp_split_null_state(src, 2L, 3L, "VVI", 1L, 1L, 1L, 1L, 1L, 1L, 2L)
  expect_equal(out$delta[[1]], 0.125)
  expect_equal(out$delta[[2]], 0.75)
  expect_equal(out$delta[[3]], 0.125)
  expect_equal(out$tau[[3]], 0.4)
  expect_equal(out$tau[[7]], 0.4)
  expect_equal(out$means[[3]], 1)
  expect_equal(out$cov, c(0.4, 0.5, 0.4))
  expect_equal(out$rho[[5]], 0.2)
  expect_equal(out$rho[[6]], 0.8)
  expect_null(profylr:::cpp_split_null_state(src, 2L, 4L, "VVI", 1L, 1L, 1L, 1L, 1L, 1L, 2L))
})

test_that("an unusable observed fit is not run and does not report p = 0", {
  h0 <- stub_fit(K = 1L, ll = -100, p = 4L, loglik_finite = FALSE, converged = FALSE)
  h1 <- stub_fit(K = 2L, ll = -80, p = 8L)
  h0$panel <- same_panel()
  h1$panel <- same_panel()
  local_mocked_bindings(
    .simulate_fit = function(...) stop("bootstrap must not run"),
    .package = "profylr"
  )
  out <- suppressMessages(blrt_check(experimental = TRUE, h0, h1, B = 2L, seed = 1))
  expect_false(out$ran)
  expect_equal(out$reason, "not run: observed fit is not usable blrt validated=false.")
  expect_identical(out$decision, "not_run")
  expect_false(out$validated)
  expect_true(is.na(out$p_lo))
  expect_true(is.na(out$p_hi))
  expect_true(is.na(out$p))
  expect_true(is.na(out$p_plus))
  expect_true(is.na(out$lr_observed))
  expect_equal(out$b, 0L)
  expect_false(out$p_provisional)
})

test_that("BLRT rejects a mismatched comparison before simulation", {
  h0 <- stub_fit(K = 1L, ll = -100, p = 4L)
  h1 <- stub_fit(K = 2L, ll = -80, p = 8L)
  h0$panel <- same_panel(0)
  h1$panel <- same_panel(1)
  local_mocked_bindings(
    .simulate_fit = function(...) stop("bootstrap must not run"),
    .package = "profylr"
  )
  expect_error(blrt_check(experimental = TRUE, h0, h1, B = 1L, seed = 1), "same observed data")
  h1$panel <- same_panel(0)
  h1$result$covariance <- "VVV"
  expect_error(blrt_check(experimental = TRUE, h0, h1, B = 1L, seed = 1), "covariance")
  h1$result$covariance <- "EEI"
  h1$spec$measurement_invariant <- FALSE
  expect_error(blrt_check(experimental = TRUE, h0, h1, B = 1L, seed = 1), "measurement_invariant")
  h1$spec$measurement_invariant <- TRUE
  h1$result$K <- 1L
  expect_error(blrt_check(experimental = TRUE, h0, h1, B = 1L, seed = 1), "nested")
})

blrt_pair <- function() {
  h0 <- stub_fit(K = 1L, ll = -10, p = 4L, n_starts = 4L)
  h1 <- stub_fit(K = 2L, ll = -9, p = 8L, n_starts = 4L)
  panel <- same_panel()
  h0$panel <- panel
  h1$panel <- panel
  h0$result$params <- list(delta = 1, tau = 1, means = 0, cov = 1, rho = numeric())
  list(h0 = h0, h1 = h1)
}

copy_fit <- function(template, loglik) {
  out <- template
  out$result <- template$result
  out$result$converged <- TRUE
  out$result$n_best <- 2L
  out$result$loglik <- loglik
  out
}

test_that("a materially negative likelihood ratio is not clamped to zero", {
  pair <- blrt_pair()
  local_mocked_bindings(
    .simulate_fit = function(fit, donors) data.frame(id = 1:2, y = c(0, 1)),
    .fit_like = function(template, frame, seed, n_starts = NULL, n_full = NULL, max_iter = NULL, start = NULL) {
      loglik <- if (identical(as.integer(template$result$K), 1L)) -10 else -100
      copy_fit(template, loglik)
    },
    .package = "profylr"
  )
  failed <- suppressMessages(blrt_check(experimental = TRUE, pair$h0, pair$h1, B = 1L, max_retries = 0L, seed = 1))
  expect_equal(failed$n_ok, 0L)
  expect_length(failed$lrs, 0L)
  expect_equal(failed$failures$reason, "h1_below_h0")
  expect_true(is.na(failed$p))
  expect_true(failed$p_provisional)
  expect_match(failed$reason, "provisional")
  expect_match(failed$reason, "n_failed > 0")
})

test_that("a likelihood ratio inside the EM allowance is kept and is not replaced by zero", {
  pair <- blrt_pair()
  local_mocked_bindings(
    .simulate_fit = function(fit, donors) data.frame(id = 1:2, y = c(0, 1)),
    .fit_like = function(template, frame, seed, n_starts = NULL, n_full = NULL, max_iter = NULL, start = NULL) {
      loglik <- if (identical(as.integer(template$result$K), 1L)) -10 else -10 - 1e-8
      copy_fit(template, loglik)
    },
    .package = "profylr"
  )
  kept <- suppressMessages(blrt_check(experimental = TRUE, pair$h0, pair$h1, B = 1L, max_retries = 0L, seed = 1))
  expect_equal(kept$n_ok, 1L)
  expect_lt(kept$lrs[[1]], 0)
  expect_equal(kept$lrs[[1]], -2e-8, tolerance = 1e-6)
  expect_false(kept$p_provisional)
})

test_that("a negative likelihood ratio retries from a null-embedded alternative", {
  pair <- blrt_pair()
  calls <- new.env(parent = emptyenv())
  calls$starts <- list()
  local_mocked_bindings(
    .simulate_fit = function(fit, donors) data.frame(id = 1:2, y = c(0, 1)),
    .fit_like = function(template, frame, seed, n_starts = NULL, n_full = NULL, max_iter = NULL, start = NULL,
                         append_start = FALSE) {
      calls$starts <- append(calls$starts, list(start))
      if (identical(as.integer(template$result$K), 1L)) loglik <- -10
      else if (is.null(start)) loglik <- -100
      else loglik <- -9
      copy_fit(template, loglik)
    },
    .package = "profylr"
  )
  recovered <- suppressMessages(blrt_check(experimental = TRUE, pair$h0, pair$h1, B = 1L, max_retries = 1L, seed = 1))
  expect_equal(recovered$n_ok, 1L)
  expect_equal(recovered$lrs[[1]], 2)
  expect_length(calls$starts, 4L)
  expect_null(calls$starts[[2]])
  expect_equal(calls$starts[[4]]$delta[[2]], 0.5)
})

test_that("select_model does not rank incomparable fits", {
  left <- stub_fit(K = 1L, ll = -100, p = 4L)
  right <- stub_fit(K = 2L, ll = -80, p = 8L)
  left$panel <- same_panel(0)
  right$panel <- same_panel(3)
  expect_error(
    select_model(list(left, right), second_seed_loglik = c(-100, -80), second_seed_n_best = c(2L, 2L)),
    "not comparable"
  )
  table <- compare_models(list(left, right))
  expect_equal(nrow(table), 2L)
  expect_true(all(table$incomparable))
  expect_false(any(table$comparable))
  quiet <- select_model(list(left, right), auto = FALSE)
  expect_equal(nrow(quiet$candidates), 2L)
})

test_that("second_seed_loglik without n_best stays not assessed", {
  fit <- stub_fit()
  sel <- select_model(list(fit), second_seed_loglik = -100)
  row <- sel$candidates[1, ]
  expect_equal(row$second_seed_status, "not assessed")
  expect_match(row$rejection_reason, "hard:second_seed_not_assessed", fixed = TRUE)
  expect_true(is.na(row$second_seed_n_best))
  expect_false(row$eligible)
})

test_that("experimental BLRT bounds require opt-in and bound failed draws", {
  expect_false(selection_rule()$experimental_blrt)
  expect_false(selection_rule()$blrt)
  off <- blrt_failure_bounds(1L, 2L, 10L, alpha = 0.05, ran = FALSE)
  expect_identical(off$decision, "not_run")
  expect_false(off$validated)
  expect_true(is.na(off$p_lo))
  expect_true(is.na(off$p_hi))
  reject <- blrt_failure_bounds(0L, 0L, 100L, alpha = 0.05)
  expect_equal(reject$p_lo, 1 / 101)
  expect_equal(reject$p_hi, 1 / 101)
  expect_identical(reject$decision, "reject")
  mid <- blrt_failure_bounds(0L, 10L, 100L, alpha = 0.05)
  expect_equal(mid$p_lo, 1 / 101)
  expect_equal(mid$p_hi, 11 / 101)
  expect_identical(mid$decision, "inconclusive")
  retain <- blrt_failure_bounds(20L, 0L, 20L, alpha = 0.05)
  expect_equal(retain$p_lo, 1)
  expect_identical(retain$decision, "retain")
  skipped <- blrt_failure_bounds(0L, 0L, 10L, n_not_run = 2L)
  expect_equal(skipped$n_unresolved, 2L)
  expect_equal(skipped$p_lo, 1 / 11)
  expect_equal(skipped$p_hi, 3 / 11)
  expect_identical(skipped$bounds_decision, "retain")
  expect_identical(skipped$decision, "inconclusive")
  forced <- blrt_failure_bounds(0L, 0L, 100L, n_not_run = 1L)
  expect_equal(forced$p_hi, 2 / 101)
  expect_identical(forced$bounds_decision, "reject")
  expect_identical(forced$decision, "inconclusive")
  expect_true(profylr:::.blrt_exceeds(1, 1))
  expect_true(profylr:::.blrt_exceeds(1 - 1e-8, 1))
  expect_true(profylr:::.blrt_exceeds(2, 1))
  expect_false(profylr:::.blrt_exceeds(1 - 1e-7, 1))
  h0 <- stub_fit(K = 1L, ll = -100, p = 4L, membership = 200)
  h1 <- stub_fit(K = 2L, ll = -80, p = 8L)
  h0$panel <- list(N = 2L)
  h1$panel <- list(N = 2L)
  not_run <- suppressMessages(blrt_check(h0, h1, B = 1L, seed = 1))
  expect_false(not_run$ran)
  expect_identical(not_run$decision, "not_run")
  expect_false(not_run$validated)
  expect_true(is.na(not_run$p))
  expect_true(is.na(not_run$p_lo))
  expect_true(is.na(not_run$p_hi))
  expect_match(not_run$reason, "not run", fixed = TRUE)
  expect_match(not_run$reason, "blrt validated=false", fixed = TRUE)
  expect_match(not_run$reason, "explicit opt-in", fixed = TRUE)
})

test_that("mixed failed and unattempted draws count once in the upper bound", {
  # B=100 scheduled draws: 3 exceedances, 2 failed attempts, 1 not attempted,
  # and 94 successful non-exceedances. A study scores decision, not bounds_decision.
  out <- blrt_failure_bounds(3L, 2L, 100L, alpha = 0.10, n_not_run = 1L)
  expect_equal(out$b + out$f + out$n_not_run + 94L, out$B)
  expect_equal(out$n_unresolved, out$f + out$n_not_run)
  expect_equal(out$p_lo, 4 / 101)
  expect_equal(out$p_hi, 7 / 101)
  expect_identical(out$bounds_decision, "reject")
  expect_identical(out$decision, "inconclusive")
})
