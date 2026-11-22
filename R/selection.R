# Automatic state-count and covariance selection.
#
# compare_models stays a report. select_model applies selection_rule when
# auto is TRUE. The disagreement table is not a vote.
#
# Lower is better. n is the number of analyzed subjects. EN is the raw
# classification entropy, and 0 log 0 is 0.
#   AIC    = -2 ll + 2 p
#   AIC3   = -2 ll + 3 p
#   BIC    = -2 ll + p log n
#   SABIC  = -2 ll + p log((n + 2) / 24)
#   CAIC   = -2 ll + p (log n + 1)
#   AWE    = -2 ll + 2 p (log n + 1.5) on the observed log-likelihood
#   ICL-BIC = BIC + 2 EN
# One wave uses the smoothed-state entropy. A latent transition model uses
# the joint sequence entropy. Normalized entropy is not a default filter.

.index_names <- c("aic", "aic3", "bic", "sabic", "caic", "awe", "icl_bic")

.bias_note <- c(
  aic = "AIC tends to over-select the number of states (Nylund, Asparouhov, and Muthen 2007).",
  aic3 = "AIC3 penalizes more than AIC and can still over-select relative to BIC (Bozdogan 1987; Nylund-Gibson and Choi 2018).",
  bic = "BIC can under-select in small samples and for latent transition models (Nylund, Asparouhov, and Muthen 2007; Edelsbrunner, Flaig, and Schneider 2022).",
  sabic = "SABIC can over-select relative to BIC in large cross-sectional samples (Nylund-Gibson and Choi 2018; Edelsbrunner, Flaig, and Schneider 2022).",
  caic = "CAIC is more conservative than BIC and can under-select (Bozdogan 1987; Nylund-Gibson and Choi 2018).",
  awe = "AWE is conservative and can under-select the number of states (Banfield and Raftery 1993; Nylund-Gibson and Choi 2018).",
  icl_bic = "ICL-BIC adds a classification-entropy penalty and can under-select when classes overlap (Biernacki, Celeux, and Govaert 2000). It is a diagnostic and is not used for automatic ranking."
)

.hard_tokens <- c(
  "hard:not_converged", "hard:non_finite", "hard:regularized",
  "hard:not_positive_definite", "hard:collapsed",
  "hard:replication_not_assessed", "hard:best_loglik_not_replicated",
  "hard:higher_admissible_not_replicated",
  "hard:information_criterion_undefined",
  "hard:posteriors_not_computed",
  "hard:second_seed_not_reproduced", "hard:second_seed_not_replicated",
  "hard:second_seed_not_assessed"
)
.soft_tokens <- c("soft:min_class_prop", "soft:min_class_n", "soft:min_avepp", "soft:entropy")
# The three second-seed tokens are applied inside the second-seed pass.
.preseed_tokens <- .hard_tokens[seq_len(length(.hard_tokens) - 3L)]

.candidate_columns <- c(
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

.finite1 <- function(x) length(x) == 1L && is.finite(x)

.cov_key <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value) || !nzchar(as.character(value))) "" else as.character(value)
}

.cov_label <- function(value) {
  key <- .cov_key(value)
  if (nzchar(key)) key else "categorical"
}

# blrt_max_iter is the bootstrap-only cap. 1000 is the smallest value in the
# TECH14 grid (500, 1000, 2000, 5000) at which LCA, EEI, and EEE kept every
# draw in both languages. Other fits keep max_iter, default 200. tol stays 1e-7.
#' @export
selection_rule <- function(min_class_prop = NULL, min_class_n = NULL, min_avepp = NULL,
                           entropy_filter = FALSE, min_entropy = 0.80, primary = NULL,
                           tie_zone = 2, second_seed_offset = 10007, require_second_seed = TRUE,
                           blrt = FALSE, blrt_B = 1000, blrt_max_retries = 3, blrt_alpha = 0.05,
                           blrt_check = FALSE, blrt_max_iter = 1000L, experimental_blrt = FALSE) {
  if (identical(primary, "icl_bic")) {
    stop("icl_bic is a diagnostic and is not used for automatic ranking", call. = FALSE)
  }
  if (!is.null(primary) && !primary %in% .index_names) {
    stop("primary must be one of ", paste(.index_names, collapse = ", "), call. = FALSE)
  }
  if (is.null(tie_zone) || !is.finite(tie_zone) || tie_zone < 0) {
    stop("tie_zone must be zero or positive", call. = FALSE)
  }
  if (blrt_B < 1) stop("blrt_B must be at least 1", call. = FALSE)
  if (is.null(blrt_max_iter) || !is.finite(blrt_max_iter) || blrt_max_iter < 1) {
    stop("blrt_max_iter must be at least 1", call. = FALSE)
  }
  list(
    min_class_prop = if (is.null(min_class_prop)) NULL else as.numeric(min_class_prop),
    min_class_n = if (is.null(min_class_n)) NULL else as.numeric(min_class_n),
    min_avepp = if (is.null(min_avepp)) NULL else as.numeric(min_avepp),
    entropy_filter = isTRUE(entropy_filter),
    min_entropy = if (is.null(min_entropy)) NULL else as.numeric(min_entropy),
    primary = primary,
    tie_zone = as.numeric(tie_zone),
    second_seed_offset = as.integer(second_seed_offset),
    require_second_seed = isTRUE(require_second_seed),
    blrt = isTRUE(blrt),
    blrt_B = as.integer(blrt_B),
    blrt_max_retries = as.integer(blrt_max_retries),
    blrt_alpha = as.numeric(blrt_alpha),
    blrt_check = isTRUE(blrt_check),
    blrt_max_iter = as.integer(blrt_max_iter),
    experimental_blrt = isTRUE(experimental_blrt)
  )
}

# Null rejection probability when every requested draw is kept.
# The reported BLRT p is b/n_ok (NA when no draw is kept). This helper is
# that rule when n_ok equals B: (floor(alpha * B) + 1) / (B + 1).
# The rank of the observed likelihood ratio is uniform on 1, ..., B+1.
#' @export
blrt_null_size <- function(alpha, B) {
  if (!is.finite(alpha) || alpha < 0 || alpha > 1) stop("alpha must be in [0, 1]", call. = FALSE)
  if (!is.finite(B) || B < 1) stop("B must be at least 1", call. = FALSE)
  (floor(alpha * B) + 1) / (B + 1)
}

.raw_entropy <- function(result) {
  if (isFALSE(result$posteriors_computed)) return(c(wave = NA_real_, sequence = NA_real_))
  stored_w <- result$entropy_wave_raw
  stored_s <- result$entropy_sequence_raw
  if (.finite1(stored_w) && .finite1(stored_s)) return(c(wave = stored_w, sequence = stored_s))
  gamma <- result$gamma
  K <- as.integer(result$K %||% 0L)
  waves <- as.integer(result$n_waves %||% 1L)
  subjects <- as.integer(result$n_subjects %||% 0L)
  if (is.null(gamma) || K < 1L || length(gamma) != subjects * waves * K) {
    return(c(
      wave = if (.finite1(stored_w)) stored_w else NA_real_,
      sequence = if (.finite1(stored_s)) stored_s else NA_real_
    ))
  }
  xi <- result$xi
  hw <- 0
  hs <- 0
  for (i in seq_len(subjects)) {
    for (t in seq_len(waves)) {
      for (state in seq_len(K)) {
        g <- gamma[((i - 1L) * waves + (t - 1L)) * K + state]
        if (is.finite(g) && g > 0) hw <- hw - g * log(g)
      }
    }
    for (state in seq_len(K)) {
      g <- gamma[((i - 1L) * waves) * K + state]
      if (is.finite(g) && g > 0) hs <- hs - g * log(g)
    }
    if (waves < 2L || is.null(xi) || length(xi) < subjects * (waves - 1L) * K * K) next
    for (t in seq_len(waves - 1L)) {
      for (j in seq_len(K)) {
        g <- gamma[((i - 1L) * waves + (t - 1L)) * K + j]
        if (!(is.finite(g) && g > 0)) next
        for (state in seq_len(K)) {
          x <- xi[(((i - 1L) * (waves - 1L) + (t - 1L)) * K + (j - 1L)) * K + state]
          if (is.finite(x) && x > 0) hs <- hs - x * log(x / g)
        }
      }
    }
  }
  c(
    wave = if (.finite1(stored_w)) stored_w else hw,
    sequence = if (.finite1(stored_s)) stored_s else hs
  )
}

#' @export
information_criteria <- function(fit) {
  result <- if (inherits(fit, "profylr_fit")) fit$result else fit
  n <- as.numeric(result$n_analyzed %||% 0)
  p <- as.numeric(result$n_parameters %||% 0)
  ll <- result$loglik
  waves <- as.integer(result$n_waves %||% 1L)
  supported <- isTRUE(result$ic_supported)
  raw <- if (isFALSE(result$posteriors_computed)) c(wave = NA_real_, sequence = NA_real_) else .raw_entropy(result)
  en <- if (waves >= 2L) raw[["sequence"]] else raw[["wave"]]
  out <- stats::setNames(rep(NA_real_, length(.index_names)), .index_names)
  out <- c(out, list(n = n, n_parameters = p, loglik = if (.finite1(ll)) ll else NA_real_,
                     entropy_raw = if (.finite1(en)) en else NA_real_, ic_supported = supported))
  # c() on a named numeric with a list coerces poorly. Rebuild explicitly.
  blank <- list(n = n, n_parameters = p, loglik = if (.finite1(ll)) as.numeric(ll) else NA_real_,
                entropy_raw = if (.finite1(en)) as.numeric(en) else NA_real_, ic_supported = supported)
  for (name in .index_names) blank[[name]] <- NA_real_
  if (!supported || !.finite1(ll) || n < 1) return(blank)
  dev <- -2 * as.numeric(ll)
  logn <- log(n)
  blank$aic <- dev + 2 * p
  blank$aic3 <- dev + 3 * p
  blank$bic <- dev + p * logn
  blank$sabic <- dev + p * log((n + 2) / 24)
  blank$caic <- dev + p * (logn + 1)
  blank$awe <- dev + 2 * p * (logn + 1.5)
  if (.finite1(en)) blank$icl_bic <- blank$bic + 2 * as.numeric(en)
  blank
}

.occ <- function(avepp, pi) {
  if (!.finite1(avepp) || !.finite1(pi)) return(NA_real_)
  if (avepp <= 0 || pi <= 0 || pi >= 1) return(NA_real_)
  if (avepp >= 1) return(Inf)
  (avepp / (1 - avepp)) / (pi / (1 - pi))
}

.avepp_block <- function(post, prop) {
  K <- ncol(post)
  valid <- if (!nrow(post)) logical() else apply(post, 1, function(row) all(is.finite(row)))
  modal <- rep(NA_integer_, nrow(post))
  if (any(valid)) {
    modal[valid] <- apply(post[valid, , drop = FALSE], 1, which.max)
  }
  matrix <- matrix(NA_real_, K, K)
  avepp <- rep(NA_real_, K)
  n_modal <- integer(K)
  for (state in seq_len(K)) {
    mask <- !is.na(modal) & modal == state
    n_modal[[state]] <- sum(mask)
    if (!n_modal[[state]]) next
    matrix[state, ] <- colMeans(post[mask, , drop = FALSE])
    avepp[[state]] <- matrix[state, state]
  }
  occ <- rep(NA_real_, K)
  if (!is.null(prop)) {
    for (state in seq_len(K)) occ[[state]] <- .occ(avepp[[state]], prop[[state]])
  }
  list(avepp = avepp, matrix = matrix, occ = occ, n_modal = n_modal)
}

#' @export
classification_diagnostics <- function(fit) {
  result <- if (inherits(fit, "profylr_fit")) fit$result else fit
  if (isFALSE(result$posteriors_computed)) {
    K <- as.integer(result$K %||% 0L)
    return(list(
      avepp = rep(NA_real_, K), avepp_matrix = matrix(NA_real_, K, K), min_avepp = NA_real_,
      occ = rep(NA_real_, K), min_class_prop = NA_real_, min_class_n = NA_real_,
      entropy_wave = NA_real_, entropy_sequence = NA_real_, entropy_raw = NA_real_,
      by_wave = data.frame(wave = numeric(), state = integer(), avepp = numeric(), occ = numeric(),
                           class_n = numeric(), class_prop = numeric(), n_modal = integer())
    ))
  }
  panel <- if (inherits(fit, "profylr_fit")) fit$panel else NULL
  K <- as.integer(result$K %||% 0L)
  waves <- as.integer(result$n_waves %||% 1L)
  subjects <- as.integer(result$n_subjects %||% 0L)
  labels <- if (!is.null(panel$waves) && length(panel$waves) == waves) panel$waves else seq_len(waves)
  membership <- result$expected_membership
  n_analyzed <- as.numeric(result$n_analyzed %||% 0)
  mem <- NULL
  min_n <- NA_real_
  min_p <- NA_real_
  if (!is.null(membership) && length(membership) == waves * K && K > 0L && n_analyzed > 0) {
    mem <- matrix(membership, nrow = waves, byrow = TRUE)
    min_n <- min(mem)
    min_p <- min(mem / n_analyzed)
  }
  raw <- .raw_entropy(result)
  en <- if (waves >= 2L) raw[["sequence"]] else raw[["wave"]]
  empty_wave <- data.frame(wave = labels[0], state = integer(), avepp = numeric(), occ = numeric(),
                           class_n = numeric(), class_prop = numeric(), n_modal = integer())
  gamma <- result$gamma
  if (is.null(gamma) || K < 1L || length(gamma) != subjects * waves * K) {
    return(list(
      avepp = rep(NA_real_, K), avepp_matrix = matrix(NA_real_, K, K), min_avepp = NA_real_,
      occ = rep(NA_real_, K), min_class_prop = min_p, min_class_n = min_n,
      entropy_wave = result$entropy_wave %||% NA_real_, entropy_sequence = result$entropy_sequence %||% NA_real_,
      entropy_raw = en, by_wave = empty_wave
    ))
  }
  arr <- array(as.numeric(gamma), dim = c(K, waves, subjects))
  pooled <- matrix(NA_real_, subjects * waves, K)
  pos <- 0L
  for (i in seq_len(subjects)) for (t in seq_len(waves)) {
    pos <- pos + 1L
    pooled[pos, ] <- arr[, t, i]
  }
  prop <- if (is.null(mem)) NULL else mem / n_analyzed
  pooled_prop <- if (is.null(prop)) NULL else colMeans(prop)
  block <- .avepp_block(pooled, pooled_prop)
  rows <- vector("list", waves * K)
  cursor <- 0L
  for (t in seq_len(waves)) {
    prow <- if (is.null(prop)) NULL else prop[t, ]
    slice <- as.numeric(arr[, t, , drop = FALSE])
    post <- matrix(slice, nrow = subjects, ncol = K, byrow = TRUE)
    wave_block <- .avepp_block(post, prow)
    for (state in seq_len(K)) {
      cursor <- cursor + 1L
      rows[[cursor]] <- data.frame(
        wave = labels[[t]], state = state, avepp = wave_block$avepp[[state]], occ = wave_block$occ[[state]],
        class_n = if (is.null(mem)) NA_real_ else mem[t, state],
        class_prop = if (is.null(prop)) NA_real_ else prop[t, state],
        n_modal = wave_block$n_modal[[state]], stringsAsFactors = FALSE
      )
    }
  }
  finite <- block$avepp[is.finite(block$avepp)]
  list(
    avepp = block$avepp, avepp_matrix = block$matrix,
    min_avepp = if (length(finite)) min(finite) else NA_real_,
    occ = block$occ, min_class_prop = min_p, min_class_n = min_n,
    entropy_wave = result$entropy_wave %||% NA_real_,
    entropy_sequence = result$entropy_sequence %||% NA_real_,
    entropy_raw = en,
    by_wave = do.call(rbind, rows)
  )
}

.soft_fail <- function(value, threshold) {
  if (is.null(threshold)) return(FALSE)
  !.finite1(value) || value < threshold
}

.primary_name <- function(rule, rows) {
  models <- unique(vapply(rows, function(row) row$model, character(1)))
  joint <- any(vapply(rows, function(row) identical(row$model, "lta") && row$n_waves >= 2L, logical(1)))
  if (joint && !identical(models, "lta")) {
    stop("select_model compares one model family at a time", call. = FALSE)
  }
  if (!is.null(rule$primary)) return(rule$primary)
  "bic"
}

.join_tokens <- function(tokens) {
  ordered <- c(.hard_tokens, .soft_tokens)
  paste(ordered[ordered %in% tokens], collapse = ",")
}

.sort_key_order <- function(rows, primary) {
  ord <- order(
    vapply(rows, function(row) row[[primary]], numeric(1)),
    vapply(rows, function(row) row$n_parameters, numeric(1)),
    vapply(rows, function(row) row$K, numeric(1)),
    vapply(rows, function(row) .cov_key(row$covariance), character(1)),
    vapply(rows, function(row) row$model_id, numeric(1))
  )
  rows[ord]
}

.choose_rows <- function(rows, primary, tie_zone) {
  eligible <- Filter(function(row) isTRUE(row$eligible) && .finite1(row[[primary]]), rows)
  if (!length(eligible)) return(list(chosen = NULL, ambiguous = FALSE, tie = list(), kind = ""))
  min_ic <- min(vapply(eligible, function(row) row[[primary]], numeric(1)))
  minimizers <- Filter(function(row) row[[primary]] <= min_ic + 1e-8, eligible)
  # tie_zone is retained on the rule and is not a reason to prefer a higher BIC.
  tie <- minimizers
  pool <- .sort_key_order(minimizers, primary)
  list(chosen = pool[[1]], ambiguous = length(minimizers) > 1L, tie = tie, kind = if (length(minimizers) > 1L) "exact" else "")
}

.disagreement_table <- function(rows) {
  records <- lapply(.index_names, function(name) {
    finite <- Filter(function(row) .finite1(row[[name]]), rows)
    if (!length(finite)) {
      return(data.frame(index = name, model_id = NA_integer_, K = NA_integer_, covariance = NA_character_,
                        value = NA_real_, bias_note = unname(.bias_note[[name]]), selects = FALSE,
                        stringsAsFactors = FALSE))
    }
    best <- finite[[1]]
    for (row in finite[-1]) {
      better <- row[[name]] < best[[name]] - 1e-12 ||
        (abs(row[[name]] - best[[name]]) <= 1e-12 && (row$K < best$K ||
          (row$K == best$K && .cov_key(row$covariance) < .cov_key(best$covariance)) ||
          (row$K == best$K && .cov_key(row$covariance) == .cov_key(best$covariance) && row$model_id < best$model_id)))
      if (isTRUE(better)) best <- row
    }
    data.frame(index = name, model_id = as.integer(best$model_id), K = as.integer(best$K),
               covariance = if (.cov_key(best$covariance) == "") NA_character_ else as.character(best$covariance),
               value = best[[name]], bias_note = unname(.bias_note[[name]]), selects = FALSE,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, records)
}

.relative_indices <- function(rows) {
  for (name in .index_names) {
    values <- vapply(rows, function(row) if (.finite1(row[[name]])) row[[name]] else NA_real_, numeric(1))
    baseline <- if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE)
    for (i in seq_along(rows)) {
      rows[[i]][[paste0("delta_", name)]] <- if (.finite1(rows[[i]][[name]]) && .finite1(baseline)) {
        rows[[i]][[name]] - baseline
      } else NA_real_
    }
  }
  weights <- lapply(rows, function(row) if (.finite1(row$delta_bic)) exp(-0.5 * row$delta_bic) else NA_real_)
  total <- sum(unlist(weights), na.rm = TRUE)
  for (i in seq_along(rows)) {
    if (!.finite1(weights[[i]]) || !total) {
      rows[[i]]$bic_bf_vs_best <- NA_real_
      rows[[i]]$bic_cmp <- NA_real_
    } else {
      rows[[i]]$bic_bf_vs_best <- weights[[i]]
      rows[[i]]$bic_cmp <- weights[[i]] / total
    }
  }
  rows
}

.selection_reason <- function(decision, chosen, primary, rule, ambiguous, warnings, blrt) {
  if (identical(decision, "auto_disabled")) {
    return(paste(
      "Automatic selection is off because auto is FALSE. Candidates, rejection reasons,",
      "and the index disagreement table are reported. No cell is chosen."
    ))
  }
  if (identical(decision, "no_selection") || is.null(chosen)) {
    return(paste(
      "No selection. No candidate passed the numerical validity checks.",
      "There is no fallback. See rejection_reason on each candidate."
    ))
  }
  value <- chosen[[primary]]
  threshold_text <- function(label, value, threshold) {
    if (is.null(threshold)) {
      paste0(label, " ", format(value, digits = 6), " (no analyst threshold). ")
    } else {
      paste0(label, " ", format(value, digits = 6), " (analyst threshold ", threshold, " applied). ")
    }
  }
  text <- paste0(
    "Automatic selection: lowest ", toupper(primary), " among eligible candidates tested. ",
    "Selected the ", chosen$model, " model with K=", chosen$K, " and covariance ", .cov_label(chosen$covariance),
    " (", format(value, digits = 6), "). ",
    "Eligible means the fit converged, the likelihood is finite, the covariance is admissible ",
    "(not regularized, positive definite, and not collapsed), and the highest admissible likelihood was replicated",
    if (isTRUE(rule$require_second_seed)) {
      ", and an independent second seed reproduced that log-likelihood. "
    } else {
      ". The second-seed check is off. "
    },
    threshold_text("Class size: minimum proportion", chosen$min_class_prop, rule$min_class_prop),
    threshold_text("minimum class n", chosen$min_class_n, rule$min_class_n),
    threshold_text("AvePP", chosen$min_avepp, rule$min_avepp),
    "Entropy ", format(chosen$entropy_wave, digits = 6)
  )
  if (isTRUE(rule$entropy_filter)) {
    text <- paste0(text, " (analyst entropy threshold ", rule$min_entropy, " applied). ")
  } else {
    text <- paste0(text, " is reported and is not a filter. ")
  }
  text <- paste0(
    text,
    "Settings: auto=TRUE, primary=", toupper(primary),
    ", min_class_prop=", if (is.null(rule$min_class_prop)) "NULL" else rule$min_class_prop,
    ", min_class_n=", if (is.null(rule$min_class_n)) "NULL" else rule$min_class_n,
    ", min_avepp=", if (is.null(rule$min_avepp)) "NULL" else rule$min_avepp,
    ", entropy_filter=", isTRUE(rule$entropy_filter),
    ", blrt=", isTRUE(rule$blrt),
    ", blrt_max_iter=", as.integer(rule$blrt_max_iter %||% 1000L),
    ", require_second_seed=", isTRUE(rule$require_second_seed),
    ". ICL-BIC is a diagnostic and is not used for automatic ranking. ",
    "K and covariance were chosen jointly. "
  )
  if (isTRUE(ambiguous)) {
    text <- paste0(text, "The choice is ambiguous: more than one eligible cell shares the minimum ",
                   toupper(primary), ". ")
  }
  if ("configural_noninvariance_possible" %in% warnings) {
    text <- paste0(text, " Per-wave enumeration selected different state counts, so configural noninvariance is possible.")
  }
  if (isTRUE(blrt$ran) && !isTRUE(blrt$validated)) {
    text <- paste0(text, " BLRT was run. validated stays false until Monte Carlo agreement, shared-dataset likelihoods, and operating characteristics are shown.",
                   " An unvalidated BLRT is report-only and does not change the selected model.")
  }
  text
}

.panel_frame <- function(fit) {
  panel <- fit$panel
  rows <- vector("list", panel$N * panel$T)
  cursor <- 0L
  for (i in seq_len(panel$N)) for (t in seq_len(panel$T)) {
    cursor <- cursor + 1L
    rec <- data.frame(id = panel$ids[[i]], wave = panel$waves[[t]], stringsAsFactors = FALSE)
    for (j in seq_along(panel$indicators)) {
      offset <- ((i - 1L) * panel$T + (t - 1L)) * panel$J + j
      if (identical(panel$family[[j]], "gaussian")) {
        value <- panel$gauss[[offset]]
        rec[[panel$indicators[[j]]]] <- if (is.finite(value)) value else NA_real_
      } else {
        code <- panel$cat[[offset]]
        rec[[panel$indicators[[j]]]] <- if (code < 0) NA_real_ else code
      }
    }
    rows[[cursor]] <- rec
  }
  do.call(rbind, rows)
}

.refit_second_seed <- function(fit, offset) {
  spec <- fit$spec
  spec$seed <- as.numeric(spec$seed %||% 1) + as.numeric(offset)
  spec$has_start <- FALSE
  spec$start <- NULL
  cpp_fit(fit$panel, spec)
}

.wave_summary <- function(fits, rule) {
  fit <- fits[[1]]
  if (is.null(fit$panel)) {
    return(list(table = NULL, note = "Per-wave enumeration was not run because the fits have no panel."))
  }
  frame <- .panel_frame(fit)
  states <- sort(unique(vapply(fits, function(item) as.integer(item$result$K), integer(1))))
  covs <- unique(Filter(nzchar, vapply(fits, function(item) .cov_key(item$result$covariance), character(1))))
  if (!length(covs)) covs <- "EEI"
  spec <- fit$spec
  family <- stats::setNames(as.list(fit$panel$family), fit$panel$indicators)
  enum <- enumerate_waves(
    frame, fit$panel$indicators, id = "id", wave = "wave",
    n_states = states, covariance = covs, family = family,
    n_starts = as.integer(spec$n_starts %||% 2L),
    screen_iter = as.integer(spec$screen_iter %||% 0L),
    n_full = as.integer(spec$n_full %||% spec$n_starts %||% 2L),
    max_iter = as.integer(spec$max_iter %||% 200L),
    tol = as.numeric(spec$tol %||% 1e-7),
    replicate_tol = as.numeric(spec$replicate_tol %||% 0.1),
    seed = as.numeric(spec$seed %||% 1),
    n_threads = as.integer(spec$n_threads %||% 1L),
    align_labels = if (is.null(spec$align_labels)) TRUE else isTRUE(spec$align_labels)
  )
  wave_rule <- rule
  wave_rule$primary <- "bic"
  rows <- lapply(names(enum$tables), function(wave) {
    ids <- enum$tables[[wave]]$model_id
    piece <- enum$fits[ids]
    chosen <- select_model(piece, rule = wave_rule, auto = TRUE, waves = "skip")
    data.frame(wave = suppressWarnings(as.numeric(wave)), K = chosen$selected_k,
               decision = chosen$decision, model_id = chosen$selected_model_id, stringsAsFactors = FALSE)
  })
  summary <- do.call(rbind, rows)
  ks <- summary$K[is.finite(summary$K)]
  note <- if (length(unique(ks)) > 1L) "configural_noninvariance_possible" else "ok"
  list(table = summary, note = note)
}

.sigma_state <- function(result, state, block = 0L) {
  name <- .cov_key(result$covariance)
  cov <- result$params$cov
  K <- as.integer(result$K)
  Jg <- as.integer(result$n_gaussian %||% 0L)
  if (!Jg || is.null(cov) || !length(cov) || !nzchar(name)) return(matrix(0, 0, 0))
  full <- function(packed) {
    matrix_out <- matrix(0, Jg, Jg)
    index <- 0L
    for (row in seq_len(Jg)) for (col in seq_len(row)) {
      index <- index + 1L
      matrix_out[row, col] <- matrix_out[col, row] <- packed[[index]]
    }
    matrix_out
  }
  if (name == "EEI") return(diag(cov[(block * Jg) + seq_len(Jg)], nrow = Jg))
  if (name == "VVI") {
    start <- (block * K + (state - 1L)) * Jg
    return(diag(cov[start + seq_len(Jg)], nrow = Jg))
  }
  packed <- Jg * (Jg + 1L) / 2L
  if (name == "EEE") return(full(cov[(block * packed) + seq_len(packed)]))
  if (name == "VVV") {
    start <- (block * K + (state - 1L)) * packed
    return(full(cov[start + seq_len(packed)]))
  }
  matrix(0, Jg, Jg)
}

.subject_masks <- function(panel) {
  masks <- array(FALSE, dim = c(panel$N, panel$T, panel$J))
  for (i in seq_len(panel$N)) for (t in seq_len(panel$T)) for (j in seq_len(panel$J)) {
    offset <- ((i - 1L) * panel$T + (t - 1L)) * panel$J + j
    masks[i, t, j] <- if (identical(panel$family[[j]], "gaussian")) {
      !is.finite(panel$gauss[[offset]])
    } else {
      panel$cat[[offset]] < 0
    }
  }
  masks
}

.simulate_fit <- function(fit, donors, masks = NULL) {
  result <- fit$result
  panel <- fit$panel
  K <- as.integer(result$K)
  waves <- as.integer(result$n_waves)
  n <- as.integer(panel$N)
  delta <- result$params$delta
  delta <- pmax(delta, 0)
  if (!sum(delta)) delta <- rep(1, K)
  tau <- result$params$tau
  means <- result$params$means
  Jg <- as.integer(result$n_gaussian %||% 0L)
  gidx <- result$gaussian_index
  mean_blocks <- max(as.integer(result$n_mean_blocks %||% 1L), 1L)
  cov_blocks <- max(as.integer(result$n_cov_blocks %||% 1L), 1L)
  meas_blocks <- max(as.integer(result$n_meas_blocks %||% 1L), 1L)
  indicators <- panel$indicators
  rho <- result$params$rho
  rmax <- as.integer(result$Rmax %||% 1L)
  if (is.null(masks)) masks <- .subject_masks(panel)
  n_rows <- n * waves
  # Factors below are built inside this draw. The next draw builds them again.
  # They are not hoisted across the bootstrap. A gaussian draw has no
  # categorical sample between Normal draws, so one rnorm() call is the same
  # stream as one rnorm(Jg) per subject-wave. sample.int(K, n, replace = TRUE)
  # matches n calls of sample.int(K, 1) for a single wave. A later wave's
  # transition depends on the subject, so those draws stay in subject order.
  if (Jg > 0L && (is.null(rho) || !length(rho))) {
    if (waves == 1L) {
      states <- sample.int(K, n, replace = TRUE, prob = delta)
    } else {
      states <- integer(n_rows)
      pos <- 0L
      for (i in seq_len(n)) {
        state <- sample.int(K, 1L, prob = delta)
        states[[pos + 1L]] <- state
        if (waves >= 2L) {
          for (t in seq_len(waves - 1L)) {
            block <- if (isTRUE(result$transition_equal)) 0L else (t - 1L)
            probs <- tau[(block * K + (state - 1L)) * K + seq_len(K)]
            probs <- pmax(probs, 0)
            if (!sum(probs)) probs <- rep(1, K)
            state <- sample.int(K, 1L, prob = probs)
            states[[pos + t + 1L]] <- state
          }
        }
        pos <- pos + waves
      }
    }
    chol_cache <- vector("list", cov_blocks * K)
    for (cb in seq_len(cov_blocks) - 1L) {
      for (state in seq_len(K)) {
        chol_cache[[cb * K + state]] <- t(chol(.sigma_state(result, state, cb)))
      }
    }
    z <- matrix(stats::rnorm(n_rows * Jg), nrow = Jg)
    id_col <- rep.int(seq_len(n), rep.int(waves, n))
    wave_col <- rep(panel$waves, times = n)
    value_cols <- vector("list", length(indicators))
    names(value_cols) <- indicators
    for (col in indicators) value_cols[[col]] <- rep(NA_real_, n_rows)
    t_index <- rep(seq_len(waves), times = n)
    row_cb <- if (cov_blocks == 1L) rep(0L, n_rows) else pmin(t_index - 1L, cov_blocks - 1L)
    row_mb <- if (mean_blocks == 1L) rep(0L, n_rows) else pmin(t_index - 1L, mean_blocks - 1L)
    for (state in seq_len(K)) {
      for (cb in seq_len(cov_blocks) - 1L) {
        idx <- which(states == state & row_cb == cb)
        if (!length(idx)) next
        factor <- chol_cache[[cb * K + state]]
        for (mb in unique(row_mb[idx])) {
          sub <- idx[row_mb[idx] == mb]
          mu <- means[(mb * K + (state - 1L)) * Jg + seq_len(Jg)]
          filled <- mu + factor %*% z[, sub, drop = FALSE]
          for (g in seq_len(Jg)) {
            value_cols[[indicators[[gidx[[g]] + 1L]]]][sub] <- filled[g, ]
          }
        }
      }
    }
    subj <- rep.int(seq_len(n), rep.int(waves, n))
    donor <- donors[subj]
    for (col in seq_along(indicators)) {
      drop <- masks[cbind(donor, t_index, col)]
      if (any(drop)) value_cols[[indicators[[col]]]][drop] <- NA_real_
    }
    out <- data.frame(id = id_col, wave = wave_col, stringsAsFactors = FALSE)
    for (col in indicators) out[[col]] <- value_cols[[col]]
    return(out)
  }
  paths <- vector("list", n)
  for (i in seq_len(n)) {
    state <- sample.int(K, 1, prob = delta)
    path <- state
    if (waves >= 2L) {
      for (t in seq_len(waves - 1L)) {
        block <- if (isTRUE(result$transition_equal)) 0L else (t - 1L)
        probs <- tau[(block * K + (state - 1L)) * K + seq_len(K)]
        probs <- pmax(probs, 0)
        if (!sum(probs)) probs <- rep(1, K)
        state <- sample.int(K, 1, prob = probs)
        path <- c(path, state)
      }
    }
    paths[[i]] <- path
  }
  id_col <- integer(n_rows)
  wave_col <- rep(panel$waves, each = n)
  value_cols <- vector("list", length(indicators))
  names(value_cols) <- indicators
  for (col in indicators) value_cols[[col]] <- rep(NA_real_, n_rows)
  cursor <- 0L
  for (i in seq_len(n)) {
    donor <- donors[[i]]
    for (t in seq_len(waves)) {
      cursor <- cursor + 1L
      id_col[[cursor]] <- i
      wave_col[[cursor]] <- panel$waves[[t]]
      state <- paths[[i]][[t]]
      if (Jg > 0L) {
        mb <- if (mean_blocks == 1L) 0L else min(t - 1L, mean_blocks - 1L)
        cb <- if (cov_blocks == 1L) 0L else min(t - 1L, cov_blocks - 1L)
        mean <- means[(mb * K + (state - 1L)) * Jg + seq_len(Jg)]
        draw <- as.numeric(mean + t(chol(.sigma_state(result, state, cb))) %*% stats::rnorm(Jg))
        for (g in seq_len(Jg)) value_cols[[indicators[[gidx[[g]] + 1L]]]][[cursor]] <- draw[[g]]
      }
      if (!is.null(rho) && length(rho)) {
        mb <- if (meas_blocks == 1L) 0L else (t - 1L)
        for (col in seq_along(indicators)) {
          if (identical(panel$family[[col]], "gaussian")) next
          ncat <- panel$ncat[[col]]
          probs <- vapply(seq_len(ncat), function(cat) {
            rho[((mb * panel$J + (col - 1L)) * K + (state - 1L)) * rmax + cat]
          }, numeric(1))
          probs <- pmax(probs, 0)
          if (!sum(probs)) probs <- rep(1, ncat)
          code <- sample.int(ncat, 1, prob = probs) - 1L
          order <- panel$level_order[[col]]
          value_cols[[indicators[[col]]]][[cursor]] <- if (!is.null(order) && code + 1L <= length(order)) {
            order[[code + 1L]]
          } else {
            code
          }
        }
      }
      for (col in seq_along(indicators)) {
        if (isTRUE(masks[donor, t, col])) value_cols[[indicators[[col]]]][[cursor]] <- NA_real_
      }
    }
  }
  out <- data.frame(id = id_col, wave = wave_col, stringsAsFactors = FALSE)
  for (col in indicators) out[[col]] <- value_cols[[col]]
  out
}

.split_null_state <- function(source, k1) {
  # Likelihood-preserving split of state 0. NULL means the stored vectors do
  # not match the layout, and the caller retries without this candidate.
  result <- source$result
  params <- result$params
  if (is.null(params)) params <- list()
  k0 <- as.integer(result$K %||% 0L)
  if (k0 < 1L) return(NULL)
  panel <- source$panel
  cov <- result$covariance
  if (is.null(cov) || !nzchar(cov)) cov <- "EEI"
  tau <- as.numeric(params$tau)
  means <- as.numeric(params$means)
  if (is.null(result$n_tau_blocks)) {
    n_tau <- if (!length(tau)) 0L else as.integer(length(tau) / (k0 * k0))
  } else {
    n_tau <- as.integer(result$n_tau_blocks)
  }
  if (is.null(result$gaussian_index)) {
    n_mean <- if (length(means)) 1L else 0L
    n_gaussian <- if (n_mean) as.integer(length(means) / (n_mean * k0)) else 0L
    n_cov <- if (length(params$cov)) 1L else 0L
  } else {
    n_gaussian <- length(result$gaussian_index)
    n_mean <- as.integer(if (is.null(result$n_mean_blocks)) (if (n_gaussian) 1L else 0L) else result$n_mean_blocks)
    n_cov <- as.integer(if (is.null(result$n_cov_blocks)) (if (n_gaussian) 1L else 0L) else result$n_cov_blocks)
  }
  rho <- as.numeric(params$rho)
  n_meas <- if (is.null(result$n_meas_blocks)) (if (length(rho)) 1L else 0L) else as.integer(result$n_meas_blocks)
  J <- as.integer(if (!is.null(panel$J)) panel$J else result$n_indicators %||% 0L)
  R <- as.integer(if (!is.null(panel$Rmax)) panel$Rmax else result$Rmax %||% 1L)
  cpp_split_null_state(params, k0, as.integer(k1), cov, n_tau, n_meas, n_mean, n_cov, n_gaussian, J, R)
}

.fit_like <- function(template, frame, seed, n_starts = NULL, n_full = NULL, max_iter = NULL, start = NULL,
                      append_start = FALSE) {
  spec <- template$spec
  result <- template$result
  starts <- if (is.null(n_starts)) max(as.integer(spec$n_starts %||% 2L), 2L) else max(as.integer(n_starts), 2L)
  full <- if (is.null(n_full)) max(as.integer(spec$n_full %||% spec$n_starts %||% 2L), 2L) else max(as.integer(n_full), 2L)
  cap <- if (is.null(max_iter)) as.integer(spec$max_iter %||% 200L) else as.integer(max_iter)
  controls <- list(
    n_starts = starts,
    screen_iter = as.integer(spec$screen_iter %||% 0L),
    n_full = full,
    max_iter = cap,
    tol = as.numeric(spec$tol %||% 1e-7),
    replicate_tol = as.numeric(spec$replicate_tol %||% 0.1),
    n_threads = as.integer(spec$n_threads %||% 1L),
    seed = as.numeric(seed),
    align_labels = FALSE
  )
  if (!is.null(start)) {
    controls$start <- start
    if (isTRUE(append_start)) controls$append_start <- TRUE
  }
  if (is.list(frame) && !is.data.frame(frame) && !is.null(frame$N) &&
      (!is.null(frame$gauss) || !is.null(frame$cat))) {
    return(.fit_prepared(template, frame, controls))
  }
  if (is.data.frame(frame)) {
    prepared <- tryCatch(.prepare_bootstrap_panel(template, frame), error = function(e) NULL)
    if (!is.null(prepared)) return(.fit_prepared(template, prepared, controls))
  }
  indicators <- template$panel$indicators
  family <- stats::setNames(as.list(template$panel$family), indicators)
  model <- template$model
  if (identical(model, "lta") || as.integer(result$n_waves) >= 2L) {
    transitions <- if (isTRUE(spec$transition_equal %||% TRUE)) "equal" else "free"
    return(do.call(fit_lta, c(list(
      frame, indicators, id = "id", wave = "wave", n_states = result$K,
      covariance = if (nzchar(.cov_key(result$covariance))) result$covariance else "EEI",
      family = family, transitions = transitions,
      measurement_invariant = isTRUE(spec$measurement_invariant %||% TRUE),
      means_invariant = isTRUE(spec$means_invariant %||% TRUE),
      covariance_invariant = isTRUE(spec$covariance_invariant %||% TRUE)
    ), controls)))
  }
  if (identical(model, "lca") || !nzchar(.cov_key(result$covariance))) {
    return(do.call(fit_lca, c(list(
      frame, indicators, id = "id", n_states = result$K, family = family
    ), controls)))
  }
  do.call(fit_lpa, c(list(
    frame, indicators, id = "id", n_states = result$K,
    covariance = result$covariance, family = family
  ), controls))
}

.fit_prepared <- function(template, panel, controls) {
  spec <- template$spec
  result <- template$result
  model <- template$model
  if (identical(model, "lta") || as.integer(result$n_waves) >= 2L) {
    transitions <- if (isTRUE(spec$transition_equal %||% TRUE)) "equal" else "free"
    built <- do.call(lta_spec, c(list(
      result$K,
      covariance = if (nzchar(.cov_key(result$covariance))) result$covariance else "EEI",
      transitions = transitions,
      measurement_invariant = isTRUE(spec$measurement_invariant %||% TRUE),
      means_invariant = isTRUE(spec$means_invariant %||% TRUE),
      covariance_invariant = isTRUE(spec$covariance_invariant %||% TRUE)
    ), controls))
    model_name <- "lta"
  } else if (identical(model, "lca") || !nzchar(.cov_key(result$covariance))) {
    built <- do.call(lca_spec, c(list(result$K), controls))
    model_name <- "lca"
  } else {
    built <- do.call(lpa_spec, c(list(result$K, covariance = result$covariance), controls))
    model_name <- model
  }
  .new_fit(cpp_fit(panel, built), panel, built, model_name)
}

.generating_schema <- function(template) {
  panel <- template$panel
  list(
    indicators = panel$indicators,
    family = panel$family,
    ncat = panel$ncat,
    level_order = panel$level_order,
    waves = panel$waves,
    measurement = panel$measurement,
    Rmax = panel$Rmax
  )
}

.prepare_bootstrap_panel <- function(template, frame) {
  indicators <- template$panel$indicators
  family <- stats::setNames(as.list(template$panel$family), indicators)
  wave <- if (identical(template$model, "lta") || as.integer(template$result$n_waves) >= 2L) "wave" else NULL
  prepare_panel(frame, indicators, "id", wave, family = family, schema = .generating_schema(template))
}

# Likelihood and parameters of the returned start. A rejected start, including
# a nonconverged start with a higher log-likelihood, is never substituted.
.blrt_solution <- function(fit) {
  result <- fit$result
  loglik <- result$loglik
  finite <- length(loglik) == 1L && is.finite(loglik)
  converged <- isTRUE(result$converged)
  if (is.null(result$admissible)) {
    admissible <- converged && finite && !isTRUE(result$regularized) &&
      (is.null(result$cov_positive_definite) || isTRUE(result$cov_positive_definite)) &&
      !isTRUE(result$cov_collapsed_component)
  } else {
    admissible <- isTRUE(result$admissible) && finite && converged
  }
  n_best <- as.integer(result$n_best %||% 0L)
  list(
    loglik = if (admissible) as.numeric(loglik) else NA_real_,
    converged = converged,
    admissible = admissible,
    replicated = admissible && n_best >= 2L,
    n_best = n_best,
    usable = admissible
  )
}

.higher_admissible_hidden <- function(result) {
  extra <- result$rejected_loglik
  reasons <- result$rejected_reason
  if (!length(extra) || !is.finite(result$loglik)) return(FALSE)
  blocked <- c("not_converged", "numerical_failure", "not_positive_definite", "variance_floor", "collapsed_component")
  for (i in seq_along(extra)) {
    if (!is.finite(extra[[i]]) || extra[[i]] <= result$loglik + 1e-8) next
    reason <- if (length(reasons) >= i) as.character(reasons[[i]]) else ""
    if (!any(vapply(blocked, function(token) grepl(token, reason, fixed = TRUE), logical(1)))) return(TRUE)
  }
  FALSE
}

.dataset_hash <- function(frame) {
  # Same frame must hash the same within a run so a retry is tied to its
  # dataset. serialize avoids printing the table through write.table.
  raw <- serialize(lapply(frame, function(col) {
    if (is.numeric(col)) as.numeric(col) else as.character(col)
  }), NULL, version = 2)
  sprintf("%d:%d", length(raw), sum(as.integer(raw)))
}

# Wald SE of a binomial proportion. It is 0 when b is 0 or b is n.
# The Wilson interval is the uncertainty reported in that case.
.wilson_ci <- function(b, n, z = 1.959963984540054) {
  if (!is.finite(n) || n < 1) return(c(NA_real_, NA_real_))
  phat <- b / n
  z2 <- z * z
  denom <- 1 + z2 / n
  center <- (phat + z2 / (2 * n)) / denom
  half <- z * sqrt(phat * (1 - phat) / n + z2 / (4 * n * n)) / denom
  low <- center - half
  high <- center + half
  if (low < 0 || b == 0) low <- 0
  if (high > 1 || b == n) high <- 1
  c(low, high)
}

.blrt_summary <- function(B, b, n_ok, n_failed, n_not_replicated, n_failed_not_converged,
                          n_failed_error, seconds, warn, failure_text) {
  p <- if (n_ok > 0L) b / n_ok else NA_real_
  p_plus <- if (n_ok > 0L) (1 + b) / (1 + n_ok) else NA_real_
  mc_se <- if (n_ok > 0L) sqrt(p * (1 - p) / n_ok) else NA_real_
  ci <- .wilson_ci(b, n_ok)
  reason <- paste0(
    "BLRT requested B=", B, ". Kept ", n_ok, " draws. ",
    n_not_replicated, " kept draws did not replicate the best log-likelihood. ",
    "Dropped ", n_failed, " draws",
    " (not converged: ", n_failed_not_converged,
    ", error: ", n_failed_error, "). ",
    failure_text, " ",
    "Successful draws=", n_ok, ". b=", b, ". ",
    "Mplus TECH14 p=b/n_ok=", format(p, digits = 6),
    " with n_ok the kept draws. ",
    "p_plus=(b+1)/(n_ok+1)=", format(p_plus, digits = 6),
    " is secondary and is not compared with alpha."
  )
  if (isTRUE(warn)) {
    reason <- paste0(
      reason,
      " THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED.",
      " THE SOLUTION MAY NOT BE TRUSTWORTHY DUE TO LOCAL MAXIMA.",
      " INCREASE THE NUMBER OF RANDOM STARTS."
    )
  }
  reason <- paste0(
    reason,
    " Unreplicated kept solutions are a diagnostic and are not a validation failure.",
    " Monte Carlo SE=", if (is.finite(mc_se)) format(mc_se, digits = 6) else "NA",
    ". Wilson 95% CI [",
    if (is.finite(ci[[1]])) format(ci[[1]], digits = 6) else "NA", ", ",
    if (is.finite(ci[[2]])) format(ci[[2]], digits = 6) else "NA",
    "]. Independent bootstrap p-values are not required to match.",
    " validated stays false until Monte Carlo agreement, shared-dataset likelihoods, and operating characteristics are shown."
  )
  list(p = p, p_plus = p_plus, mc_se = mc_se, p_ci_low = ci[[1]], p_ci_high = ci[[2]], reason = reason)
}

#' @export
blrt_hardware_threads <- function() {
  as.integer(cpp_blrt_hardware_threads())
}

.blrt_fit_spec <- function(template, n_starts, n_full, max_iter) {
  spec <- template$spec
  result <- template$result
  controls <- list(
    n_starts = as.integer(n_starts),
    screen_iter = as.integer(spec$screen_iter %||% 0L),
    n_full = as.integer(n_full),
    max_iter = as.integer(max_iter),
    tol = as.numeric(spec$tol %||% 1e-7),
    replicate_tol = as.numeric(spec$replicate_tol %||% 0.1),
    n_threads = 1L,
    seed = 1,
    align_labels = FALSE
  )
  indicators <- template$panel$indicators
  family <- stats::setNames(as.list(template$panel$family), indicators)
  model <- template$model
  if (identical(model, "lta") || as.integer(result$n_waves) >= 2L) {
    transitions <- if (isTRUE(spec$transition_equal %||% TRUE)) "equal" else "free"
    out <- do.call(lta_spec, c(list(
      result$K,
      covariance = if (nzchar(.cov_key(result$covariance))) result$covariance else "EEI",
      transitions = transitions,
      measurement_invariant = isTRUE(spec$measurement_invariant %||% TRUE),
      means_invariant = isTRUE(spec$means_invariant %||% TRUE),
      covariance_invariant = isTRUE(spec$covariance_invariant %||% TRUE)
    ), controls))
  } else if (identical(model, "lca") || !nzchar(.cov_key(result$covariance))) {
    out <- do.call(lca_spec, c(list(result$K), controls))
  } else {
    out <- do.call(lpa_spec, c(list(result$K, covariance = result$covariance), controls))
  }
  out$family_map <- family
  out
}

.blrt_print_worker_load <- function(jobs, busy) {
  if (!nzchar(Sys.getenv("LTA_PROFILE_WORKERS", ""))) return(invisible(NULL))
  jobs <- as.integer(jobs)
  busy <- as.numeric(busy)
  if (!length(jobs)) return(invisible(NULL))
  parts <- vapply(seq_along(jobs), function(i) {
    sprintf("w%d_jobs=%d w%d_busy_s=%.6f", i - 1L, jobs[[i]], i - 1L, busy[[i]])
  }, character(1))
  cat("worker_load n=", length(jobs), " ", paste(parts, collapse = " "), "\n", sep = "")
  utils::flush.console()
  invisible(NULL)
}

.blrt_batch_size <- function(B, n_threads, batch_size) {
  requested <- as.integer(B)
  threads <- as.integer(n_threads)
  if (!is.finite(threads) || threads < 1L) stop("n_threads must be at least 1", call. = FALSE)
  batch <- if (is.null(batch_size)) threads else as.integer(batch_size)
  if (!is.finite(batch) || batch < 1L) stop("batch_size must be at least 1", call. = FALSE)
  if (is.finite(requested) && requested > 0L && batch > requested) batch <- requested
  batch
}

.blrt_draw_pool <- function(h0, h1, B, max_retries, base, cap, s0, f0, s1, f1, n_threads,
                            batch_size = NULL) {
  requested <- as.integer(B)
  batch <- .blrt_batch_size(requested, n_threads, batch_size)
  hashes <- character(requested)
  n <- h0$panel$N
  masks <- .subject_masks(h0$panel)
  spec0 <- .blrt_fit_spec(h0, s0, f0, cap)
  spec1 <- .blrt_fit_spec(h1, s1, f1, cap)
  seconds_prepare <- 0
  ok <- logical()
  reason <- character()
  lr <- numeric()
  h0_replicated <- logical()
  h1_replicated <- logical()
  h1_best_fit_replicated <- logical()
  h1_replication_assessed <- logical()
  attempt_draw <- integer()
  attempt_retry <- integer()
  attempt_starts <- integer()
  attempt_starts_h0 <- integer()
  attempt_starts_h1 <- integer()
  attempt_h1_embedded <- logical()
  worker_jobs <- integer()
  worker_busy_s <- numeric()
  n_threads_used <- 1L
  seconds_convert <- 0
  seconds_engine <- 0
  seconds_export <- 0
  # An exception here, or inside cpp_blrt_fit_draws, leaves this function.
  # Earlier batches are not returned. R polls the user interrupt between
  # waves inside one batch. Python's binding does not.
  starts <- if (requested > 0L) seq(1L, requested, by = batch) else integer()
  for (start in starts) {
    stop_at <- min(start + batch - 1L, requested)
    draws <- if (requested > 0L) seq.int(start, stop_at) else integer()
    frames <- vector("list", length(draws))
    present <- logical(length(draws))
    prep_t0 <- proc.time()[["elapsed"]]
    for (local in seq_along(draws)) {
      draw <- draws[[local]]
      set.seed(base + 1000 * draw)
      donors <- sample.int(n, n, replace = TRUE)
      frame <- tryCatch(.simulate_fit(h0, donors, masks), error = function(e) NULL)
      if (is.null(frame)) next
      prepared <- tryCatch(.prepare_bootstrap_panel(h0, frame), error = function(e) NULL)
      if (is.null(prepared)) next
      frames[[local]] <- prepared
      hashes[[draw]] <- .dataset_hash(frame)
      present[[local]] <- TRUE
    }
    seconds_prepare <- seconds_prepare + (proc.time()[["elapsed"]] - prep_t0)
    part <- cpp_blrt_fit_draws(
      frames, present, spec0, spec1, as.numeric(base), as.integer(max_retries), as.integer(n_threads),
      as.integer(draws)
    )
    rm(frames)
    ok <- c(ok, part$ok)
    reason <- c(reason, part$reason)
    lr <- c(lr, part$lr)
    h0_replicated <- c(h0_replicated, part$h0_replicated)
    h1_replicated <- c(h1_replicated, part$h1_replicated)
    h1_best_fit_replicated <- c(h1_best_fit_replicated, part$h1_best_fit_replicated)
    h1_replication_assessed <- c(h1_replication_assessed, part$h1_replication_assessed)
    attempt_draw <- c(attempt_draw, part$attempt_draw)
    attempt_retry <- c(attempt_retry, part$attempt_retry)
    attempt_starts <- c(attempt_starts, part$attempt_starts)
    attempt_starts_h0 <- c(attempt_starts_h0, part$attempt_starts_h0)
    attempt_starts_h1 <- c(attempt_starts_h1, part$attempt_starts_h1)
    attempt_h1_embedded <- c(attempt_h1_embedded, part$attempt_h1_embedded)
    jobs <- as.integer(part$worker_jobs)
    busy <- as.numeric(part$worker_busy_s)
    width <- max(length(worker_jobs), length(jobs))
    if (length(worker_jobs) < width) {
      worker_jobs <- c(worker_jobs, rep(0L, width - length(worker_jobs)))
      worker_busy_s <- c(worker_busy_s, rep(0, width - length(worker_busy_s)))
    }
    if (length(jobs)) {
      worker_jobs[seq_along(jobs)] <- worker_jobs[seq_along(jobs)] + jobs
      worker_busy_s[seq_along(busy)] <- worker_busy_s[seq_along(busy)] + busy
    }
    n_threads_used <- max(n_threads_used, as.integer(part$n_threads_used))
    seconds_convert <- seconds_convert + as.numeric(part$seconds_convert)
    seconds_engine <- seconds_engine + as.numeric(part$seconds_engine)
    seconds_export <- seconds_export + as.numeric(part$seconds_export)
  }
  raw <- list(
    ok = ok, reason = reason, lr = lr,
    h0_replicated = h0_replicated, h1_replicated = h1_replicated,
    h1_best_fit_replicated = h1_best_fit_replicated,
    h1_replication_assessed = h1_replication_assessed,
    attempt_draw = attempt_draw, attempt_retry = attempt_retry,
    attempt_starts = attempt_starts, attempt_starts_h0 = attempt_starts_h0,
    attempt_starts_h1 = attempt_starts_h1, attempt_h1_embedded = attempt_h1_embedded,
    worker_jobs = worker_jobs, worker_busy_s = worker_busy_s,
    n_threads_used = n_threads_used,
    seconds_convert = seconds_convert, seconds_engine = seconds_engine,
    seconds_export = seconds_export
  )
  asm_t0 <- proc.time()[["elapsed"]]
  lrs <- numeric(as.integer(B))
  lr_draws <- integer(as.integer(B))
  fail_draw <- integer(as.integer(B))
  fail_reason <- character(as.integer(B))
  n_ok <- 0L
  n_failed <- 0L
  n_failed_not_converged <- 0L
  n_failed_error <- 0L
  n_failed_inadmissible <- 0L
  n_not_replicated <- 0L
  n_retried <- sum(raw$attempt_retry > 0L)
  for (draw in seq_len(as.integer(B))) {
    if (!isTRUE(raw$ok[[draw]])) {
      why <- as.character(raw$reason[[draw]])
      if (!nzchar(why)) why <- "error"
      n_failed <- n_failed + 1L
      fail_draw[[n_failed]] <- draw
      fail_reason[[n_failed]] <- why
      if (identical(why, "not_converged")) n_failed_not_converged <- n_failed_not_converged + 1L
      else if (identical(why, "inadmissible")) n_failed_inadmissible <- n_failed_inadmissible + 1L
      else n_failed_error <- n_failed_error + 1L
      next
    }
    n_ok <- n_ok + 1L
    lrs[[n_ok]] <- raw$lr[[draw]]
    lr_draws[[n_ok]] <- draw
    assessed <- if (is.null(raw$h1_replication_assessed)) TRUE else isTRUE(raw$h1_replication_assessed[[draw]])
    if (!isTRUE(raw$h0_replicated[[draw]]) || (assessed && !isTRUE(raw$h1_replicated[[draw]]))) {
      n_not_replicated <- n_not_replicated + 1L
    }
  }
  lrs <- lrs[seq_len(n_ok)]
  lr_draws <- lr_draws[seq_len(n_ok)]
  failures <- data.frame(draw = fail_draw[seq_len(n_failed)], reason = fail_reason[seq_len(n_failed)],
                         stringsAsFactors = FALSE)
  seconds_assemble <- proc.time()[["elapsed"]] - asm_t0
  .blrt_print_worker_load(raw$worker_jobs, raw$worker_busy_s)
  attempts <- data.frame(
    draw = as.integer(raw$attempt_draw),
    retry = as.integer(raw$attempt_retry),
    dataset_hash = hashes[as.integer(raw$attempt_draw)],
    n_starts = as.integer(raw$attempt_starts),
    n_starts_h0 = as.integer(if (is.null(raw$attempt_starts_h0)) raw$attempt_starts else raw$attempt_starts_h0),
    n_starts_h1 = as.integer(if (is.null(raw$attempt_starts_h1)) raw$attempt_starts else raw$attempt_starts_h1),
    stringsAsFactors = FALSE
  )
  list(
    lrs = lrs, lr_draws = lr_draws, failures = failures, attempts = attempts,
    n_failed = n_failed, n_failed_not_converged = n_failed_not_converged,
    n_failed_error = n_failed_error, n_failed_inadmissible = n_failed_inadmissible,
    n_not_replicated = n_not_replicated, n_retried = as.integer(n_retried),
    n_threads_used = as.integer(raw$n_threads_used),
    worker_jobs = as.integer(raw$worker_jobs),
    worker_busy_s = as.numeric(raw$worker_busy_s),
    seconds_prepare = seconds_prepare,
    seconds_convert = as.numeric(raw$seconds_convert),
    seconds_engine = as.numeric(raw$seconds_engine),
    seconds_export = as.numeric(raw$seconds_export),
    seconds_assemble = seconds_assemble
  )
}

# Plus-one bounds over the scheduled B. Exceedance is LR_b >= LR_obs.
# Failed draws f and unattempted draws n_not_run are both unresolved:
# n_unresolved = f + n_not_run.
# p_lo = (b+1)/(B+1) counts unresolved draws as non-exceedances.
# p_hi = (b+n_unresolved+1)/(B+1) counts them as exceedances.
# When n_not_run is 0 this is (b+f+1)/(B+1).
# bounds_decision uses only that interval. decision copies it, except that
# any unattempted draw forces decision to inconclusive. That force is an
# extra conservative rule and is not the bounds decision. Any later BLRT
# operating-characteristic study scores the returned decision, override
# included. bounds_decision stays separate and is not that study's call.
# A check that did not run, or B < 1, is not_run and both bounds are undefined.
# validated stays FALSE. The bounds do not select a model.
#' @export
blrt_failure_bounds <- function(b, f, B, alpha = 0.05, ran = TRUE, n_not_run = 0L) {
  b <- as.integer(b)
  f <- as.integer(f)
  requested <- as.integer(B)
  skipped <- as.integer(n_not_run)
  alpha <- as.numeric(alpha)
  if (any(!is.finite(c(b, f, requested, skipped, alpha))) || b < 0L || f < 0L || requested < 0L || skipped < 0L) {
    stop("b, f, B, and n_not_run must be non-negative", call. = FALSE)
  }
  if (alpha < 0 || alpha > 1) stop("alpha must be in [0, 1]", call. = FALSE)
  unresolved <- f + skipped
  out <- list(
    b = b, f = f, B = requested, n_not_run = skipped, n_unresolved = unresolved,
    alpha = alpha, p_lo = NA_real_, p_hi = NA_real_,
    bounds_decision = "not_run", decision = "not_run", validated = FALSE,
    exceedance = "LR_b >= LR_obs"
  )
  if (!isTRUE(ran) || requested < 1L) return(out)
  out$p_lo <- (b + 1) / (requested + 1)
  out$p_hi <- (b + unresolved + 1) / (requested + 1)
  out$bounds_decision <- if (out$p_hi <= alpha) "reject" else if (out$p_lo > alpha) "retain" else "inconclusive"
  # Extra conservative rule, separate from the interval above.
  out$decision <- if (skipped > 0L) "inconclusive" else out$bounds_decision
  out
}

.blrt_exceeds <- function(lr_b, lr_obs) {
  lr_b >= lr_obs - 1e-8
}

.stamp_experimental_blrt <- function(out) {
  out$validated <- FALSE
  out$experimental <- TRUE
  out$switched <- FALSE
  failed <- as.integer(out$n_failed %||% 0L)
  kept <- as.integer(out$n_ok %||% 0L)
  requested <- as.integer(out$B %||% 0L)
  skipped <- requested - kept - failed
  if (skipped < 0L) skipped <- 0L
  bounds <- blrt_failure_bounds(
    as.integer(out$b %||% 0L), failed, requested,
    as.numeric(out$alpha %||% 0.05), ran = isTRUE(out$ran), n_not_run = skipped
  )
  out$f <- failed
  out$n_not_run <- bounds$n_not_run
  out$n_unresolved <- bounds$n_unresolved
  out$p_lo <- bounds$p_lo
  out$p_hi <- bounds$p_hi
  out$bounds_decision <- bounds$bounds_decision
  out$decision <- bounds$decision
  if (!grepl("blrt validated=false", out$reason, fixed = TRUE)) {
    out$reason <- paste0(out$reason, " blrt validated=false.")
  }
  if (isTRUE(out$ran)) {
    out$reason <- paste0(
      out$reason,
      " Experimental bounds p_lo=(b+1)/(B+1)=", format(bounds$p_lo, digits = 6),
      ", p_hi=(b+n_unresolved+1)/(B+1)=", format(bounds$p_hi, digits = 6),
      " with n_unresolved=f+n_not_run=", bounds$n_unresolved,
      ", bounds_decision=", bounds$bounds_decision,
      ", decision=", bounds$decision, ".",
      " Exceedance is LR_b >= LR_obs.",
      " Unattempted draws are unresolved in the bounds over the scheduled B.",
      " Any unattempted draw also forces decision=inconclusive.",
      " That force is an extra conservative rule and is not the bounds decision.",
      " These bounds do not change automatic selection."
    )
  }
  out$report_only <- TRUE
  out
}

.blrt_telemetry_env <- new.env(parent = emptyenv())

#' Telemetry from the most recent ``blrt_check``. It is not a field of that result.
#' @export
blrt_last_telemetry <- function() {
  if (!exists("value", envir = .blrt_telemetry_env, inherits = FALSE)) return(list())
  .blrt_telemetry_env$value
}

.publish_blrt <- function(out, telemetry) {
  .blrt_telemetry_env$value <- telemetry
  out
}

# Parametric bootstrap likelihood-ratio test. p = b/n_ok when at least one
# draw is kept, and is NA when none is kept. p_plus = (b+1)/(n_ok+1) uses
# that same count. p_lo and p_hi still divide by the requested B+1.
# The likelihood is the returned admissible converged start. A rejected start
# is never substituted. Each bootstrap dataset is simulated once. A retry
# adds starts on that same dataset. validated stays FALSE until Monte Carlo
# agreement, shared-dataset likelihoods, and operating characteristics are
# shown. Independent bootstrap p-values are not required to match.
# select_model does not let an unvalidated result change the selected model.
# batch_size bounds how many simulated datasets the threaded pool holds.
# The default is n_threads. n_threads == 1 still uses the one-draw serial loop.
#' Bootstrap likelihood-ratio test.
#'
#' @param h0,h1 Fitted null and alternative models.
#' @param B Number of bootstrap draws.
#' @param max_retries Extra attempts on the same simulated dataset.
#' @param alpha Tail probability used by the bounds.
#' @param seed Bootstrap seed. \code{NULL} uses the null fit's seed.
#' @param max_iter Bootstrap EM cap. \code{NULL} uses the selection rule.
#' @param n_threads Worker count.
#' @param experimental Must be \code{TRUE} or the bootstrap does not run.
#' @param batch_size How many simulated datasets the pool holds at once.
#'   \code{NULL} uses \code{n_threads}.
#' @export
blrt_check <- function(h0, h1, B = 1000, max_retries = 3, alpha = 0.05, seed = NULL, max_iter = NULL,
                       n_threads = 1L, experimental = FALSE, batch_size = NULL) {
  if (!isTRUE(experimental)) {
    message(
      "not run: experimental BLRT requires explicit opt-in (experimental=TRUE). ",
      "blrt validated=false. Excluded from automatic selection."
    )
    return(.publish_blrt(list(
      ran = FALSE, validated = FALSE, experimental = FALSE, decision = "not_run",
      report_only = TRUE, switched = FALSE,
      p = NA_real_, p_plus = NA_real_, p_lo = NA_real_, p_hi = NA_real_,
      b = 0L, f = 0L, B = as.integer(B), alpha = as.numeric(alpha),
      reason = paste0(
        "not run: experimental BLRT requires explicit opt-in (experimental=TRUE). ",
        "blrt validated=false. Excluded from automatic selection."
      )
    ), list()))
  }
  s0 <- max(as.integer(h0$spec$n_starts %||% 2L), 2L)
  s1 <- max(as.integer(h1$spec$n_starts %||% 2L), 2L)
  f0 <- max(as.integer(h0$spec$n_full %||% s0), 2L)
  f1 <- max(as.integer(h1$spec$n_full %||% s1), 2L)
  runs <- as.integer(B) * (s0 + s1)
  cap <- if (is.null(max_iter)) as.integer(selection_rule()$blrt_max_iter) else as.integer(max_iter)
  if (!is.finite(cap) || cap < 1L) stop("blrt max_iter must be at least 1", call. = FALSE)
  n_threads <- as.integer(n_threads)
  if (!is.finite(n_threads) || n_threads < 1L) stop("n_threads must be at least 1", call. = FALSE)
  cost <- paste0(
    "BLRT estimated cost: B=", as.integer(B), " draws x (H0 starts=", s0, " + H1 starts=", s1,
    ") = ", runs, " EM runs before retries. Bootstrap fits use max_iter=", cap,
    ". Observed H0 and H1 fits keep their own max_iter. validated stays false until Monte Carlo agreement, shared-dataset likelihoods, and operating characteristics are shown."
  )
  message(cost)
  sol0 <- .blrt_solution(h0)
  sol1 <- .blrt_solution(h1)
  .blrt_material_ll <- 1e-5
  .h1_materially_below <- function(ll0, ll1) {
    is.finite(ll0) && is.finite(ll1) && ll1 < ll0 - .blrt_material_ll
  }
  out <- list(
    ran = TRUE, validated = FALSE, cost_note = cost, B = as.integer(B),
    starts_h0 = s0, starts_h1 = s1, estimated_runs = runs, alpha = as.numeric(alpha),
    p = NA_real_, p_plus = NA_real_, mc_se = NA_real_,
    p_ci_low = NA_real_, p_ci_high = NA_real_, p_ci_method = "wilson_95", b = 0L,
    lr_observed = 2 * (sol1$loglik - sol0$loglik),
    observed_h0 = sol0, observed_h1 = sol1,
    observed_h0_replicated = sol0$replicated,
    # replication_assessed means at least two starts were run. It does not mean
    # the best likelihood was found again. best_fit_replicated is that event,
    # and observed_h1_replicated keeps the same value.
    observed_h1_replicated = sol1$replicated,
    observed_h1_best_fit_replicated = sol1$replicated,
    observed_h1_replication_assessed = TRUE,
    n_ok = 0L, n_retried = 0L, n_failed = 0L, n_not_replicated = 0L,
    n_failed_not_converged = 0L, n_failed_error = 0L, n_failed_inadmissible = 0L,
    failures = data.frame(draw = integer(), reason = character()),
    attempts = data.frame(
      draw = integer(), retry = integer(), dataset_hash = character(),
      n_starts = integer(), n_starts_h0 = integer(), n_starts_h1 = integer()
    ),
    switched = FALSE, report_only = TRUE, max_iter = cap,
    n_threads = as.integer(n_threads),
    reason = "validated stays false until Monte Carlo agreement, shared-dataset likelihoods, and operating characteristics are shown.",
    p_provisional = FALSE
  )
  if (!isTRUE(sol0$usable) || !isTRUE(sol1$usable)) {
    out$ran <- FALSE
    out$lr_observed <- NA_real_
    out$reason <- "not run: observed fit is not usable"
    out <- .stamp_experimental_blrt(out)
    message(out$reason)
    return(.publish_blrt(out, list()))
  }
  .require_blrt_pair(h0, h1)
  base <- if (is.null(seed)) as.numeric(h0$spec$seed %||% 1) else as.numeric(seed)
  if (isTRUE(.h1_materially_below(sol0$loglik, sol1$loglik))) {
    embedded <- .split_null_state(h0, h1$result$K)
    if (!is.null(embedded)) {
      refit <- .fit_like(h1, h0$panel, base + 17, n_starts = s1, n_full = f1, max_iter = cap,
                         start = embedded, append_start = TRUE)
      sol1 <- .blrt_solution(refit)
      assessed <- as.integer(refit$result$n_starts %||% 0L) >= 2L
      out$observed_h1 <- sol1
      out$observed_h1_replication_assessed <- assessed
      out$observed_h1_replicated <- assessed && isTRUE(sol1$replicated)
      out$observed_h1_best_fit_replicated <- out$observed_h1_replicated
      out$lr_observed <- 2 * (sol1$loglik - sol0$loglik)
    }
    if (!isTRUE(sol1$usable) || isTRUE(.h1_materially_below(sol0$loglik, sol1$loglik))) {
      out$ran <- FALSE
      out$lr_observed <- NA_real_
      out$reason <- "not run: observed alternative is materially below the null"
      out <- .stamp_experimental_blrt(out)
      message(out$reason)
      return(.publish_blrt(out, list()))
    }
  }
  started <- proc.time()[["elapsed"]]
  lrs <- numeric()
  lr_draws <- integer()
  telemetry <- list(n_threads_used = 1L)
  if (n_threads > 1L) {
    pooled <- .blrt_draw_pool(h0, h1, B, max_retries, base, cap, s0, f0, s1, f1, n_threads, batch_size)
    lrs <- pooled$lrs
    lr_draws <- pooled$lr_draws
    out$failures <- pooled$failures
    out$attempts <- pooled$attempts
    out$n_failed <- pooled$n_failed
    out$n_failed_not_converged <- pooled$n_failed_not_converged
    out$n_failed_error <- pooled$n_failed_error
    out$n_failed_inadmissible <- pooled$n_failed_inadmissible
    out$n_not_replicated <- pooled$n_not_replicated
    out$n_retried <- pooled$n_retried
    telemetry$n_threads_used <- as.integer(pooled$n_threads_used)
    telemetry$worker_jobs <- pooled$worker_jobs
    telemetry$worker_busy_s <- pooled$worker_busy_s
    telemetry$seconds_prepare <- pooled$seconds_prepare
    telemetry$seconds_convert <- pooled$seconds_convert
    telemetry$seconds_engine <- pooled$seconds_engine
    telemetry$seconds_export <- pooled$seconds_export
    telemetry$seconds_assemble <- pooled$seconds_assemble
  } else {
  n <- h0$panel$N
  for (draw in seq_len(as.integer(B))) {
    ok <- FALSE
    why <- "error"
    set.seed(base + 1000 * draw)
    donors <- sample.int(n, n, replace = TRUE)
    frame <- tryCatch(.simulate_fit(h0, donors), error = function(e) NULL)
    if (is.null(frame)) {
      out$failures <- rbind(out$failures, data.frame(draw = draw, reason = "error"))
      out$n_failed <- out$n_failed + 1L
      out$n_failed_error <- out$n_failed_error + 1L
      next
    }
    dataset_hash <- .dataset_hash(frame)
    embed_next <- NULL
    for (retry in 0:as.integer(max_retries)) {
      if (retry > 0L) out$n_retried <- out$n_retried + 1L
      scale <- retry + 1L
      h1_start <- embed_next
      embed_next <- NULL
      embedded_this <- !is.null(h1_start)
      h1_count <- s1 * scale + if (embedded_this) 1L else 0L
      out$attempts <- rbind(out$attempts, data.frame(
        draw = draw, retry = retry, dataset_hash = dataset_hash,
        n_starts = s0 * scale, n_starts_h0 = s0 * scale, n_starts_h1 = h1_count,
        stringsAsFactors = FALSE
      ))
      fit0 <- tryCatch(
        .fit_like(h0, frame, base + 1000 * draw + retry, n_starts = s0 * scale, n_full = f0 * scale, max_iter = cap),
        error = function(e) NULL
      )
      fit1 <- tryCatch(
        if (is.null(h1_start)) {
          .fit_like(h1, frame, base + 200000 + 1000 * draw + retry, n_starts = s1 * scale, n_full = f1 * scale, max_iter = cap)
        } else {
          .fit_like(h1, frame, base + 200000 + 1000 * draw + retry, n_starts = s1 * scale, n_full = f1 * scale,
                    max_iter = cap, start = h1_start, append_start = TRUE)
        },
        error = function(e) NULL
      )
      if (is.null(fit0) || is.null(fit1)) {
        why <- "error"
        next
      }
      draw0 <- .blrt_solution(fit0)
      draw1 <- .blrt_solution(fit1)
      if (!draw0$usable || !draw1$usable) {
        why <- if (!draw0$converged || !draw1$converged) "not_converged" else "inadmissible"
        next
      }
      if (isTRUE(.h1_materially_below(draw0$loglik, draw1$loglik))) {
        why <- "h1_below_h0"
        if (retry < as.integer(max_retries)) {
          embed_next <- .split_null_state(fit0, fit1$result$K)
        }
        next
      }
      lrs <- c(lrs, 2 * (draw1$loglik - draw0$loglik))
      lr_draws <- c(lr_draws, draw)
      assessed <- h1_count >= 2L
      if (!draw0$replicated || (assessed && !draw1$replicated)) out$n_not_replicated <- out$n_not_replicated + 1L
      ok <- TRUE
      break
    }
    if (!ok) {
      out$n_failed <- out$n_failed + 1L
      out$failures <- rbind(out$failures, data.frame(draw = draw, reason = why, stringsAsFactors = FALSE))
      if (identical(why, "not_converged")) out$n_failed_not_converged <- out$n_failed_not_converged + 1L
      else if (identical(why, "inadmissible")) out$n_failed_inadmissible <- out$n_failed_inadmissible + 1L
      else out$n_failed_error <- out$n_failed_error + 1L
    }
  }
  }
  out$n_ok <- length(lrs)
  out$lrs <- lrs
  out$lr_draws <- lr_draws
  out$b <- if (length(lrs)) sum(.blrt_exceeds(lrs, out$lr_observed)) else 0L
  telemetry$seconds <- proc.time()[["elapsed"]] - started
  failure_text <- if (!nrow(out$failures)) {
    "Unresolved failures: none."
  } else {
    paste0(
      "Unresolved failures: ",
      paste(sprintf("draw %d %s", out$failures$draw, out$failures$reason), collapse = "; "),
      "."
    )
  }
  warn <- out$n_not_replicated > 0L || !sol0$replicated ||
    (isTRUE(out$observed_h1_replication_assessed) && !sol1$replicated)
  summary <- .blrt_summary(
    out$B, out$b, out$n_ok, out$n_failed, out$n_not_replicated,
    out$n_failed_not_converged, out$n_failed_error, telemetry$seconds, warn, failure_text
  )
  out$p <- summary$p
  out$p_plus <- summary$p_plus
  out$mc_se <- summary$mc_se
  out$p_ci_low <- summary$p_ci_low
  out$p_ci_high <- summary$p_ci_high
  out$reason <- summary$reason
  out$p_provisional <- isTRUE(out$n_failed > 0L)
  if (isTRUE(out$p_provisional)) {
    out$reason <- paste0(out$reason, " p = b/n_ok is provisional because n_failed > 0.")
  }
  out <- .stamp_experimental_blrt(out)
  message(out$reason)
  .publish_blrt(out, telemetry)
}

.rows_to_frame <- function(rows) {
  take_num <- function(name) vapply(rows, function(row) {
    value <- row[[name]]
    if (is.null(value) || length(value) == 0L || is.na(value)) NA_real_ else as.numeric(value)
  }, numeric(1))
  take_int <- function(name) vapply(rows, function(row) as.integer(row[[name]]), integer(1))
  take_lgl <- function(name) vapply(rows, function(row) isTRUE(row[[name]]), logical(1))
  cov <- vapply(rows, function(row) {
    key <- .cov_key(row$covariance)
    if (nzchar(key)) key else NA_character_
  }, character(1))
  tab <- tibble::tibble(
    model_id = take_int("model_id"),
    model = vapply(rows, function(row) row$model, character(1)),
    K = take_int("K"),
    covariance = cov,
    n_parameters = take_int("n_parameters"),
    n_waves = take_int("n_waves"),
    n_analyzed = take_int("n_analyzed"),
    loglik = take_num("loglik")
  )
  for (name in .index_names) tab[[name]] <- take_num(name)
  for (name in .index_names) tab[[paste0("delta_", name)]] <- take_num(paste0("delta_", name))
  tab$bic_bf_vs_best <- take_num("bic_bf_vs_best")
  tab$bic_cmp <- take_num("bic_cmp")
  tab$min_class_prop <- take_num("min_class_prop")
  tab$min_class_n <- take_num("min_class_n")
  tab$min_avepp <- take_num("min_avepp")
  tab$entropy_wave <- take_num("entropy_wave")
  tab$entropy_sequence <- take_num("entropy_sequence")
  tab$entropy_raw <- take_num("entropy_raw")
  tab$converged <- take_lgl("converged")
  tab$finite <- take_lgl("finite")
  tab$regularized <- take_lgl("regularized")
  tab$positive_definite <- take_lgl("positive_definite")
  tab$collapsed <- take_lgl("collapsed")
  tab$n_starts <- take_int("n_starts")
  tab$n_best <- take_int("n_best")
  tab$replicate_tol <- take_num("replicate_tol")
  tab$second_seed_loglik <- take_num("second_seed_loglik")
  tab$second_seed_n_best <- take_num("second_seed_n_best")
  tab$second_seed_abs_gap <- take_num("second_seed_abs_gap")
  tab$second_seed_status <- vapply(rows, function(row) {
    value <- row$second_seed_status
    if (is.null(value) || length(value) == 0L || is.na(value)) NA_character_ else as.character(value)
  }, character(1))
  tab$hard_pass <- take_lgl("hard_pass")
  tab$soft_pass <- take_lgl("soft_pass")
  tab$eligible <- take_lgl("eligible")
  tab$in_tie_zone <- take_lgl("in_tie_zone")
  tab$rejection_reason <- vapply(rows, function(row) row$rejection_reason %||% "", character(1))
  tab$selected <- take_lgl("selected")
  tab[.candidate_columns]
}

.panels_comparable <- function(fits) {
  panels <- lapply(fits, function(fit) fit$panel)
  if (all(vapply(panels, is.null, logical(1)))) return(TRUE)
  if (any(vapply(panels, is.null, logical(1)))) return(FALSE)
  base <- panels[[1]]
  for (panel in panels[-1]) {
    if (!identical(as.character(base$indicators), as.character(panel$indicators))) return(FALSE)
    if (!identical(as.character(base$family), as.character(panel$family))) return(FALSE)
    if (!identical(as.character(base$ids), as.character(panel$ids))) return(FALSE)
    if (!identical(as.character(base$waves), as.character(panel$waves))) return(FALSE)
    if (!identical(base$gauss, panel$gauss)) return(FALSE)
    if (!identical(base$cat, panel$cat)) return(FALSE)
    if (!identical(base$ncat, panel$ncat)) return(FALSE)
    if (!identical(base$level_order, panel$level_order)) return(FALSE)
  }
  TRUE
}

.require_blrt_pair <- function(h0, h1) {
  if (!.panels_comparable(list(h0, h1))) {
    stop("BLRT requires the same observed data, indicator encodings, wave schedule, and subjects", call. = FALSE)
  }
  if (as.integer(h0$result$K) >= as.integer(h1$result$K)) {
    stop("BLRT requires a nested alternative with more states than the null", call. = FALSE)
  }
  cov0 <- h0$result$covariance %||% ""
  cov1 <- h1$result$covariance %||% ""
  if (!identical(as.character(cov0), as.character(cov1))) {
    stop("BLRT requires the same covariance structure on the null and the alternative", call. = FALSE)
  }
  for (key in c("measurement_invariant", "means_invariant", "covariance_invariant", "transition_equal")) {
    left <- h0$spec[[key]]
    right <- h1$spec[[key]]
    if (is.null(left)) left <- TRUE
    if (is.null(right)) right <- TRUE
    if (!identical(isTRUE(left), isTRUE(right))) {
      stop("BLRT requires the same ", key, " setting on the null and the alternative", call. = FALSE)
    }
  }
}

#' Choose K and covariance jointly.
#'
#' Hard filters are convergence, a finite likelihood, an admissible covariance,
#' and replication of the highest admissible likelihood. A streaming fit that
#' omitted posteriors is excluded from automatic ranking: it is not eligible,
#' because `hard:posteriors_not_computed` is recorded and the information
#' criteria that need a posterior are missing.
#' @export
select_model <- function(fits, rule = selection_rule(), auto = TRUE,
                         second_seed_loglik = NULL, second_seed_n_best = NULL, waves = "auto") {
  if (inherits(fits, "profylr_wave_enumeration")) {
    stop("per-wave enumeration does not choose the joint number of states. Pass the joint fits to select_model.",
         call. = FALSE)
  }
  if (inherits(fits, "profylr_grid")) fits <- fits$fits
  if (inherits(fits, "profylr_fit")) fits <- list(fits)
  if (!length(fits)) stop("select_model needs at least one fit", call. = FALSE)
  if (isTRUE(auto) && !.panels_comparable(fits)) {
    stop("fits are not comparable: automatic ranking needs the same observations and response schema", call. = FALSE)
  }
  if (is.null(rule)) rule <- selection_rule()
  supplied <- !is.null(second_seed_loglik)
  if (supplied && length(second_seed_loglik) != length(fits)) {
    stop("second_seed_loglik must have one value per fit", call. = FALSE)
  }
  if (!is.null(second_seed_n_best) && length(second_seed_n_best) != length(fits)) {
    stop("second_seed_n_best must have one value per fit", call. = FALSE)
  }
  rows <- vector("list", length(fits))
  for (i in seq_along(fits)) {
    fit <- fits[[i]]
    result <- fit$result
    ic <- information_criteria(fit)
    diag <- classification_diagnostics(fit)
    cov <- result$covariance
    finite <- .finite1(result$loglik)
    categorical <- !nzchar(.cov_key(cov))
    pd_flag <- result$cov_positive_definite
    if (is.null(pd_flag)) pd_flag <- categorical
    row <- list(
      model_id = i, model = fit$model, K = as.integer(result$K),
      covariance = if (categorical) NA_character_ else as.character(cov),
      n_parameters = as.integer(result$n_parameters %||% 0L),
      n_waves = as.integer(result$n_waves %||% 1L),
      n_analyzed = as.integer(result$n_analyzed %||% 0L),
      loglik = if (finite) as.numeric(result$loglik) else NA_real_,
      min_class_prop = diag$min_class_prop, min_class_n = diag$min_class_n, min_avepp = diag$min_avepp,
      entropy_wave = result$entropy_wave %||% NA_real_,
      entropy_sequence = result$entropy_sequence %||% NA_real_,
      entropy_raw = ic$entropy_raw,
      converged = isTRUE(result$converged), finite = finite,
      regularized = isTRUE(result$regularized),
      positive_definite = isTRUE(pd_flag),
      collapsed = isTRUE(result$cov_collapsed_component),
      n_starts = as.integer(result$n_starts %||% 0L),
      n_best = as.integer(result$n_best %||% 0L),
      replicate_tol = as.numeric(fit$spec$replicate_tol %||% 0.1),
      fit = fit
    )
    for (name in .index_names) row[[name]] <- ic[[name]]
    rows[[i]] <- row
  }
  primary <- .primary_name(rule, rows)
  for (i in seq_along(rows)) {
    tokens <- character()
    row <- rows[[i]]
    if (!row$converged) tokens <- c(tokens, "hard:not_converged")
    if (!row$finite) tokens <- c(tokens, "hard:non_finite")
    if (row$regularized) tokens <- c(tokens, "hard:regularized")
    if (!row$positive_definite) tokens <- c(tokens, "hard:not_positive_definite")
    if (row$collapsed) tokens <- c(tokens, "hard:collapsed")
    if (row$n_starts < 2L) tokens <- c(tokens, "hard:replication_not_assessed")
    else if (row$n_best < 2L) tokens <- c(tokens, "hard:best_loglik_not_replicated")
    if (.higher_admissible_hidden(row$fit$result)) tokens <- c(tokens, "hard:higher_admissible_not_replicated")
    if (!.finite1(row[[primary]])) tokens <- c(tokens, "hard:information_criterion_undefined")
    if (isFALSE(row$fit$result$posteriors_computed)) tokens <- c(tokens, "hard:posteriors_not_computed")
    rows[[i]]$preseed <- tokens
  }
  for (i in seq_along(rows)) {
    rows[[i]]$second_seed_loglik <- NA_real_
    rows[[i]]$second_seed_n_best <- NA_real_
    rows[[i]]$second_seed_abs_gap <- NA_real_
    rows[[i]]$second_seed_status <- NA_character_
    if (!isTRUE(rule$require_second_seed) || !isTRUE(auto)) next
    if (any(rows[[i]]$preseed %in% .preseed_tokens)) next
    if (supplied) {
      ll2 <- as.numeric(second_seed_loglik[[i]])
      if (is.null(second_seed_n_best)) {
        rows[[i]]$second_seed_loglik <- ll2
        rows[[i]]$second_seed_status <- "not assessed"
        rows[[i]]$preseed <- c(rows[[i]]$preseed, "hard:second_seed_not_assessed")
        next
      }
      rows[[i]]$second_seed_status <- "assessed"
      nbest <- as.integer(second_seed_n_best[[i]])
    } else if (is.null(rows[[i]]$fit$panel)) {
      ll2 <- NA_real_
      nbest <- 0L
    } else {
      refit <- .refit_second_seed(rows[[i]]$fit, rule$second_seed_offset)
      ll2 <- refit$loglik
      nbest <- refit$n_best
    }
    rows[[i]]$second_seed_status <- "assessed"
    rows[[i]]$second_seed_loglik <- ll2
    rows[[i]]$second_seed_n_best <- nbest
    if (.finite1(ll2) && rows[[i]]$finite) rows[[i]]$second_seed_abs_gap <- abs(ll2 - rows[[i]]$loglik)
    if (!.finite1(ll2) || (rows[[i]]$finite && !isTRUE(logliks_replicate(
        ll2, rows[[i]]$loglik, rows[[i]]$replicate_tol)))) {
      rows[[i]]$preseed <- c(rows[[i]]$preseed, "hard:second_seed_not_reproduced")
    }
    if (nbest < 2L) rows[[i]]$preseed <- c(rows[[i]]$preseed, "hard:second_seed_not_replicated")
  }
  for (i in seq_along(rows)) {
    tokens <- rows[[i]]$preseed
    if (.soft_fail(rows[[i]]$min_class_prop, rule$min_class_prop)) tokens <- c(tokens, "soft:min_class_prop")
    if (.soft_fail(rows[[i]]$min_class_n, rule$min_class_n)) tokens <- c(tokens, "soft:min_class_n")
    if (.soft_fail(rows[[i]]$min_avepp, rule$min_avepp)) tokens <- c(tokens, "soft:min_avepp")
    if (isTRUE(rule$entropy_filter) && .soft_fail(rows[[i]]$entropy_wave, rule$min_entropy)) {
      tokens <- c(tokens, "soft:entropy")
    }
    hard <- tokens[startsWith(tokens, "hard:")]
    soft <- tokens[startsWith(tokens, "soft:")]
    rows[[i]]$hard_pass <- !length(hard)
    rows[[i]]$soft_pass <- !length(soft)
    rows[[i]]$eligible <- !length(tokens)
    rows[[i]]$rejection_reason <- .join_tokens(tokens)
    rows[[i]]$selected <- FALSE
    rows[[i]]$in_tie_zone <- FALSE
  }
  rows <- .relative_indices(rows)
  disagreement <- .disagreement_table(rows)
  warnings <- character()
  wave_table <- NULL
  joint <- any(vapply(rows, function(row) identical(row$model, "lta") && row$n_waves >= 2L, logical(1)))
  if (isTRUE(auto) && identical(waves, "auto") && joint) {
    summary <- .wave_summary(lapply(rows, function(row) row$fit), rule)
    wave_table <- summary$table
    if (identical(summary$note, "configural_noninvariance_possible")) warnings <- c(warnings, summary$note)
  }
  blrt <- list(
    ran = FALSE, validated = FALSE, switched = FALSE, ambiguous_kind = "",
    reason = paste0(
      "BLRT is off by default. The procedure follows Mplus TECH14 ",
      "(p=b/n_ok, and a draw whose best log-likelihood did not replicate is kept). ",
      "Independent bootstrap p-values are not required to match. ",
      "validated stays false until Monte Carlo agreement, shared-dataset likelihoods, and operating characteristics are shown. ",
      "An unreplicated kept solution is a diagnostic and is not a validation failure."
    ),
    p = NA_real_, B = rule$blrt_B, seconds = NA_real_, starts_h0 = NA_integer_, starts_h1 = NA_integer_,
    cost_note = ""
  )
  chosen <- NULL
  ambiguous <- FALSE
  if (isTRUE(auto)) {
    decision_bits <- .choose_rows(rows, primary, rule$tie_zone)
    chosen <- decision_bits$chosen
    ambiguous <- decision_bits$ambiguous
    tie <- decision_bits$tie
    tie_ids <- vapply(tie, function(row) row$model_id, integer(1))
    for (i in seq_along(rows)) rows[[i]]$in_tie_zone <- rows[[i]]$model_id %in% tie_ids
    blrt$ambiguous_kind <- decision_bits$kind
    if (!is.null(chosen) && isTRUE(rule$blrt) && (isTRUE(ambiguous) || isTRUE(rule$blrt_check)) &&
        !isTRUE(rule$experimental_blrt)) {
      blrt$ran <- FALSE
      blrt$validated <- FALSE
      blrt$switched <- FALSE
      blrt$decision <- "not_run"
      blrt$reason <- paste0(
        "not run: experimental BLRT is excluded from automatic selection. ",
        "blrt validated=false. Pass experimental_blrt=TRUE for a report-only result. ",
        "That result does not change the selected model."
      )
    } else if (!is.null(chosen) && isTRUE(rule$blrt) && (isTRUE(ambiguous) || isTRUE(rule$blrt_check))) {
      rival <- NULL
      if (isTRUE(ambiguous)) {
        bigger <- Filter(function(row) row$n_parameters > chosen$n_parameters, tie)
        if (length(bigger)) rival <- .sort_key_order(bigger, primary)[[1]]
      } else if (isTRUE(rule$blrt_check)) {
        others <- Filter(function(row) isTRUE(row$eligible) && row$model_id != chosen$model_id, rows)
        if (length(others)) rival <- .sort_key_order(others, primary)[[1]]
      }
      if (is.null(rival)) {
        blrt$reason <- "BLRT was not run because the tie zone has no second eligible cell."
      } else if (is.null(chosen$fit$panel) || is.null(rival$fit$panel)) {
        blrt$reason <- "BLRT was not run because the candidate fits have no panel to simulate from."
      } else {
        pair <- if (chosen$n_parameters <= rival$n_parameters) list(chosen, rival) else list(rival, chosen)
        ran <- blrt_check(pair[[1]]$fit, pair[[2]]$fit, B = rule$blrt_B, max_retries = rule$blrt_max_retries,
                          alpha = rule$blrt_alpha, seed = pair[[1]]$fit$spec$seed %||% 1,
                          max_iter = rule$blrt_max_iter, experimental = TRUE)
        ran$ambiguous_kind <- decision_bits$kind
        ran$switched <- FALSE
        ran$report_only <- !isTRUE(ran$validated)
        if (!isTRUE(ran$validated)) {
          ran$reason <- paste0(ran$reason, " Unvalidated BLRT is report-only and does not change the selected model.")
        }
        blrt <- ran
      }
    } else if (isTRUE(rule$blrt) && !isTRUE(ambiguous) && !isTRUE(rule$blrt_check)) {
      blrt$reason <- "BLRT runs only in the tie zone or when blrt_check is TRUE."
    }
    if (!is.null(chosen)) {
      for (i in seq_along(rows)) rows[[i]]$selected <- rows[[i]]$model_id == chosen$model_id
    }
  }
  decision <- if (!isTRUE(auto)) "auto_disabled" else if (!is.null(chosen)) "selected" else "no_selection"
  reason <- .selection_reason(decision, chosen, primary, rule, ambiguous, warnings, blrt)
  grid <- paste(vapply(rows, function(row) {
    paste0(row$model, " K=", row$K, " ", .cov_label(row$covariance))
  }, character(1)), collapse = "; ")
  reason <- paste0(reason, " Search grid: ", grid, ".")
  public <- lapply(rows, function(row) {
    row$fit <- NULL
    row$preseed <- NULL
    row
  })
  structure(list(
    decision = decision,
    ambiguous = if (isTRUE(auto)) isTRUE(ambiguous) else FALSE,
    reason = reason,
    selected_model_id = if (is.null(chosen) || !isTRUE(auto)) NA_integer_ else as.integer(chosen$model_id),
    automatic_model_id = if (is.null(chosen) || !isTRUE(auto)) NA_integer_ else as.integer(chosen$model_id),
    automatic_decision = if (!isTRUE(auto)) "auto_disabled" else if (!is.null(chosen)) "selected" else "no_selection",
    accepted = NULL,
    accepted_model_id = NA_integer_,
    selected_k = if (is.null(chosen) || !isTRUE(auto)) NA_integer_ else as.integer(chosen$K),
    primary = primary,
    rule = rule,
    candidates = .rows_to_frame(public),
    disagreement = disagreement,
    warnings = warnings,
    blrt = blrt,
    wave_enumeration = wave_table,
    fits = fits,
    comparison = compare_models(fits)
  ), class = "profylr_model_selection")
}

#' @export
accept <- function(selection, model_id) {
  if (!inherits(selection, "profylr_model_selection")) {
    stop("accept() records the analyst's choice on a select_model result", call. = FALSE)
  }
  model_id <- as.integer(model_id)
  if (!model_id %in% selection$candidates$model_id) {
    stop("model_id is not one of the tested candidates", call. = FALSE)
  }
  selection$accepted_model_id <- model_id
  selection$accepted <- TRUE
  selection
}

.plot_candidates <- function(x) {
  if (inherits(x, "profylr_model_selection")) return(x$candidates)
  if (is.data.frame(x)) return(x)
  stop("expected a model selection or a candidate table", call. = FALSE)
}

#' @export
plot_ic_elbow <- function(selection) {
  frame <- .plot_candidates(selection)
  pieces <- lapply(c("aic", "bic", "sabic"), function(stat) {
    tibble::tibble(
      K = frame$K,
      covariance = vapply(frame$covariance, .cov_label, character(1)),
      criterion = toupper(stat),
      value = frame[[stat]],
      hard_pass = frame$hard_pass,
      selected = frame$selected
    )
  })
  long <- do.call(rbind, pieces)
  long <- long[is.finite(long$value), , drop = FALSE]
  if (!nrow(long)) stop("no supported information criteria to plot", call. = FALSE)
  inadmissible <- long[!long$hard_pass, , drop = FALSE]
  admissible <- long[long$hard_pass, , drop = FALSE]
  picked <- long[long$selected, , drop = FALSE]
  plot <- ggplot2::ggplot(long, ggplot2::aes(K, value, color = criterion, group = criterion)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~covariance) +
    ggplot2::labs(
      title = "Information criteria by covariance",
      y = "Inadmissible cells are crossed and the selected cell is ringed",
      x = "States", color = NULL
    )
  if (nrow(admissible)) plot <- plot + ggplot2::geom_point(data = admissible, size = 2.2)
  if (nrow(inadmissible)) {
    plot <- plot + ggplot2::geom_point(data = inadmissible, shape = 4, size = 2.6, color = "#9b2226")
  }
  if (nrow(picked)) {
    plot <- plot + ggplot2::geom_point(data = picked, shape = 21, size = 5, stroke = 1.2, fill = NA, color = "black")
  }
  plot
}

.max_posterior_values <- function(fit) {
  result <- if (inherits(fit, "profylr_fit")) fit$result else fit
  if (isFALSE(result$posteriors_computed)) {
    stop("posteriors were not computed; refit with posteriors=TRUE", call. = FALSE)
  }
  gamma <- result$gamma
  K <- as.integer(result$K)
  waves <- as.integer(result$n_waves %||% 1L)
  subjects <- as.integer(result$n_subjects %||% 0L)
  if (is.null(gamma) || length(gamma) != subjects * waves * K) stop("this fit has no posterior to plot", call. = FALSE)
  arr <- array(as.numeric(gamma), dim = c(K, waves, subjects))
  values <- numeric()
  for (i in seq_len(subjects)) for (t in seq_len(waves)) {
    post <- arr[, t, i]
    if (all(is.finite(post))) values <- c(values, max(post))
  }
  values
}

#' @export
plot_avepp <- function(fit) {
  result <- if (inherits(fit, "profylr_fit")) fit$result else fit
  if (isFALSE(result$posteriors_computed)) {
    stop("posteriors were not computed; refit with posteriors=TRUE", call. = FALSE)
  }
  diag <- classification_diagnostics(fit)
  matrix_out <- diag$avepp_matrix
  K <- nrow(matrix_out)
  rows <- list()
  for (modal in seq_len(K)) for (class in seq_len(K)) {
    rows[[length(rows) + 1L]] <- data.frame(
      modal = modal, class = class, value = matrix_out[modal, class], stringsAsFactors = FALSE
    )
  }
  frame <- do.call(rbind, rows)
  ggplot2::ggplot(frame, ggplot2::aes(factor(class), factor(modal), fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(is.finite(value), sprintf("%.2f", value), ""))) +
    ggplot2::scale_fill_gradient(limits = c(0, 1), low = "white", high = "#1f4e79", na.value = "grey90") +
    ggplot2::labs(x = "Posterior class", y = "Modal class", fill = NULL, title = "Average posterior by modal class")
}

#' @export
plot_max_posterior <- function(fit) {
  values <- .max_posterior_values(fit)
  frame <- data.frame(maximum = values)
  ggplot2::ggplot(frame, ggplot2::aes(maximum)) +
    ggplot2::geom_histogram(bins = 15, fill = "#1f4e79", color = "white") +
    ggplot2::labs(x = "Maximum posterior", y = "Person-waves", title = "Assignment certainty")
}

#' Class proportion by wave.
#'
#' @param fit A profylr fit.
#' @param kind \code{posterior} (expected membership), \code{hard}, or
#'   \code{model}. Posterior and hard draws require a computed posterior.
#'   Model-implied prevalence does not.
#' @export
plot_prevalence <- function(fit, kind = c("posterior", "model", "hard")) {
  kind <- match.arg(kind)
  result <- fit$result
  if (!identical(kind, "model")) {
    if (isFALSE(result$posteriors_computed)) {
      stop("posteriors were not computed; refit with posteriors=TRUE", call. = FALSE)
    }
  }
  K <- as.integer(result$K)
  waves <- as.integer(result$n_waves)
  values <- switch(kind,
    model = result$prevalence_model,
    hard = result$prevalence_hard,
    result$expected_membership / result$n_analyzed
  )
  prop <- matrix(values, nrow = waves, byrow = TRUE)
  labels <- if (!is.null(fit$panel$waves) && length(fit$panel$waves) == waves) fit$panel$waves else seq_len(waves)
  rows <- list()
  for (t in seq_len(waves)) for (state in seq_len(K)) {
    rows[[length(rows) + 1L]] <- data.frame(
      wave = as.character(labels[[t]]), state = paste("Class", state), prevalence = prop[t, state],
      stringsAsFactors = FALSE
    )
  }
  frame <- do.call(rbind, rows)
  ggplot2::ggplot(frame, ggplot2::aes(wave, prevalence, fill = state)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(x = "Wave", y = "Expected proportion", fill = NULL, title = "Class prevalence")
}

#' @export
plot_index_disagreement <- function(selection) {
  if (!inherits(selection, "profylr_model_selection")) {
    stop("plot_index_disagreement expects a model selection", call. = FALSE)
  }
  frame <- selection$disagreement
  frame <- frame[is.finite(frame$K), , drop = FALSE]
  if (!nrow(frame)) stop("no index chose a candidate", call. = FALSE)
  frame$covariance <- vapply(frame$covariance, .cov_label, character(1))
  plot <- ggplot2::ggplot(frame, ggplot2::aes(K, index)) +
    ggplot2::geom_point(size = 3, color = "#1f4e79") +
    ggplot2::geom_text(ggplot2::aes(label = covariance), nudge_x = 0.08, hjust = 0, size = 3) +
    ggplot2::labs(x = "States chosen by that index", y = NULL,
                  title = "Index disagreement. This is not a vote.")
  if (identical(selection$decision, "selected")) {
    plot <- plot + ggplot2::geom_vline(xintercept = selection$selected_k, linetype = "dashed", color = "#9b2226")
  }
  plot
}
