#' Latent profiles, classes, and transitions
#'
#' Release 1 fits indicators only. Covariates are a separate role and are
#' refused. Distal outcomes are Release 3 and need a declared timing.
#' Ordinal indicators are Release 2. Count families come later.
#'
#' @name profylr
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  "K", "value", "covariance", "criterion", "reliable", "index", "maximum",
  "n", "pattern", "wave", "prevalence", "variable", "estimate", "state",
  "category", "to"
))

#' @export
build_info <- function() {
  info <- cpp_build_info()
  info$package <- "profylr"
  info$version <- as.character(utils::packageVersion("profylr"))
  info$engine <- "cpp-em"
  info
}

.pattern_mode <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !(value %in% c("none", "compress"))) {
    stop("patterns must be \"none\" or \"compress\"", call. = FALSE)
  }
  value
}

.estep_thread_count <- function(value) {
  # 1024 matches kMaxEstepThreads and is below .Machine$integer.max, so
  # as.integer cannot turn an accepted value into NA_integer_.
  if (is.logical(value) || length(value) != 1L || !is.numeric(value) ||
      is.na(value) || !is.finite(value) || value != trunc(value) ||
      value < 1 || value > 1024) {
    stop("estep_threads must be a finite whole number from 1 to 1024", call. = FALSE)
  }
  as.integer(value)
}

#' Latent transition specification.
#'
#' @param n_states Number of latent states.
#' @param covariance Covariance structure. \code{NULL} is a latent class model.
#' @param measurement_invariant,means_invariant,covariance_invariant
#'   Hold the named block equal across waves.
#' @param transitions \code{equal} or \code{free} transition matrices.
#' @param n_starts,screen_iter,n_full Random-start schedule.
#' @param max_iter,tol EM iteration cap and relative convergence tolerance.
#' @param replicate_tol Absolute log-likelihood gap that still counts as one solution.
#' @param n_threads,seed,align_labels Worker count, seed, and label alignment.
#' @param covariates Refused in Release 1.
#' @param start,append_start Optional supplied start.
#' @param estep \code{stored} or \code{streaming}.
#' @param posteriors Return posterior state probabilities.
#' @param estep_threads Threads inside one E-step. A start or bootstrap worker forces this to 1.
#' @param patterns \code{none} or \code{compress}.
#' @export
lta_spec <- function(n_states, covariance = "VVV", measurement_invariant = TRUE,
                     means_invariant = TRUE, covariance_invariant = TRUE,
                     transitions = c("equal", "free"), n_starts = 40L,
                     screen_iter = 10L, n_full = 4L, max_iter = 200L,
                     tol = 1e-7, replicate_tol = 0.1, n_threads = 1L,
                     seed = 1, align_labels = TRUE, covariates = NULL, start = NULL,
                     append_start = FALSE, estep = c("stored", "streaming"),
                     posteriors = TRUE, estep_threads = 1L,
                     patterns = "none") {
  transitions <- match.arg(transitions)
  estep <- match.arg(estep)
  if (!is.null(covariates) && length(covariates)) {
    stop("Covariates are not estimated in Release 1. They are a separate role ",
         "from indicators and were not dropped into the measurement model.",
         call. = FALSE)
  }
  spec <- list(
    K = as.integer(n_states),
    covariance = if (is.null(covariance)) NULL else cpp_parse_covariance(covariance),
    measurement_invariant = isTRUE(measurement_invariant),
    means_invariant = isTRUE(means_invariant),
    covariance_invariant = isTRUE(covariance_invariant),
    transition_equal = identical(transitions, "equal"),
    n_starts = as.integer(n_starts),
    screen_iter = as.integer(screen_iter),
    n_full = as.integer(n_full),
    max_iter = as.integer(max_iter),
    tol = as.numeric(tol),
    replicate_tol = as.numeric(replicate_tol),
    n_threads = as.integer(n_threads),
    seed = as.numeric(seed),
    align_labels = isTRUE(align_labels)
  )
  # Absent keys keep the historical spec: stored E-step, posteriors returned.
  if (!identical(estep, "stored")) spec$estep <- estep
  if (!isTRUE(posteriors)) spec$posteriors <- FALSE
  threads <- .estep_thread_count(estep_threads)
  if (!identical(threads, 1L)) spec$estep_threads <- threads
  mode <- .pattern_mode(patterns)
  if (!identical(mode, "none")) spec$patterns <- mode
  if (!is.null(start)) {
    spec$start <- start
    if (isTRUE(append_start)) {
      spec$append_supplied_start <- TRUE
    } else {
      spec$has_start <- TRUE
      spec$n_starts <- 1L
    }
  }
  spec
}

#' Absolute replication test
#'
#' Two log-likelihoods are the same solution when
#' \code{abs(ll - best) <= replicate_tol}. The default \code{0.1} is
#' chosen to be comparable to the approximately 0.1 tolerance Muthén
#' described on the Mplus discussion board (2018); Mplus does not document
#' an exact value. It does not scale with N or with the log-likelihood.
#' A caller can set \code{replicate_tol}.
#'
#' @param ll Log-likelihood to test.
#' @param best Reference log-likelihood, usually the best one.
#' @param replicate_tol Absolute gap. Default \code{0.1}.
#' @export
logliks_replicate <- function(ll, best, replicate_tol = 0.1) {
  cpp_logliks_replicate(as.numeric(ll), as.numeric(best), as.numeric(replicate_tol))
}

#' @export
lpa_spec <- function(n_states, covariance = "VVV", ...) {
  lta_spec(n_states, covariance = covariance, ...)
}

#' @export
lca_spec <- function(n_states, ...) {
  lta_spec(n_states, covariance = NULL, ...)
}

.family_of <- function(x, declared) {
  if (!is.null(declared)) return(cpp_parse_family(declared))
  x <- x[!is.na(x)]
  if (is.numeric(x) && (length(unique(x)) > 2 || !all(unique(x) %in% c(0, 1)))) "gaussian" else if (length(unique(x)) <= 2) "bernoulli" else "nominal"
}

#' @export
.integer_waves <- function(x, what) {
  if (!length(x)) stop(what, " is empty", call. = FALSE)
  if (is.numeric(x)) {
    if (any(!is.finite(x)) || any(abs(x - round(x)) > 1e-8)) {
      stop(what, " must be integer wave codes; fractional schedules are rejected", call. = FALSE)
    }
    return(as.integer(round(x)))
  }
  x
}

prepare_panel <- function(data, indicators, id, wave = NULL, family = NULL, schedule = NULL, schema = NULL) {
  if (!length(indicators)) stop("name at least one indicator", call. = FALSE)
  data <- as.data.frame(data)
  if (!id %in% names(data)) stop("missing key column ", id, call. = FALSE)
  if (is.null(wave)) {
    data[["._wave"]] <- 1L
    wave <- "._wave"
  }
  if (!wave %in% names(data)) stop("missing key column ", wave, call. = FALSE)
  if (anyNA(data[[id]]) || anyNA(data[[wave]])) stop("missing key in id or wave", call. = FALSE)
  key <- paste(as.character(data[[id]]), as.character(data[[wave]]), sep = "\r")
  if (anyDuplicated(key)) stop("duplicate subject-wave key", call. = FALSE)
  ids <- unique(data[[id]])
  observed <- unique(data[[wave]])
  if (!is.null(schema)) {
    if (!identical(as.character(indicators), as.character(schema$indicators))) {
      stop("prediction indicators do not match the training schema", call. = FALSE)
    }
    waves <- schema$waves
  } else if (!is.null(schedule)) {
    waves <- .integer_waves(schedule, "schedule")
    if (anyDuplicated(waves)) stop("duplicate schedule entry", call. = FALSE)
  } else if (is.numeric(observed)) {
    observed <- .integer_waves(observed, "wave")
    waves <- seq(min(observed), max(observed))
  } else {
    # First appearance. Python uses the same rule. Lexical order is not a wave order.
    waves <- observed
  }
  families <- character(length(indicators))
  if (!is.null(schema)) {
    families <- as.character(schema$family)
  } else {
    for (j in seq_along(indicators)) {
      declared <- NULL
      if (is.list(family)) declared <- family[[indicators[[j]]]]
      if (is.character(family) && length(family) == length(indicators)) declared <- family[[j]]
      families[[j]] <- .family_of(data[[indicators[[j]]]], declared)
    }
  }
  measurement <- if (all(families == "gaussian")) "gaussian" else if (all(families %in% c("bernoulli", "nominal"))) "categorical" else "mixed"
  N <- length(ids)
  T <- length(waves)
  J <- length(indicators)
  id_pos <- stats::setNames(seq_along(ids), as.character(ids))
  wave_pos <- stats::setNames(seq_along(waves), as.character(waves))
  gauss <- rep(NA_real_, N * T * J)
  cat <- rep(-1L, N * T * J)
  ncat <- integer(J)
  levels <- vector("list", J)
  level_order <- vector("list", J)
  for (j in seq_along(indicators)) {
    if (families[[j]] == "gaussian") next
    if (!is.null(schema) && !is.null(schema$level_order) && !is.null(schema$level_order[[j]])) {
      order <- schema$level_order[[j]]
      ncat[[j]] <- as.integer(schema$ncat[[j]])
      levels[[j]] <- stats::setNames(seq_along(order) - 1L, as.character(order))
      level_order[[j]] <- order
      next
    }
    vals <- unique(data[[indicators[[j]]]][!is.na(data[[indicators[[j]]]])])
    vals <- sort(vals)
    if (families[[j]] == "bernoulli" && all(vals %in% c(0, 1))) {
      ncat[[j]] <- 2L
      levels[[j]] <- c(`0` = 0L, `1` = 1L)
      level_order[[j]] <- c(0L, 1L)
      next
    }
    if (families[[j]] == "bernoulli" && length(vals) != 2L) {
      stop(indicators[[j]], " was declared Bernoulli but does not have 2 observed categories.", call. = FALSE)
    }
    if (length(vals) < 2L) stop(indicators[[j]], " needs at least two observed categories.", call. = FALSE)
    ncat[[j]] <- length(vals)
    levels[[j]] <- stats::setNames(seq_along(vals) - 1L, as.character(vals))
    level_order[[j]] <- vals
  }
  ii <- id_pos[as.character(data[[id]])]
  tt <- wave_pos[as.character(data[[wave]])]
  if (anyNA(ii) || anyNA(tt)) stop("wave is not in the schedule", call. = FALSE)
  base <- ((as.integer(ii) - 1L) * T + (as.integer(tt) - 1L)) * J
  for (j in seq_along(indicators)) {
    value <- data[[indicators[[j]]]]
    ok <- !is.na(value)
    if (!any(ok)) next
    offset <- base[ok] + j
    if (families[[j]] == "gaussian") {
      gauss[offset] <- as.numeric(value[ok])
    } else {
      vals <- value[ok]
      keys <- character(length(vals))
      bern <- families[[j]] == "bernoulli"
      for (k in seq_along(vals)) {
        keys[[k]] <- if (bern && vals[[k]] %in% c(0, 1)) as.character(as.integer(vals[[k]])) else as.character(vals[[k]])
      }
      coded <- levels[[j]][keys]
      if (anyNA(coded)) stop("unseen category in ", indicators[[j]], call. = FALSE)
      cat[offset] <- as.integer(coded)
    }
  }
  list(
    N = N, T = T, J = J, measurement = measurement, family = families, ncat = ncat,
    Rmax = max(c(ncat, 1L)),
    cat = if (measurement == "gaussian") NULL else cat,
    gauss = if (measurement == "categorical") NULL else gauss,
    ids = ids, waves = waves, indicators = indicators, levels = levels, level_order = level_order
  )
}

.new_fit <- function(result, panel, spec, model) {
  structure(list(result = result, panel = panel, spec = spec, model = model,
                 engine = result$engine, draws = NULL, interval_type = result$interval_type),
            class = "profylr_fit")
}

.fit_model <- function(data, indicators, id, wave, spec, model) {
  panel <- prepare_panel(data, indicators, id, wave, family = spec$family_map)
  .new_fit(cpp_fit(panel, spec), panel, spec, model)
}

#' @export
fit_lta <- function(data, indicators, id, wave, n_states = 2L, family = NULL,
                    covariance = "VVV", ...) {
  spec <- lta_spec(n_states, covariance = covariance, ...)
  spec$family_map <- family
  .fit_model(data, indicators, id, wave, spec, "lta")
}

#' @export
fit_lpa <- function(data, indicators, id = NULL, n_states = 2L, covariance = "VVV",
                    family = NULL, ...) {
  data <- as.data.frame(data)
  if (is.null(id)) {
    data[["._id"]] <- seq_len(nrow(data))
    id <- "._id"
  }
  spec <- lpa_spec(n_states, covariance = covariance, ...)
  if (identical(spec$patterns, "compress")) {
    stop("patterns=\"compress\" is implemented for categorical models only. ",
         "Gaussian and mixed models stay on patterns=\"none\".", call. = FALSE)
  }
  spec$family_map <- family
  .fit_model(data, indicators, id, NULL, spec, "lpa")
}

#' @export
fit_lca <- function(data, indicators, id = NULL, wave = NULL, n_states = 2L,
                    family = NULL, ...) {
  data <- as.data.frame(data)
  if (is.null(id)) {
    data[["._id"]] <- seq_len(nrow(data))
    id <- "._id"
  }
  spec <- lca_spec(n_states, ...)
  spec$family_map <- family
  .fit_model(data, indicators, id, wave, spec, "lca")
}

#' @export
glance.profylr_fit <- function(x, ...) {
  r <- x$result
  tibble::tibble(
    model = x$model, status = r$status, status_reason = r$status_reason,
    engine = r$engine, loglik = r$loglik, aic = r$aic, bic = r$bic, sabic = r$sabic,
    n_parameters = r$n_parameters, n_analyzed = r$n_analyzed, n_excluded = r$n_excluded,
    n_states = r$K, n_waves = r$n_waves, entropy_wave = r$entropy_wave,
    entropy_sequence = r$entropy_sequence, converged = r$converged, n_best = r$n_best,
    n_starts = r$n_starts, ic_supported = r$ic_supported, interval_type = r$interval_type,
    parameter_uncertainty = r$parameter_uncertainty, state_uncertainty = r$state_uncertainty,
    has_draws = r$has_draws, covariance = r$covariance
  )
}

#' @export
tidy.profylr_fit <- function(x, ...) {
  r <- x$result
  p <- r$params
  K <- r$K
  rows <- list()
  add <- function(parameter, state, to = NA_integer_, variable = NA_character_,
                  category = NA_integer_, wave = NA_integer_, estimate) {
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      parameter = parameter, state = state, to = to, variable = variable,
      category = category, wave = wave, estimate = estimate
    )
  }
  for (k in seq_len(K)) add("initial", k, estimate = p$delta[[k]])
  if (length(p$tau)) {
    blocks <- length(p$tau) / (K * K)
    for (b in seq_len(blocks)) for (j in seq_len(K)) for (k in seq_len(K)) {
      add("transition", j, to = k, wave = if (r$transition_equal) NA_integer_ else b,
          estimate = p$tau[[(b - 1L) * K * K + (j - 1L) * K + k]])
    }
  }
  names <- x$panel$indicators
  if (length(p$rho)) {
    J <- x$panel$J
    R <- r$Rmax
    blocks <- length(p$rho) / (J * K * R)
    for (b in seq_len(blocks)) for (j in seq_len(J)) {
      if (identical(x$panel$family[[j]], "gaussian")) next
      for (k in seq_len(K)) for (cat in seq_len(x$panel$ncat[[j]])) {
        add("response", k, variable = names[[j]], category = cat - 1L,
            wave = if (r$measurement_invariant) NA_integer_ else b,
            estimate = p$rho[[(((b - 1L) * J + (j - 1L)) * K + (k - 1L)) * R + cat]])
      }
    }
  }
  if (length(p$means)) {
    Jg <- r$n_gaussian
    blocks <- length(p$means) / (K * Jg)
    for (b in seq_len(blocks)) for (k in seq_len(K)) for (g in seq_len(Jg)) {
      j <- r$gaussian_index[[g]] + 1L
      add("mean", k, variable = names[[j]],
          wave = if (r$means_invariant) NA_integer_ else b,
          estimate = p$means[[((b - 1L) * K + (k - 1L)) * Jg + g]])
    }
  }
  tibble::as_tibble(do.call(rbind, rows))
}

.require_posteriors <- function(result) {
  if (isFALSE(result$posteriors_computed)) {
    stop("posteriors were not computed; refit with posteriors=TRUE", call. = FALSE)
  }
}

#' @export
augment.profylr_fit <- function(x, ...) {
  r <- x$result
  .require_posteriors(r)
  N <- r$n_subjects
  T <- r$n_waves
  K <- r$K
  rows <- vector("list", N * T)
  pos <- 0L
  for (i in seq_len(N)) for (t in seq_len(T)) {
    pos <- pos + 1L
    base <- ((i - 1L) * T + (t - 1L)) * K
    rec <- tibble::tibble(
      id = x$panel$ids[[i]], wave = x$panel$waves[[t]],
      modal_state = r$modal[[(i - 1L) * T + t]] + 1L,
      viterbi_state = r$viterbi[[(i - 1L) * T + t]] + 1L
    )
    for (k in seq_len(K)) {
      rec[[paste0("smoothed_", k)]] <- r$gamma[[base + k]]
      rec[[paste0("filtered_", k)]] <- r$filtered[[base + k]]
    }
    rows[[pos]] <- rec
  }
  tibble::as_tibble(do.call(rbind, rows))
}

#' @export
estimate_distal <- function(name, family, timing = NULL, wave = NULL,
                            from_state = NULL, to_state = NULL, method = NULL) {
  cpp_estimate_distal(list(name = name, family = family, timing = timing, wave = wave,
                           from_state = from_state, to_state = to_state, method = method))
}

#' @export
decode_states <- function(fit, method = c("smoothed", "filtered", "viterbi")) {
  method <- match.arg(method)
  result <- fit$result
  # Stored and streaming-with-posteriors fits keep the historical augment
  # subset: a tibble, and an excluded subject's -1 becomes 0L.
  if (!isFALSE(result$posteriors_computed)) {
    aug <- augment(fit)
    if (identical(method, "viterbi")) return(aug[, c("id", "wave", "viterbi_state")])
    if (identical(method, "filtered")) return(aug[, !grepl("^smoothed_", names(aug))])
    return(aug)
  }
  if (!identical(method, "viterbi")) {
    stop("posteriors were not computed; refit with posteriors=TRUE", call. = FALSE)
  }
  width <- as.integer(result$n_waves)
  n <- as.integer(result$n_subjects)
  rows <- vector("list", n * width)
  pos <- 0L
  for (i in seq_len(n)) for (t in seq_len(width)) {
    pos <- pos + 1L
    rows[[pos]] <- tibble::tibble(
      id = fit$panel$ids[[i]], wave = fit$panel$waves[[t]],
      viterbi_state = result$viterbi[[(i - 1L) * width + t]] + 1L
    )
  }
  tibble::as_tibble(do.call(rbind, rows))
}

.comparison_columns <- c(
  "model_id", "role", "wave", "model", "K", "covariance",
  "measurement_invariant", "means_invariant", "covariance_invariant", "transitions",
  "loglik", "n_parameters", "bic", "sabic", "aic", "ic_supported", "ic_reason",
  "n_best", "n_starts", "n_converged_starts", "converged", "status",
  "min_expected_n", "min_expected_proportion", "small_state_flag", "small_state_rule",
  "min_expected_transitions", "transition_note",
  "entropy_wave", "entropy_sequence", "mean_max_posterior", "assignment_note",
  "blrt", "blrt_reason", "lmr", "lmr_reason",
  "n_boundary", "regularized", "min_eigenvalue", "boundary_warning", "regularization_warning",
  "reliable", "unreliable_reason", "inference_warning", "criterion_note", "selected", "selection_rationale"
)

# Fit-level failures make the fit unusable. Other tokens warn. An absolute
# eigenvalue is not a cutoff, and entropy is not read.
.block_tokens <- c(
  "not_converged", "numerical_failure", "best_loglik_not_replicated",
  "not_positive_definite"
)
.warn_tokens <- c(
  "boundary_parameters", "variance_floor", "replication_not_assessed",
  "ill_conditioned_covariance", "collapsed_component", "numerically_empty_state",
  "posteriors_not_computed"
)
.measurement_block <- c(
  "not_positive_definite", "collapsed_component", "numerically_empty_state",
  "posteriors_not_computed"
)
.comparison_block <- c(
  "variance_floor", "replication_not_assessed", "not_positive_definite",
  "collapsed_component", "numerically_empty_state", "posteriors_not_computed"
)
.note_only <- c("boundary_parameters", "ill_conditioned_covariance")
.procedures <- c("model_comparison", "assignment", "measurement", "transitions", "further_starts")

.reason_tokens <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text) || !nzchar(text)) return(character())
  parts <- strsplit(as.character(text), ",", fixed = TRUE)[[1]]
  parts[nzchar(parts)]
}

.reliability <- function(result) {
  reason <- result$status_reason
  if (is.null(reason) || length(reason) == 0 || is.na(reason)) reason <- ""
  tokens <- .reason_tokens(reason)
  blocking <- character()
  warning <- character()
  if (!isTRUE(result$converged) || "not_converged" %in% tokens) blocking <- c(blocking, "not_converged")
  if (!is.finite(result$loglik) || "numerical_failure" %in% tokens) blocking <- c(blocking, "numerical_failure")
  if ((isTRUE(result$n_starts >= 2) && isTRUE(result$n_best < 2)) || "best_loglik_not_replicated" %in% tokens) {
    blocking <- c(blocking, "best_loglik_not_replicated")
  }
  if ("not_positive_definite" %in% tokens || isFALSE(result$cov_positive_definite)) {
    blocking <- c(blocking, "not_positive_definite")
  }
  warning <- c(warning, intersect(.warn_tokens, tokens))
  if (isTRUE(result$n_starts < 2) || "replication_not_assessed" %in% tokens) {
    warning <- c(warning, "replication_not_assessed")
  }
  if (isTRUE(result$regularized) || "variance_floor" %in% tokens) warning <- c(warning, "variance_floor")
  if (isTRUE(result$n_boundary > 0) || "boundary_parameters" %in% tokens) {
    warning <- c(warning, "boundary_parameters")
  }
  list(
    reliable = !length(unique(blocking)),
    blocked = paste(unique(blocking), collapse = ","),
    warned = paste(unique(warning), collapse = ",")
  )
}

.eligibility <- function(blocking, warning, role) {
  block <- unique(blocking)
  warn <- unique(warning)
  flags <- list(
    model_comparison = TRUE, assignment = TRUE, measurement = TRUE,
    transitions = TRUE, further_starts = TRUE
  )
  detail <- character()
  if (length(block)) {
    primary <- intersect(.block_tokens, block)[[1]]
    for (proc in c("model_comparison", "assignment", "measurement", "transitions")) {
      flags[[proc]] <- FALSE
      detail <- c(detail, paste0(proc, ":", primary))
    }
  } else {
    meas <- intersect(.measurement_block, warn)
    if (length(meas)) {
      flags[["measurement"]] <- FALSE
      detail <- c(detail, paste0("measurement:", meas[[1]]))
    }
    comp <- intersect(.comparison_block, warn)
    if (length(comp)) {
      flags[["model_comparison"]] <- FALSE
      detail <- c(detail, paste0("model_comparison:", comp[[1]]))
    }
  }
  if (identical(as.character(role), "wave_diagnostic")) {
    flags[["model_comparison"]] <- FALSE
    detail <- detail[!startsWith(detail, "model_comparison:")]
    detail <- c("model_comparison:wave_diagnostic", detail)
  }
  notes <- intersect(.note_only, warn)
  list(flags = flags, detail = paste(detail, collapse = ","), notes = paste(notes, collapse = ","))
}

.mean_max_posterior <- function(result) {
  gamma <- result$gamma
  K <- result$K
  if (!length(gamma) || K < 1 || length(gamma) %% K != 0) return(NA_real_)
  mat <- matrix(gamma, nrow = K)
  keep <- apply(mat, 2, function(column) all(is.finite(column)))
  if (!any(keep)) return(NA_real_)
  mean(apply(mat[, keep, drop = FALSE], 2, max))
}

.criterion_note <- function(tab) {
  "BIC, AIC, and SABIC stay in separate columns. Nothing in this table totals them or chooses a model."
}

.small_state <- function(min_n, min_p, min_proportion, min_expected_n) {
  if (is.null(min_proportion) && is.null(min_expected_n)) {
    return(list(flag = FALSE, rule = ""))
  }
  flagged <- FALSE
  parts <- character()
  if (!is.null(min_proportion)) {
    parts <- c(parts, paste0("min_proportion=", min_proportion))
    if (is.finite(min_p) && min_p < min_proportion) flagged <- TRUE
  }
  if (!is.null(min_expected_n)) {
    parts <- c(parts, paste0("min_expected_n=", min_expected_n))
    if (is.finite(min_n) && min_n < min_expected_n) flagged <- TRUE
  }
  list(flag = flagged, rule = paste(parts, collapse = "; "))
}

#' @export
compare_models <- function(fits, wave = NULL, role = NULL, min_proportion = NULL, min_expected_n = NULL) {
  if (inherits(fits, "profylr_grid")) return(fits$comparison)
  if (inherits(fits, "profylr_fit")) fits <- list(fits)
  if (!length(fits)) stop("compare_models needs at least one fit", call. = FALSE)
  n <- length(fits)
  if (is.null(wave)) wave <- rep(NA, n)
  if (is.null(role)) role <- rep("candidate", n)
  if (length(wave) != n || length(role) != n) {
    stop("wave and role must have one entry per fit", call. = FALSE)
  }
  rows <- vector("list", n)
  for (i in seq_len(n)) {
    fit <- fits[[i]]
    result <- fit$result
    gate <- .reliability(result)
    membership <- result$expected_membership
    transitions <- result$expected_transitions
    min_n <- if (length(membership)) min(membership) else NA_real_
    min_p <- if (length(membership) && result$n_analyzed > 0) min_n / result$n_analyzed else NA_real_
    small <- .small_state(min_n, min_p, min_proportion, min_expected_n)
    supported <- isTRUE(result$ic_supported)
    rows[[i]] <- tibble::tibble(
      model_id = i,
      role = role[[i]],
      wave = wave[[i]],
      model = fit$model,
      K = result$K,
      covariance = if (!nzchar(result$covariance)) NA_character_ else result$covariance,
      measurement_invariant = result$measurement_invariant,
      means_invariant = result$means_invariant,
      covariance_invariant = result$covariance_invariant,
      transitions = if (result$n_waves < 2) NA_character_ else if (isTRUE(result$transition_equal)) "equal" else "free",
      loglik = result$loglik,
      n_parameters = result$n_parameters,
      bic = if (supported) result$bic else NA_real_,
      sabic = if (supported) result$sabic else NA_real_,
      aic = if (supported) result$aic else NA_real_,
      ic_supported = supported,
      ic_reason = if (supported) "" else "Information criteria are not supported for an unusable or regularized fit.",
      n_best = result$n_best,
      n_starts = result$n_starts,
      n_converged_starts = result$n_converged_starts,
      converged = result$converged,
      status = result$status,
      min_expected_n = min_n,
      min_expected_proportion = min_p,
      small_state_flag = small$flag,
      small_state_rule = small$rule,
      min_expected_transitions = if (length(transitions)) min(transitions) else NA_real_,
      transition_note = "Expected transition counts are reported. No sparsity cutoff is applied.",
      entropy_wave = result$entropy_wave,
      entropy_sequence = result$entropy_sequence,
      mean_max_posterior = .mean_max_posterior(result),
      assignment_note = "Entropy and the mean maximum posterior describe assignment certainty. They are not used to choose the number of states.",
      blrt = NA_real_,
      blrt_reason = "BLRT is not validated in Release 1.",
      lmr = NA_real_,
      lmr_reason = "LMR/VLMR is not validated in Release 1.",
      n_boundary = result$n_boundary,
      regularized = result$regularized,
      min_eigenvalue = result$min_eigenvalue,
      boundary_warning = if (isTRUE(result$n_boundary > 0)) "boundary parameter estimates" else "",
      regularization_warning = if (isTRUE(result$regularized)) "covariance was ridged onto a variance floor; information criteria are not supported" else "",
      reliable = gate$reliable,
      unreliable_reason = gate$blocked,
      inference_warning = gate$warned,
      criterion_note = "",
      selected = FALSE,
      selection_rationale = ""
    )
  }
  tab <- tibble::as_tibble(do.call(rbind, rows))
  tab$criterion_note <- .criterion_note(tab)
  same <- .panels_comparable(fits)
  tab$comparable <- same
  tab$incomparable <- !same
  tab
}

.utc_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

.as_comparison <- function(x) {
  if (inherits(x, "profylr_wave_enumeration")) {
    return(do.call(rbind, x$tables))
  }
  if (inherits(x, "profylr_grid")) return(x$comparison)
  if (inherits(x, "profylr_selection")) return(x$comparison)
  x
}

.eligibility_from_row <- function(row) {
  .eligibility(
    .reason_tokens(as.character(row$unreliable_reason[[1]])),
    .reason_tokens(as.character(row$inference_warning[[1]])),
    as.character(row$role[[1]])
  )
}

#' @export
record_selection <- function(comparison, chosen, rationale) {
  tab <- .as_comparison(comparison)
  rationale <- trimws(as.character(rationale))
  if (!nzchar(rationale)) {
    stop("Record the analyst's rationale. An empty rationale does not select a model.", call. = FALSE)
  }
  hit <- which(tab$model_id == chosen)
  if (!length(hit)) stop("chosen is not a model_id in this comparison", call. = FALSE)
  snapshot <- tab
  snapshot$selected <- FALSE
  snapshot$selection_rationale <- ""
  snapshot$selected[hit] <- TRUE
  snapshot$selection_rationale[hit] <- rationale
  gate <- .eligibility_from_row(snapshot[hit[[1]], ])
  flags <- gate$flags
  detail <- gate$detail
  notes <- gate$notes
  if (length(hit) > 1L) {
    flags[["model_comparison"]] <- FALSE
    extra <- "model_comparison:model_id_not_unique_across_waves"
    detail <- if (nzchar(detail)) paste(detail, extra, sep = ",") else extra
  }
  row <- snapshot[hit[[1]], ]
  structure(list(
    format_version = 1L,
    chosen = list(
      model_id = row$model_id[[1]],
      model = row$model[[1]],
      K = row$K[[1]],
      covariance = row$covariance[[1]],
      measurement_invariant = row$measurement_invariant[[1]],
      means_invariant = row$means_invariant[[1]],
      covariance_invariant = row$covariance_invariant[[1]],
      transitions = row$transitions[[1]],
      wave = row$wave[[1]],
      role = row$role[[1]]
    ),
    rationale = rationale,
    timestamp = .utc_now(),
    eligible_for_downstream = flags,
    eligibility_detail = detail,
    eligibility_notes = notes,
    comparison = snapshot
  ), class = "profylr_selection")
}

#' @export
save_model <- function(x, file) {
  payload <- list(format_version = 1L, package = "profylr", saved_at = .utc_now(), object = x)
  saveRDS(payload, file)
  invisible(file)
}

#' @export
load_model <- function(file) {
  payload <- readRDS(file)
  if (!is.list(payload) || !identical(payload$format_version, 1L) || !identical(payload$package, "profylr")) {
    stop("this file is not a profylr model archive of format version 1", call. = FALSE)
  }
  payload$object
}

#' @export
report_model <- function(x, procedure = NULL) {
  if (inherits(x, "profylr_model_selection")) {
    return(paste(c(
      x$reason,
      "This is the automatic selection from select_model, not an analyst record. accepted starts unset."
    ), collapse = "\n"))
  }
  if (inherits(x, "profylr_selection")) {
    flags <- x$eligible_for_downstream
    if (!is.null(procedure)) {
      if (!procedure %in% .procedures) {
        stop("procedure must be one of ", paste(.procedures, collapse = ", "), call. = FALSE)
      }
      if (!isTRUE(flags[[procedure]])) {
        why <- x$eligibility_detail
        if (is.null(why) || !nzchar(why)) why <- "unspecified"
        stop("This selection is not eligible for ", procedure, ": ", why, call. = FALSE)
      }
    }
    cov <- x$chosen$covariance
    if (is.na(cov) || !nzchar(as.character(cov))) cov <- "none"
    lines <- c(
      "Analyst selection",
      paste0("Chosen spec: model=", x$chosen$model, ", K=", x$chosen$K, ", covariance=", cov),
      paste0("Rationale: ", x$rationale),
      paste0("Recorded: ", x$timestamp),
      "Eligible for downstream use:"
    )
    for (name in .procedures) {
      lines <- c(lines, paste0("  ", name, ": ", if (isTRUE(flags[[name]])) "yes" else "no"))
    }
    if (!is.null(x$eligibility_detail) && nzchar(x$eligibility_detail)) {
      lines <- c(lines, paste0("Eligibility detail: ", x$eligibility_detail))
    }
    if (!is.null(x$eligibility_notes) && nzchar(x$eligibility_notes)) {
      lines <- c(lines, paste0("Notes: ", x$eligibility_notes))
    }
    lines <- c(
      lines,
      "This record preserves the analyst's choice. It is not an endorsement.",
      "The comparison table snapshot is stored with this selection.",
      "No automatic rule chose this model."
    )
    if (!is.null(procedure)) lines <- c(lines, paste0("Eligible for ", procedure, "."))
    return(paste(lines, collapse = "\n"))
  }
  "No model has been selected. compare_models does not choose a number of states."
}

.fit_state_grid <- function(data, indicators, id, wave, n_states, covariance, family, longitudinal, ...) {
  probe <- data
  probe_id <- id
  if (is.null(probe_id)) {
    probe[["._id"]] <- seq_len(nrow(probe))
    probe_id <- "._id"
  }
  panel <- prepare_panel(probe, indicators, probe_id, wave, family = family)
  gaussian <- any(panel$family == "gaussian")
  covs <- if (gaussian) as.character(covariance) else list(NULL)
  if (gaussian && !length(covs)) stop("name at least one covariance structure", call. = FALSE)
  fits <- list()
  for (K in n_states) {
    for (cov in covs) {
      fits[[length(fits) + 1L]] <- if (longitudinal) {
        fit_lta(data, indicators, id = id, wave = wave, n_states = K, family = family, covariance = cov, ...)
      } else if (gaussian) {
        fit_lpa(data, indicators, id = id, n_states = K, family = family, covariance = cov, ...)
      } else {
        fit_lca(data, indicators, id = id, wave = wave, n_states = K, family = family, ...)
      }
    }
  }
  fits
}

#' @export
fit_grid <- function(data, indicators, id = NULL, wave = NULL, n_states = 1:3,
                     covariance = c("EEI", "VVI", "EEE", "VVV"),
                     model = c("detect", "lpa", "lca", "lta"), by_wave = FALSE, family = NULL, ...) {
  model <- match.arg(model)
  data <- as.data.frame(data)
  n_states <- as.integer(n_states)
  if (!length(n_states) || any(n_states < 1L)) {
    stop("n_states must be one or more state counts, each at least 1", call. = FALSE)
  }
  if (by_wave || model == "lta") {
    if (is.null(wave) || is.null(id)) stop("a longitudinal grid needs id and wave", call. = FALSE)
  }
  if (by_wave) {
    waves <- sort(unique(data[[wave]]))
    fits <- list()
    tagged <- list()
    for (w in waves) {
      part <- data[data[[wave]] == w, , drop = FALSE]
      piece <- .fit_state_grid(part, indicators, id, NULL, n_states, covariance, family, FALSE, ...)
      for (fit in piece) {
        fits[[length(fits) + 1L]] <- fit
        tagged[[length(tagged) + 1L]] <- w
      }
    }
    comparison <- compare_models(fits, wave = unlist(tagged), role = rep("wave_diagnostic", length(fits)))
    return(structure(list(fits = fits, comparison = comparison), class = "profylr_grid"))
  }
  longitudinal <- model == "lta" || (model == "detect" && !is.null(wave) && length(unique(data[[wave]])) > 1L)
  if (longitudinal && (is.null(wave) || is.null(id))) {
    stop("a joint latent transition grid needs id and wave", call. = FALSE)
  }
  fits <- .fit_state_grid(data, indicators, id, if (longitudinal) wave else NULL,
                          n_states, covariance, family, longitudinal, ...)
  comparison <- compare_models(fits, role = rep("candidate", length(fits)))
  structure(list(fits = fits, comparison = comparison), class = "profylr_grid")
}

.wave_diagnostic_note <- paste(
  "Per-wave enumeration is diagnostic only. It does not choose the joint number of states.",
  "Extra states can compensate for wrongly imposed invariance."
)

#' @export
enumerate_waves <- function(...) {
  grid <- fit_grid(..., by_wave = TRUE)
  waves <- unique(grid$comparison$wave)
  tables <- lapply(waves, function(w) {
    sub <- grid$comparison[grid$comparison$wave == w, , drop = FALSE]
    sub$diagnostic_note <- .wave_diagnostic_note
    sub
  })
  names(tables) <- as.character(waves)
  structure(list(tables = tables, fits = grid$fits, note = .wave_diagnostic_note),
            class = "profylr_wave_enumeration")
}

#' @export
plot_fit_criteria <- function(x) {
  tab <- if (inherits(x, "profylr_grid")) x$comparison else x
  pieces <- lapply(c("bic", "sabic", "aic"), function(stat) {
    tibble::tibble(
      K = tab$K,
      covariance = ifelse(is.na(tab$covariance), "categorical", tab$covariance),
      criterion = toupper(stat),
      value = tab[[stat]],
      reliable = tab$reliable
    )
  })
  frame <- do.call(rbind, pieces)
  frame <- frame[is.finite(frame$value), , drop = FALSE]
  if (!nrow(frame)) stop("no supported information criteria to plot", call. = FALSE)
  ggplot2::ggplot(frame, ggplot2::aes(K, value, color = covariance, linetype = criterion,
                                      shape = reliable, group = interaction(covariance, criterion))) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::labs(
      title = "Descriptive fit criteria",
      subtitle = "Not a test and not a selection. Entropy is not used to choose the number of states.",
      x = "States", y = NULL, color = "Covariance", linetype = "Criterion", shape = "Reliable"
    )
}

.name_indicators <- function(index, panel) {
  vapply(index, function(j) panel$indicators[[j + 1L]], character(1))
}

#' @export
describe_indicators <- function(data, indicators, id, wave = NULL, by_wave = FALSE, family = NULL) {
  panel <- prepare_panel(data, indicators, id, wave, family = family)
  raw <- cpp_describe_indicators(panel, by_wave)
  out <- tibble::as_tibble(raw)
  out$variable <- .name_indicators(out$variable, panel)
  out$wave <- ifelse(out$wave < 0, NA, panel$waves[out$wave + 1L])
  out
}

#' @export
correlate_indicators <- function(data, indicators, id = NULL, wave = NULL, family = NULL) {
  data <- as.data.frame(data)
  if (is.null(id)) {
    data[["._id"]] <- seq_len(nrow(data))
    id <- "._id"
  }
  panel <- prepare_panel(data, indicators, id, wave, family = family)
  raw <- cpp_correlate_indicators(panel)
  out <- tibble::as_tibble(raw)
  out$variable_a <- .name_indicators(out$variable_a, panel)
  out$variable_b <- .name_indicators(out$variable_b, panel)
  out$wave <- ifelse(out$wave < 0, NA, out$wave)
  out
}

#' @export
fiml_moments <- function(data, indicators, id = NULL, wave = NULL, covariance = c("VVV", "EEI"), family = NULL) {
  covariance <- match.arg(covariance)
  data <- as.data.frame(data)
  if (is.null(id)) {
    data[["._id"]] <- seq_len(nrow(data))
    id <- "._id"
  }
  panel <- prepare_panel(data, indicators, id, wave, family = family)
  moments <- cpp_fiml_moments(panel, covariance)
  names(moments$mean) <- panel$indicators
  dimnames(moments$cov) <- list(panel$indicators, panel$indicators)
  dimnames(moments$cor) <- list(panel$indicators, panel$indicators)
  moments
}

#' @export
plot_profiles <- function(fit) {
  tidy_fit <- tidy(fit)
  means <- tidy_fit[tidy_fit$parameter == "mean", ]
  if (nrow(means)) {
    return(ggplot2::ggplot(means, ggplot2::aes(variable, estimate, color = factor(state), group = state)) +
             ggplot2::geom_line() + ggplot2::geom_point() +
             ggplot2::labs(color = "State", y = "State mean", x = NULL, title = "Gaussian profiles"))
  }
  resp <- tidy_fit[tidy_fit$parameter == "response", ]
  ggplot2::ggplot(resp, ggplot2::aes(variable, estimate, color = factor(state), group = interaction(state, category))) +
    ggplot2::geom_point() +
    ggplot2::labs(color = "State", y = "Category probability", x = NULL, title = "Response profiles")
}

#' @export
plot_transitions <- function(fit) {
  trans <- tidy(fit)
  trans <- trans[trans$parameter == "transition", ]
  if (!nrow(trans)) stop("a one-wave model has no transition matrix", call. = FALSE)
  if (any(!is.na(trans$wave))) trans <- trans[trans$wave == min(trans$wave, na.rm = TRUE), ]
  ggplot2::ggplot(trans, ggplot2::aes(factor(to), factor(state), fill = estimate)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", estimate))) +
    ggplot2::scale_fill_gradient(limits = c(0, 1), low = "white", high = "#1f4e79") +
    ggplot2::labs(x = "To", y = "From", fill = NULL, title = "Transition probabilities")
}

#' @export
describe_missingness <- function(data, indicators, id, wave = NULL, family = NULL) {
  panel <- prepare_panel(data, indicators, id, wave, family = family)
  out <- cpp_describe_missingness(panel)
  out$excluded_ids <- panel$ids[out$excluded_subjects + 1L]
  out
}

#' @export
standardize_indicators <- function(data, indicators, id, wave = NULL, scope = c("pooled", "within_wave")) {
  scope <- match.arg(scope)
  panel <- prepare_panel(data, indicators, id, wave)
  scaled <- cpp_standardize_indicators(panel, scope)
  if (isTRUE(scaled$warned) && identical(panel$measurement, "categorical")) {
    stop(scaled$warning, call. = FALSE)
  }
  out <- as.data.frame(data)
  values <- scaled$values
  if (!is.null(values)) {
    work <- as.data.frame(data)
    wave_col <- wave
    if (is.null(wave)) {
      work[["._wave"]] <- 1L
      wave_col <- "._wave"
    }
    id_pos <- stats::setNames(seq_along(panel$ids), as.character(panel$ids))
    wave_pos <- stats::setNames(seq_along(panel$waves), as.character(panel$waves))
    J <- panel$J
    T <- panel$T
    for (row in seq_len(nrow(work))) {
      i <- id_pos[[as.character(work[[id]][[row]])]]
      t <- wave_pos[[as.character(work[[wave_col]][[row]])]]
      if (is.null(i) || is.null(t)) next
      for (j in seq_along(panel$indicators)) {
        if (panel$family[[j]] != "gaussian") next
        value <- values[(i - 1L) * T * J + (t - 1L) * J + j]
        if (is.finite(value)) out[[panel$indicators[[j]]]][[row]] <- value
      }
    }
  }
  attr(out, "center") <- scaled$center
  attr(out, "scale") <- scaled$scale
  attr(out, "scope") <- scaled$scope
  attr(out, "warning") <- scaled$warning
  out
}

#' @export
compare_invariance <- function(restricted, unrestricted) {
  cpp_compare_invariance(restricted$panel, restricted$result, unrestricted$result)
}

#' @export
compare_transitions <- function(equal_fit, free_fit) {
  cpp_compare_transitions(equal_fit$panel, equal_fit$result, free_fit$result)
}

#' @export
add_starts <- function(fit, n_new, seed = 1) {
  if (n_new < 1) stop("n_new must be at least 1", call. = FALSE)
  spec <- fit$spec
  spec$has_start <- FALSE
  result <- cpp_add_starts(fit$panel, spec, fit$result, as.integer(n_new), as.numeric(seed))
  .new_fit(result, fit$panel, fit$spec, fit$model)
}

#' @export
align_states <- function(fit, reference) {
  params <- if (inherits(reference, "profylr_fit")) reference$result$params else reference
  out <- cpp_align_states(fit$panel, fit$spec, fit$result, params)
  list(fit = .new_fit(out$result, fit$panel, fit$spec, fit$model), alignment = out$alignment)
}

#' @export
posterior_states <- function(fit, kind = c("smoothed", "filtered", "modal")) {
  kind <- match.arg(kind)
  aug <- augment(fit)
  if (identical(kind, "modal")) return(aug[, c("id", "wave", "modal_state")])
  prefix <- if (identical(kind, "smoothed")) "smoothed_" else "filtered_"
  cols <- grep(paste0("^", prefix), names(aug), value = TRUE)
  rows <- vector("list", length(cols))
  for (i in seq_along(cols)) {
    rows[[i]] <- data.frame(
      id = aug$id, wave = aug$wave, kind = kind,
      state = as.integer(sub(prefix, "", cols[[i]])),
      probability = aug[[cols[[i]]]],
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

#' @export
posterior_transitions <- function(fit) {
  result <- fit$result
  .require_posteriors(result)
  K <- result$K
  N <- result$n_subjects
  T <- result$n_waves
  xi <- result$xi
  if (T < 2 || !length(xi)) {
    return(data.frame(id = integer(), from_wave = integer(), from_state = integer(),
                      to_state = integer(), probability = numeric()))
  }
  rows <- vector("list", N * (T - 1L) * K * K)
  pos <- 1L
  for (i in seq_len(N)) {
    for (step in seq_len(T - 1L)) {
      for (a in seq_len(K)) {
        for (b in seq_len(K)) {
          rows[[pos]] <- data.frame(
            id = fit$panel$ids[[i]],
            from_wave = fit$panel$waves[[step]],
            from_state = a,
            to_state = b,
            probability = xi[(((i - 1L) * (T - 1L) + (step - 1L)) * K + (a - 1L)) * K + b],
            stringsAsFactors = FALSE
          )
          pos <- pos + 1L
        }
      }
    }
  }
  do.call(rbind, rows)
}

#' @export
transition_matrix <- function(fit) {
  tidy_fit <- tidy(fit)
  tidy_fit[tidy_fit$parameter == "transition", , drop = FALSE]
}

#' @export
state_prevalence <- function(fit, kind = c("model", "posterior", "hard")) {
  kind <- match.arg(kind)
  if (!identical(kind, "model")) .require_posteriors(fit$result)
  key <- switch(kind, model = "prevalence_model", posterior = "prevalence_posterior", hard = "prevalence_hard")
  values <- fit$result[[key]]
  K <- fit$result$K
  n_waves <- length(values) / K
  data.frame(
    kind = kind,
    wave = rep(fit$panel$waves, each = K),
    state = rep(seq_len(K), times = n_waves),
    prevalence = values,
    stringsAsFactors = FALSE
  )
}

#' @export
expected_counts <- function(fit) {
  result <- fit$result
  .require_posteriors(result)
  K <- result$K
  membership <- result$expected_membership
  n_waves <- length(membership) / K
  member_rows <- data.frame(
    quantity = "membership",
    wave = rep(fit$panel$waves[seq_len(n_waves)], each = K),
    from_state = rep(seq_len(K), times = n_waves),
    to_state = NA_integer_,
    count = membership,
    stringsAsFactors = FALSE
  )
  transitions <- result$expected_transitions
  if (!length(transitions)) return(member_rows)
  intervals <- length(transitions) / (K * K)
  trans_rows <- data.frame(
    quantity = "transition",
    wave = rep(fit$panel$waves[seq_len(intervals)], each = K * K),
    from_state = rep(rep(seq_len(K), each = K), times = intervals),
    to_state = rep(rep(seq_len(K), times = K), times = intervals),
    count = transitions,
    stringsAsFactors = FALSE
  )
  rbind(member_rows, trans_rows)
}

#' @export
plot_missingness <- function(data, indicators, id, wave = NULL, family = NULL) {
  summary <- describe_missingness(data, indicators, id, wave, family)
  frame <- data.frame(pattern = summary$pattern, n = summary$pattern_n, stringsAsFactors = FALSE)
  ggplot2::ggplot(frame, ggplot2::aes(n, stats::reorder(pattern, n))) +
    ggplot2::geom_col(fill = "#1f4e79") +
    ggplot2::labs(x = "Subjects", y = NULL, title = "Wave-missingness patterns")
}
