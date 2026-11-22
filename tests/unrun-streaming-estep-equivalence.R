# Not collected by testthat (the name does not start with test-).
# Not executed with the streaming-E-step edit.
#
# Record the default stored fit on 2c53f102351ca57bcbbb3d58c75f3327ba3cf7be.
# Compare estep="streaming" on this PR. Identity follows the loaded package.
# The record file does not carry a SHA.
# Do not set LTA_STREAM_RECORD and LTA_STREAM_BASE together.
#
#   git worktree add /tmp/stream-base 2c53f102351ca57bcbbb3d58c75f3327ba3cf7be
#   cp profylr/tests/unrun-streaming-estep-equivalence.R /tmp/stream-base/profylr/tests/
#   Sys.setenv(LTA_STREAM_RECORD = "/tmp/stream-base.rds")
#   source("/tmp/stream-base/profylr/tests/unrun-streaming-estep-equivalence.R")

if (!"profylr" %in% loadedNamespaces()) {
  if (requireNamespace("profylr", quietly = TRUE)) {
    loadNamespace("profylr")
  } else {
    stop("profylr namespace is not loaded", call. = FALSE)
  }
}

record_commit <- "2c53f102351ca57bcbbb3d58c75f3327ba3cf7be"
record_hashes <- c(
  lta_cpp = "068e877b99bf3ef24465eb00c0c9f2ecc3f525227f276dd6aa522797e2821e4f",
  lta_hpp = "1ba2eada0e2f75d3e731a99b1ab3d07ffa6ac06874d05737a9637b6b284f4f73",
  profylr_cpp = "216bfab6d17b49553cc04b41386a66a1bd6e18a56e344b5f1f6cae82ca5e0afe",
  api_r = "bf25d6ebf9bbfc76f75ba552d0020ec199cdcc590f5ae8abd36f81cc7b24d25b",
  selection_r = "039db9ca60504f6a0732de09955aca784569e71354c252cb4355283ef8e8ca97"
)
head_hashes <- c(
  lta_cpp = "c9cdff1bdbd41be39af2d0a6e6826d4c3a14c2de4520b2aae4f436c68a949a56",
  lta_hpp = "1ebe8680c1e1949a9dc0a828fcb571764d453256858cefeec65d7ad21e5034ca",
  profylr_cpp = "86bd6ee8d8349e7fcba7e33c7210fcb3497c4b0c34d05150f0d456af8eb6d9f9",
  api_r = "0135f6885d0cd4f5ea0143b8be38b68be952001af7b4b5eda345f8dccbdf9573",
  selection_r = "b524804c8039cebffe587bee65531075bd733f57330ac8c7d7f596bac5101df3"
)

sha256_file <- function(path) {
  line <- system2("sha256sum", path, stdout = TRUE)
  if (!is.null(attr(line, "status")) && attr(line, "status") != 0) {
    stop("sha256sum failed for ", path, call. = FALSE)
  }
  strsplit(line[[1]], " ", fixed = TRUE)[[1]][[1]]
}

path_inside <- function(path, root) {
  path <- normalizePath(path, mustWork = TRUE)
  root <- normalizePath(root, mustWork = TRUE)
  prefix <- if (endsWith(root, .Platform$file.sep)) root else paste0(root, .Platform$file.sep)
  identical(path, root) || startsWith(path, prefix)
}

src_file <- function(fn) {
  file <- utils::getSrcFilename(fn, full.names = TRUE)
  if (length(file) != 1L || !nzchar(file)) return("")
  file
}

loaded_root <- function() {
  ns_path <- tryCatch(
    normalizePath(getNamespaceInfo("profylr", "path"), mustWork = TRUE),
    error = function(e) stop("profylr namespace is not loaded", call. = FALSE)
  )
  dlls <- getLoadedDLLs()
  if (!"profylr" %in% names(dlls)) stop("profylr DLL is not loaded", call. = FALSE)
  dll_path <- normalizePath(dlls[["profylr"]][["path"]], mustWork = TRUE)
  current <- ns_path
  if (!dir.exists(current)) current <- dirname(current)
  root <- NULL
  repeat {
    has_git <- dir.exists(file.path(current, ".git")) || file.exists(file.path(current, ".git"))
    has_api <- file.exists(file.path(current, "profylr", "R", "api.R"))
    has_core <- file.exists(file.path(current, "core", "src", "lta.cpp"))
    if (has_git && has_api && has_core) {
      root <- current
      break
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  if (is.null(root)) stop("loaded profylr path is not inside a source tree", call. = FALSE)
  if (!path_inside(ns_path, root) || !path_inside(dll_path, root)) {
    stop("loaded profylr namespace or DLL is outside the checked source tree", call. = FALSE)
  }
  api <- normalizePath(file.path(root, "profylr", "R", "api.R"), mustWork = TRUE)
  fit <- get("fit_lca", envir = asNamespace("profylr"))
  fit_file <- src_file(fit)
  if (!nzchar(fit_file) || !identical(normalizePath(fit_file, mustWork = TRUE), api)) {
    stop("loaded fit_lca was not defined from this tree's api.R", call. = FALSE)
  }
  root
}

git_out <- function(root, args) {
  line <- system2("git", c("-C", root, args), stdout = TRUE, stderr = FALSE)
  status <- attr(line, "status")
  if (!is.null(status) && status != 0) stop("git failed: ", paste(args, collapse = " "), call. = FALSE)
  if (!length(line)) return("")
  line[[1]]
}

source_identity <- function() {
  root <- loaded_root()
  c(
    commit = git_out(root, c("rev-parse", "HEAD")),
    lta_cpp = sha256_file(file.path(root, "core", "src", "lta.cpp")),
    lta_hpp = sha256_file(file.path(root, "core", "include", "lta", "lta.hpp")),
    profylr_cpp = sha256_file(file.path(root, "profylr", "src", "profylr.cpp")),
    api_r = sha256_file(file.path(root, "profylr", "R", "api.R")),
    selection_r = sha256_file(file.path(root, "profylr", "R", "selection.R"))
  )
}

require_record_identity <- function() {
  found <- source_identity()
  if (!identical(found[["commit"]], record_commit)) stop("record worktree is not 2c53f10", call. = FALSE)
  if (!identical(found[names(record_hashes)], record_hashes)) {
    stop("record sources are not the 2c53f10 copies", call. = FALSE)
  }
}

require_compare_identity <- function() {
  root <- loaded_root()
  found <- source_identity()
  if (identical(found[["commit"]], record_commit)) stop("compare worktree is still 2c53f10", call. = FALSE)
  if (identical(found[names(record_hashes)], record_hashes)) stop("compare sources still match 2c53f10", call. = FALSE)
  if (!identical(found[names(head_hashes)], head_hashes)) stop("compare sources are not this PR", call. = FALSE)
  git_out(root, c("cat-file", "-e", "HEAD:profylr/tests/unrun-streaming-estep-equivalence.R"))
}

same_result <- function(left, right) {
  identical(left, right, num.eq = FALSE, single.NA = FALSE)
}

categorical_lca <- function() {
  group <- rep(c(0, 1), each = 12)
  set.seed(23)
  u <- as.numeric(group)
  v <- as.numeric(ifelse(runif(24) < 0.8, group, 1 - group))
  u[1] <- NA
  data.frame(u = u, v = v)
}

categorical_lta <- function() {
  n <- 16L
  t <- 2L
  group <- rep(rep(c(0, 1), each = n / 2), each = t)
  set.seed(31)
  u <- as.numeric(group)
  v <- as.numeric(ifelse(runif(n * t) < 0.8, group, 1 - group))
  u[2] <- NA
  v[5] <- NA
  data.frame(id = rep(seq_len(n), each = t), wave = rep(c(1L, 2L), n), u = u, v = v)
}

controls <- list(n_starts = 4L, screen_iter = 0L, n_full = 2L, max_iter = 20L, seed = 23, n_threads = 1L)

pack_fit <- function(fit, posteriors = TRUE) {
  result <- fit$result
  packed <- list(
    loglik = result$loglik,
    n_iter = as.integer(result$n_iter),
    delta = result$params$delta,
    tau = result$params$tau,
    rho = result$params$rho
  )
  if (isTRUE(posteriors)) {
    packed$gamma <- result$gamma
    packed$filtered <- result$filtered
    packed$xi <- result$xi
  }
  packed
}

fit_pair <- function(extra = list()) {
  args <- c(controls, extra)
  lca <- do.call(profylr::fit_lca, c(list(categorical_lca(), c("u", "v"), n_states = 2L), args))
  lta <- do.call(profylr::fit_lta, c(list(categorical_lta(), c("u", "v"), "id", "wave", n_states = 2L), args))
  if (as.integer(lta$result$n_waves) < 2L) stop("categorical LTA fixture is not two waves", call. = FALSE)
  if (is.null(extra$posteriors) || isTRUE(extra$posteriors)) {
    if (!length(lta$result$xi)) stop("two-wave categorical fixture did not produce xi", call. = FALSE)
  }
  list(lca = lca, lta = lta)
}

same_frame <- function(left, right) {
  if (!identical(class(left), class(right))) stop("frame class changed", call. = FALSE)
  if (!identical(names(left), names(right))) stop("frame columns changed", call. = FALSE)
  for (nm in names(left)) {
    if (!identical(left[[nm]], right[[nm]], num.eq = FALSE, single.NA = FALSE)) {
      stop("frame column changed: ", nm, call. = FALSE)
    }
  }
}

check_viterbi <- function(fit) {
  marked <- fit
  res <- fit$result
  res$viterbi <- fit$result$viterbi
  res$viterbi[1] <- -1L
  marked$result <- res
  aug <- augment(marked)
  if (!inherits(aug, "tbl_df")) stop("augment is not a tibble", call. = FALSE)
  if (!all(c("smoothed_1", "filtered_1") %in% names(aug))) {
    stop("augment did not produce posterior columns", call. = FALSE)
  }
  present <- decode_states(marked, "viterbi")
  if (!inherits(present, "tbl_df")) stop("stored viterbi decode is not a tibble", call. = FALSE)
  if (!identical(names(present), c("id", "wave", "viterbi_state"))) {
    stop("stored viterbi columns changed", call. = FALSE)
  }
  same_frame(present, aug[, c("id", "wave", "viterbi_state")])
  if (!identical(present$viterbi_state[[1]], 0L)) stop("excluded viterbi code is not 0L", call. = FALSE)
  same_frame(decode_states(marked, "smoothed"), aug)
  bare <- marked
  bare$result$posteriors_computed <- NULL
  routed <- decode_states(bare, "viterbi")
  same_frame(present, routed)
  omitted <- marked
  omitted$result$posteriors_computed <- FALSE
  decoded <- decode_states(omitted, "viterbi")
  if (!inherits(decoded, "tbl_df")) stop("omitted viterbi decode is not a tibble", call. = FALSE)
  if (!identical(names(decoded), c("id", "wave", "viterbi_state"))) {
    stop("omitted viterbi columns changed", call. = FALSE)
  }
  if (!identical(decoded$viterbi_state[[1]], 0L)) stop("excluded viterbi code is not 0L", call. = FALSE)
}

record <- Sys.getenv("LTA_STREAM_RECORD", unset = "")
base <- Sys.getenv("LTA_STREAM_BASE", unset = "")
if (!nzchar(record) && !nzchar(base)) {
  message("skipped: set LTA_STREAM_RECORD or LTA_STREAM_BASE to run this comparison")
  quit(status = 0)
}
if (nzchar(record) && nzchar(base)) stop("set exactly one of LTA_STREAM_RECORD and LTA_STREAM_BASE", call. = FALSE)

if (nzchar(record)) {
  require_record_identity()
  pair <- fit_pair()
  payload <- list(lca = pack_fit(pair$lca), lta = pack_fit(pair$lta))
  saveRDS(payload, record)
} else {
  require_compare_identity()
  recorded <- readRDS(base)
  streamed <- fit_pair(list(estep = "streaming", posteriors = TRUE))
  omitted <- fit_pair(list(estep = "streaming", posteriors = FALSE))
  expect_closed <- function(expr) {
    raised <- tryCatch({
      force(expr)
      FALSE
    }, error = function(e) grepl("posteriors were not computed", conditionMessage(e), fixed = TRUE))
    if (!isTRUE(raised)) stop("accessor did not fail closed", call. = FALSE)
  }
  for (name in c("lca", "lta")) {
    if (!isTRUE(streamed[[name]]$result$posteriors_computed)) {
      stop("streaming posterior fit did not set posteriors_computed", call. = FALSE)
    }
    if (!same_result(pack_fit(streamed[[name]]), recorded[[name]])) {
      stop("streaming posterior fit does not match stored ", name, call. = FALSE)
    }
    if (!same_result(pack_fit(omitted[[name]], FALSE), recorded[[name]][c("loglik", "n_iter", "delta", "tau", "rho")])) {
      stop("streaming fit without posteriors does not match stored parameters ", name, call. = FALSE)
    }
    if (!isFALSE(omitted[[name]]$result$posteriors_computed)) {
      stop("posteriors_computed is not FALSE", call. = FALSE)
    }
    for (field in c("gamma", "filtered", "xi")) {
      value <- omitted[[name]]$result[[field]]
      if (!identical(value, numeric(0))) {
        stop("omitted posterior is not numeric(0): ", field, call. = FALSE)
      }
    }
    fit <- omitted[[name]]
    expect_closed(augment(fit))
    expect_closed(posterior_states(fit))
    expect_closed(posterior_transitions(fit))
    expect_closed(state_prevalence(fit, "posterior"))
    expect_closed(state_prevalence(fit, "hard"))
    expect_closed(expected_counts(fit))
    expect_closed(plot_prevalence(fit, "posterior"))
    expect_closed(plot_prevalence(fit, "hard"))
    expect_closed(plot_avepp(fit))
    expect_closed(plot_max_posterior(fit))
    state_prevalence(fit, "model")
    plot_prevalence(fit, "model")
    check_viterbi(streamed[[name]])
  }
}
