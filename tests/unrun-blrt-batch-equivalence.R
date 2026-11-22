# Not under testthat (the name does not start with test-).
# Not executed with the bounded-BLRT edit.
#
# This file is not in a861687943823dcaa39e4366421816536e072a6a. Copy it onto a
# worktree of that revision. The record run calls blrt_check with
# n_threads = 2 and no batch_size. On that revision that is one
# cpp_blrt_fit_draws of every simulated panel. The head run compares that
# record with the default batch and with sizes 1, 7, and B at 2 and 4
# workers, and .blrt_draw_pool at 1, 2, and 4 workers. A categorical panel with missing
# indicators is included. Do not set LTA_BLRT_RECORD and LTA_BLRT_BASE
# together.
#
#   git worktree add /tmp/blrt-base a861687943823dcaa39e4366421816536e072a6a
#   cp profylr/tests/unrun-blrt-batch-equivalence.R /tmp/blrt-base/profylr/tests/
#   Sys.setenv(LTA_BLRT_RECORD = "/tmp/blrt-base-r.rec")
#   # Load profylr from that worktree (pkgload::load_all), not from another
#   # library. The namespace path, the DLL, and the BLRT srcrefs must sit in
#   # the same tree whose HEAD and selection.R / lta.cpp are hashed.
#   source("profylr/tests/unrun-blrt-batch-equivalence.R")
#   Sys.unsetenv("LTA_BLRT_RECORD")
#   Sys.setenv(LTA_BLRT_BASE = "/tmp/blrt-base-r.rec")
#   source("profylr/tests/unrun-blrt-batch-equivalence.R")

if (!"package:profylr" %in% search()) library(profylr)

fields <- c(
  "lrs", "lr_draws", "b", "p", "p_plus", "n_ok", "n_failed",
  "n_failed_not_converged", "n_failed_error", "n_failed_inadmissible",
  "n_not_replicated", "n_retried", "failures", "attempts"
)

statistical <- function(result) {
  result[fields]
}

gaussian_fits <- function() {
  set.seed(19)
  group <- rep(c(-1.5, 1.5), each = 12)
  data <- data.frame(y1 = group + rnorm(24, 0, 0.4), y2 = group + rnorm(24, 0, 0.4))
  controls <- list(covariance = "EEI", n_starts = 4L, screen_iter = 0L, n_full = 2L,
                   max_iter = 20L, seed = 19L, n_threads = 1L)
  h0 <- do.call(profylr::fit_lpa, c(list(data, c("y1", "y2"), n_states = 1L), controls))
  h1 <- do.call(profylr::fit_lpa, c(list(data, c("y1", "y2"), n_states = 2L), controls))
  list(h0, h1)
}

categorical_fits <- function() {
  set.seed(23)
  group <- rep(c(0, 1), each = 12)
  u <- as.numeric(group)
  v <- as.numeric(ifelse(runif(24) < 0.8, group, 1 - group))
  u[c(1, 8)] <- NA
  v[4] <- NA
  data <- data.frame(u = u, v = v)
  controls <- list(n_starts = 4L, screen_iter = 0L, n_full = 2L, max_iter = 20L,
                   seed = 23L, n_threads = 1L)
  h0 <- do.call(profylr::fit_lca, c(list(data, c("u", "v"), n_states = 1L), controls))
  h1 <- do.call(profylr::fit_lca, c(list(data, c("u", "v"), n_states = 2L), controls))
  list(h0, h1)
}

run_check <- function(pair, threads, batch_size = NULL, use_batch = FALSE) {
  args <- list(pair[[1]], pair[[2]], B = 8L, max_retries = 1L, seed = 19,
               max_iter = 20L, n_threads = as.integer(threads), experimental = TRUE)
  if (isTRUE(use_batch)) args$batch_size <- batch_size
  result <- suppressMessages(do.call(profylr:::blrt_check, args))
  if (!is.finite(result$lr_observed)) stop("observed likelihood ratio is not finite", call. = FALSE)
  if (!identical(as.integer(result$B), 8L) || !identical(result$validated, FALSE)) {
    stop("B or validated changed", call. = FALSE)
  }
  statistical(result)
}

# num.eq = FALSE keeps signed zeros distinct. single.NA = FALSE keeps distinct
# NaN payloads distinct. all.equal(tolerance = 0) does neither.
same_result <- function(left, right) {
  identical(left, right, num.eq = FALSE, single.NA = FALSE)
}

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
    has_sel <- file.exists(file.path(current, "profylr", "R", "selection.R"))
    has_core <- file.exists(file.path(current, "core", "src", "lta.cpp"))
    if (has_git && has_sel && has_core) {
      root <- current
      break
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  if (is.null(root)) {
    stop("loaded profylr path is not inside a source tree", call. = FALSE)
  }
  if (!path_inside(ns_path, root) || !path_inside(dll_path, root)) {
    stop("loaded profylr namespace or DLL is outside the checked source tree", call. = FALSE)
  }
  selection <- normalizePath(file.path(root, "profylr", "R", "selection.R"), mustWork = TRUE)
  pool <- get(".blrt_draw_pool", envir = asNamespace("profylr"))
  check <- get("blrt_check", envir = asNamespace("profylr"))
  pool_file <- src_file(pool)
  check_file <- src_file(check)
  if (!nzchar(pool_file) || !nzchar(check_file)) {
    stop("loaded BLRT functions have no source reference", call. = FALSE)
  }
  if (!identical(normalizePath(pool_file, mustWork = TRUE), selection) ||
      !identical(normalizePath(check_file, mustWork = TRUE), selection)) {
    stop("loaded BLRT functions were not defined from this tree's selection.R", call. = FALSE)
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
  list(
    commit = git_out(root, c("rev-parse", "HEAD")),
    selection = sha256_file(file.path(root, "profylr", "R", "selection.R")),
    core = sha256_file(file.path(root, "core", "src", "lta.cpp"))
  )
}

base_commit <- "a861687943823dcaa39e4366421816536e072a6a"
base_selection <- "da0d0375739670d4d5d572cb58e6cca414e6993ccad52fa017b635df8237a69f"
base_core <- "8da03e20806891d3749643d6d79fc51216687c7509a4e02710634d0a8edf4bb5"
# selection.R and lta.cpp bytes at af953ef. A later edit fails closed until the pins move.
head_selection <- "b524804c8039cebffe587bee65531075bd733f57330ac8c7d7f596bac5101df3"
head_core <- "c9cdff1bdbd41be39af2d0a6e6826d4c3a14c2de4520b2aae4f436c68a949a56"
head_anchor <- "bdd9e76bba3a3a6dde0872a7c30dc63b06f4b178"
compare_commit <- "af953ef27053f3b0a7aae2cf117a9eb4434c5345"

require_record_identity <- function() {
  found <- source_identity()
  if (!identical(found$commit, base_commit)) stop("record worktree is not a861687", call. = FALSE)
  if (!identical(found$selection, base_selection) || !identical(found$core, base_core)) {
    stop("record sources are not the a861687 selection.R and lta.cpp", call. = FALSE)
  }
  found$commit
}

require_compare_identity <- function() {
  root <- loaded_root()
  found <- source_identity()
  if (identical(found$commit, base_commit)) stop("compare worktree is still a861687", call. = FALSE)
  git_out(root, c("merge-base", "--is-ancestor", head_anchor, "HEAD"))
  git_out(root, c("merge-base", "--is-ancestor", compare_commit, "HEAD"))
  if (!identical(found$selection, head_selection) || !identical(found$core, head_core)) {
    stop("compare sources are not the af953ef selection.R and lta.cpp", call. = FALSE)
  }
  git_out(root, c("cat-file", "-e", "HEAD:profylr/tests/unrun-blrt-batch-equivalence.R"))
  found$commit
}

record <- Sys.getenv("LTA_BLRT_RECORD", unset = NA)
base <- Sys.getenv("LTA_BLRT_BASE", unset = NA)
if (!is.na(record) && !is.na(base)) {
  message("FAIL set only one of LTA_BLRT_RECORD and LTA_BLRT_BASE")
  if (identical(record, base)) message("FAIL LTA_BLRT_RECORD and LTA_BLRT_BASE are the same path")
  quit(status = 1)
}
if (is.na(record) && is.na(base)) {
  message("skipped: set LTA_BLRT_RECORD or LTA_BLRT_BASE to run this comparison")
  quit(status = 0)
}

workloads <- list(
  gaussian_threaded_pool = gaussian_fits(),
  categorical_missing_threaded_pool = categorical_fits()
)
payload <- list(commit = if (!is.na(record)) require_record_identity() else require_compare_identity())
for (name in names(workloads)) {
  # n_threads = 2 and no batch_size: the historical full-vector pool on a861687.
  payload[[name]] <- run_check(workloads[[name]], 2L)
}

if (!is.na(record)) {
  saveRDS(payload, record)
  message("ok blrt base threaded-pool record")
  quit(status = 0)
}

saved <- readRDS(base)
if (!identical(saved$commit, "a861687943823dcaa39e4366421816536e072a6a")) {
  message("FAIL base record commit")
  quit(status = 1)
}
for (name in names(workloads)) {
  reference <- saved[[name]]
  if (is.null(reference)) {
    message("FAIL base record missing ", name)
    quit(status = 1)
  }
  pair <- workloads[[name]]
  for (threads in c(2L, 4L)) {
    if (!same_result(run_check(pair, threads), reference)) {
      message("FAIL default-batch blrt_check ", threads)
      quit(status = 1)
    }
    for (batch in c(1L, 7L, 8L)) {
      if (!same_result(run_check(pair, threads, batch, TRUE), reference)) {
        message("FAIL batched blrt_check ", threads, " ", batch)
        quit(status = 1)
      }
    }
  }
  pool_fields <- c(
    "lrs", "lr_draws", "n_failed", "n_failed_not_converged", "n_failed_error",
    "n_failed_inadmissible", "n_not_replicated", "n_retried", "failures", "attempts"
  )
  h0 <- pair[[1]]
  h1 <- pair[[2]]
  s0 <- max(as.integer(if (is.null(h0$spec$n_starts)) 2L else h0$spec$n_starts), 2L)
  s1 <- max(as.integer(if (is.null(h1$spec$n_starts)) 2L else h1$spec$n_starts), 2L)
  f0 <- max(as.integer(if (is.null(h0$spec$n_full)) s0 else h0$spec$n_full), 2L)
  f1 <- max(as.integer(if (is.null(h1$spec$n_full)) s1 else h1$spec$n_full), 2L)
  expected <- reference[pool_fields]
  for (threads in c(1L, 2L, 4L)) {
    for (batch in c(1L, 7L, 8L, threads)) {
      pooled <- profylr:::.blrt_draw_pool(h0, h1, 8L, 1L, 19, 20L, s0, f0, s1, f1, threads, batch)
      if (!same_result(pooled[pool_fields], expected)) {
        message("FAIL .blrt_draw_pool ", threads, " ", batch)
        quit(status = 1)
      }
    }
  }
}
message("ok blrt batch equivalence source")
quit(status = 0)
