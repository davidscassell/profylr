# Not collected by testthat (the name does not start with test-).
# Not executed with the within-fit E-step edit.
#
# Record serial fits on f1cf2bd7cb41e302da7db2194642f8c004bf074d.
# estep_threads=1 must match that record bitwise. Threads 2, 4, and 8 must
# match each other bitwise and lie within 1e-8 of serial.
# Do not set LTA_ETHREAD_RECORD and LTA_ETHREAD_BASE together.

if (!"profylr" %in% loadedNamespaces()) loadNamespace("profylr")

record_commit <- "f1cf2bd7cb41e302da7db2194642f8c004bf074d"
record_hashes <- c(
  lta_cpp = "a7a9d7796f2451977cb23cdda70744e5ee45882843514a35d237781a73ce5d36",
  lta_hpp = "dc9d9fe767b0c61adecb838059c4ff1d47cc10c2c3b5adbbd5080dc766433e5c",
  profylr_cpp = "39a419509ba1a12e5037518f9597f6d8762354ef101d1fb5cc1c3785cacda905",
  api_r = "4c82c57b74b43ff62c4033c104e98dea32cc878dcce4a95667a8fce8559ed4f2"
)
head_hashes <- c(
  lta_cpp = "c9cdff1bdbd41be39af2d0a6e6826d4c3a14c2de4520b2aae4f436c68a949a56",
  lta_hpp = "1ebe8680c1e1949a9dc0a828fcb571764d453256858cefeec65d7ad21e5034ca",
  profylr_cpp = "86bd6ee8d8349e7fcba7e33c7210fcb3497c4b0c34d05150f0d456af8eb6d9f9",
  api_r = "0135f6885d0cd4f5ea0143b8be38b68be952001af7b4b5eda345f8dccbdf9573"
)

sha256_file <- function(path) {
  line <- system2("sha256sum", path, stdout = TRUE)
  if (!is.null(attr(line, "status")) && attr(line, "status") != 0) stop("sha256sum failed", call. = FALSE)
  strsplit(line[[1]], " ", fixed = TRUE)[[1]][[1]]
}
git_out <- function(root, args) {
  line <- system2("git", c("-C", root, args), stdout = TRUE, stderr = FALSE)
  status <- attr(line, "status")
  if (!is.null(status) && status != 0) stop("git failed", call. = FALSE)
  if (!length(line)) return("")
  line[[1]]
}
loaded_root <- function() {
  ns <- normalizePath(getNamespaceInfo("profylr", "path"), mustWork = TRUE)
  root <- ns
  repeat {
    if ((dir.exists(file.path(root, ".git")) || file.exists(file.path(root, ".git"))) &&
        file.exists(file.path(root, "core", "src", "lta.cpp"))) break
    parent <- dirname(root)
    if (identical(parent, root)) stop("loaded profylr is not inside a source tree", call. = FALSE)
    root <- parent
  }
  root
}
source_identity <- function() {
  root <- loaded_root()
  c(
    commit = git_out(root, c("rev-parse", "HEAD")),
    lta_cpp = sha256_file(file.path(root, "core", "src", "lta.cpp")),
    lta_hpp = sha256_file(file.path(root, "core", "include", "lta", "lta.hpp")),
    profylr_cpp = sha256_file(file.path(root, "profylr", "src", "profylr.cpp")),
    api_r = sha256_file(file.path(root, "profylr", "R", "api.R"))
  )
}

pack_fit <- function(fit) {
  result <- fit$result
  list(
    loglik = result$loglik, n_iter = as.integer(result$n_iter),
    delta = result$params$delta, tau = result$params$tau, rho = result$params$rho,
    means = result$params$means, cov = result$params$cov,
    gamma = result$gamma, filtered = result$filtered, xi = result$xi
  )
}
same_bits <- function(left, right) identical(left, right, num.eq = FALSE, single.NA = FALSE)
close_numeric <- function(left, right, name) {
  a <- as.numeric(left)
  b <- as.numeric(right)
  if (length(a) != length(b)) stop("shape changed ", name, call. = FALSE)
  nan_a <- is.nan(a)
  nan_b <- is.nan(b)
  pos_a <- is.infinite(a) & a > 0
  pos_b <- is.infinite(b) & b > 0
  neg_a <- is.infinite(a) & a < 0
  neg_b <- is.infinite(b) & b < 0
  if (!identical(nan_a, nan_b) || !identical(pos_a, pos_b) || !identical(neg_a, neg_b)) {
    stop("non-finite pattern ", name, call. = FALSE)
  }
  finite <- !(nan_a | pos_a | neg_a)
  if (!any(finite)) return(invisible(NULL))
  diff <- abs(a[finite] - b[finite])
  if (any(is.nan(diff))) stop("NaN in finite diff ", name, call. = FALSE)
  if (max(diff) > 1e-8) stop(name, " outside 1e-8", call. = FALSE)
  invisible(NULL)
}
close_fit <- function(serial, parallel) {
  if (!identical(as.integer(serial$n_iter), as.integer(parallel$n_iter))) stop("iteration count changed", call. = FALSE)
  close_numeric(serial$loglik, parallel$loglik, "loglik")
  for (key in c("delta", "tau", "rho", "means", "cov")) {
    close_numeric(serial$params[[key]], parallel$params[[key]], key)
  }
  for (key in c("gamma", "filtered", "xi")) close_numeric(serial[[key]], parallel[[key]], key)
}

frames <- function() {
  # 16 full blocks of 32 plus a one-subject remainder.
  n <- 513L
  set.seed(11)
  cat <- c(rep(0, 256), rep(1, 257))
  u <- as.numeric(cat)
  v <- as.numeric(ifelse(runif(n) < 0.75, cat, 1 - cat))
  u[1] <- NA
  lca <- data.frame(u = u, v = v)
  group <- rep(cat, each = 2)
  uu <- as.numeric(group)
  vv <- as.numeric(ifelse(runif(n * 2L) < 0.75, group, 1 - group))
  uu[2] <- NA
  lta <- data.frame(id = rep(seq_len(n), each = 2), wave = rep(c(1L, 2L), n), u = uu, v = vv)
  g <- ifelse(cat == 0, -1, 1)
  gauss <- data.frame(y1 = g + rnorm(n, 0, 0.3), y2 = g + rnorm(n, 0, 0.3))
  list(lca = lca, lta = lta, gauss = gauss)
}

fit_all <- function(extra = list()) {
  controls <- c(list(n_starts = 2L, screen_iter = 0L, n_full = 2L, max_iter = 15L, seed = 11, n_threads = 1L), extra)
  data <- frames()
  out <- list(
    lca = do.call(profylr::fit_lca, c(list(data$lca, c("u", "v"), n_states = 2L), controls)),
    lta = do.call(profylr::fit_lta, c(list(data$lta, c("u", "v"), "id", "wave", n_states = 2L), controls)),
    lta_stream = do.call(profylr::fit_lta, c(list(data$lta, c("u", "v"), "id", "wave", n_states = 2L, estep = "streaming"), controls)),
    lpa = do.call(profylr::fit_lpa, c(list(data$gauss, c("y1", "y2"), n_states = 2L, covariance = "EEI"), controls))
  )
  analyzed <- as.integer(out$lta$result$n_analyzed)
  if (analyzed < 512L || analyzed %% 32L == 0L) {
    stop("fixture is not 512 or more subjects with a partial last block", call. = FALSE)
  }
  nblocks <- (analyzed + 31L) %/% 32L
  assignment <- function(nthreads) {
    spans <- character(nthreads)
    cursor <- 0L
    left_blocks <- nblocks
    left_threads <- nthreads
    for (t in seq_len(nthreads)) {
      take <- left_blocks %/% left_threads
      spans[[t]] <- paste0(cursor, ":", cursor + take)
      cursor <- cursor + take
      left_blocks <- left_blocks - take
      left_threads <- left_threads - 1L
    }
    paste(spans, collapse = ",")
  }
  splits <- c(assignment(2L), assignment(4L), assignment(8L))
  if (length(unique(splits)) != 3L) stop("2/4/8 threads share a block assignment", call. = FALSE)
  waves <- function(nthreads) {
    grouped <- character()
    wave <- 0L
    while (wave < nblocks) {
      grouped <- c(grouped, paste(wave:min(wave + nthreads - 1L, nblocks - 1L), collapse = "+"))
      wave <- wave + nthreads
    }
    paste(grouped, collapse = ",")
  }
  if (length(unique(c(waves(2L), waves(4L), waves(8L)))) != 3L) {
    stop("2/4/8 threads share a streaming wave", call. = FALSE)
  }
  out
}

record <- Sys.getenv("LTA_ETHREAD_RECORD", unset = "")
base <- Sys.getenv("LTA_ETHREAD_BASE", unset = "")
if (!nzchar(record) && !nzchar(base)) {
  message("skipped: set LTA_ETHREAD_RECORD or LTA_ETHREAD_BASE to run this comparison")
  quit(status = 0)
}
if (nzchar(record) && nzchar(base)) stop("set exactly one of LTA_ETHREAD_RECORD and LTA_ETHREAD_BASE", call. = FALSE)
found <- source_identity()
if (nzchar(record)) {
  if (!identical(found[["commit"]], record_commit) || !identical(found[names(record_hashes)], record_hashes)) {
    stop("record tree is not f1cf2bd with its sources", call. = FALSE)
  }
  saveRDS(lapply(fit_all(), pack_fit), record)
} else {
  if (identical(found[["commit"]], record_commit) || identical(found[names(head_hashes)], record_hashes) ||
      !identical(found[names(head_hashes)], head_hashes)) {
    stop("compare sources are not this PR's copies", call. = FALSE)
  }
  git_out(loaded_root(), c("cat-file", "-e", "HEAD:profylr/tests/unrun-estep-threads-equivalence.R"))
  recorded <- readRDS(base)
  serial <- fit_all(list(estep_threads = 1L))
  for (name in names(serial)) {
    if (!same_bits(pack_fit(serial[[name]]), recorded[[name]])) {
      stop("estep_threads=1 does not match the serial record ", name, call. = FALSE)
    }
  }
  parallel <- lapply(c(2L, 4L, 8L), function(threads) fit_all(list(estep_threads = threads)))
  names(parallel) <- c("2", "4", "8")
  for (name in names(serial)) {
    if (!same_bits(pack_fit(parallel[["4"]][[name]]), pack_fit(parallel[["2"]][[name]])) ||
        !same_bits(pack_fit(parallel[["8"]][[name]]), pack_fit(parallel[["2"]][[name]]))) {
      stop("thread counts disagree ", name, call. = FALSE)
    }
    close_fit(serial[[name]]$result, parallel[["2"]][[name]]$result)
  }
}
