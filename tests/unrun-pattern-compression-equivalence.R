# Not collected by testthat (the name does not start with test-).
# Not executed with the pattern-compression edit.
#
# Record uncompressed fits on c8feb1bd7bb3e6710933bc8a4b58e9c9ee7c2635.
# patterns="none" must match that record bitwise. patterns="compress" must
# lie within the non-finite-then-1e-8 rule. Threads 2, 4, and 8 in compressed
# mode must match each other bitwise.

if (!"profylr" %in% loadedNamespaces()) loadNamespace("profylr")

record_commit <- "c8feb1bd7bb3e6710933bc8a4b58e9c9ee7c2635"
record_hashes <- c(
  lta_cpp = "70b3f8d9c5cb1299184194d3d64b2dd70d72ca163b63e772530ad01d8f363320",
  lta_hpp = "fa6565abdbc992afb0a71da65da4ee1399c9d2e1809277c4b5aeaa8694dcc9f3",
  profylr_cpp = "d13dac594b2053927baef05cd8da91bc9fa24876034dd95dc602fdcac1c74d60",
  api_r = "01f0e8a676db2fe991334ffc4e64f470566d58c4e921f0a1a5ecc3e2a1d10377"
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

pack_fit <- function(fit, posteriors = TRUE) {
  result <- fit$result
  packed <- list(
    loglik = result$loglik, n_iter = as.integer(result$n_iter),
    delta = result$params$delta, tau = result$params$tau, rho = result$params$rho,
    means = result$params$means, cov = result$params$cov
  )
  if (isTRUE(posteriors)) {
    packed$gamma <- result$gamma
    packed$filtered <- result$filtered
    packed$xi <- result$xi
  }
  packed
}
same_codes <- function(left, right, name) {
  if (length(left) != length(right)) stop(name, " length", call. = FALSE)
  if (!identical(as.integer(left), as.integer(right))) stop(name, " code", call. = FALSE)
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
close_fit <- function(serial, parallel, posteriors = TRUE) {
  if (!identical(as.integer(serial$n_iter), as.integer(parallel$n_iter))) stop("iteration count changed", call. = FALSE)
  close_numeric(serial$loglik, parallel$loglik, "loglik")
  for (key in c("delta", "tau", "rho", "means", "cov")) {
    close_numeric(serial$params[[key]], parallel$params[[key]], key)
  }
  if (isTRUE(posteriors)) for (key in c("gamma", "filtered", "xi")) close_numeric(serial[[key]], parallel[[key]], key)
}

frames <- function() {
  set.seed(11)
  base <- rbind(c(0, 0), c(0, 1), c(1, 0), c(1, 1))
  dup <- base[rep(1:4, length.out = 80), , drop = FALSE]
  dup[1, 1] <- NA
  dup[4, 2] <- NA
  dup <- rbind(dup, c(NA_real_, NA_real_))
  group <- rep(1:4, length.out = 40)
  uu <- rep(base[group, 1], each = 2)
  vv <- rep(base[group, 2], each = 2)
  uu[2] <- NA
  lta <- data.frame(
    id = c(rep(seq_len(40), each = 2), 41L, 41L),
    wave = c(rep(c(1L, 2L), 40), 1L, 2L),
    u = c(uu, NA_real_, NA_real_),
    v = c(vv, NA_real_, NA_real_)
  )
  wide_n <- 513L
  bits <- sapply(0:9, function(bit) bitwAnd(bitwShiftR(seq_len(wide_n) - 1L, bit), 1L))
  bits[1, 1] <- NA
  list(
    dup = data.frame(u = dup[, 1], v = dup[, 2]),
    wide = as.data.frame(bits),
    lta = lta,
    gauss = data.frame(y = seq(-1, 1, length.out = 12)),
    mixed = data.frame(id = c(1L, 1L, 2L, 2L), wave = c(1L, 2L, 1L, 2L),
                       u = c(0, 1, 0, 1), y = c(0.2, -0.4, 1.5, 0.7))
  )
}

.merge_controls <- function(base, extra) {
  base[names(extra)] <- extra
  base
}

fit_dup <- function(extra = list(), posteriors = TRUE, estep = "stored") {
  controls <- .merge_controls(list(
    n_starts = 2L, screen_iter = 0L, n_full = 2L, max_iter = 12L, seed = 11,
    n_threads = 1L, posteriors = posteriors, estep = estep
  ), extra)
  do.call(profylr::fit_lca, c(list(frames()$dup, c("u", "v"), n_states = 2L), controls))
}
fit_wide <- function(extra = list(), estep = "stored") {
  data <- frames()$wide
  controls <- .merge_controls(list(
    n_starts = 1L, screen_iter = 0L, n_full = 1L, max_iter = 4L, seed = 3,
    n_threads = 1L, estep = estep
  ), extra)
  do.call(profylr::fit_lca, c(list(data, names(data), n_states = 2L), controls))
}
fit_lta_fix <- function(extra = list()) {
  controls <- .merge_controls(list(
    n_starts = 2L, screen_iter = 0L, n_full = 2L, max_iter = 8L, seed = 5, n_threads = 1L
  ), extra)
  do.call(profylr::fit_lta, c(list(frames()$lta, c("u", "v"), "id", "wave", n_states = 2L), controls))
}

repeated_frame <- function(waves) {
  rows <- list()
  subject <- 0L
  for (pid in 0:256) {
    bits <- bitwAnd(bitwShiftR(pid, 0:8), 1L)
    missing <- pid %% 19L == 0L
    reps <- 2L + pid %% 3L
    for (rep_i in seq_len(reps)) {
      if (waves == 1L) {
        vals <- as.numeric(bits)
        if (missing) vals[1] <- NA_real_
        rows[[length(rows) + 1L]] <- vals
      } else {
        wave1 <- as.numeric(bits[1:5])
        wave2 <- c(as.numeric(bits[6:9]), 0)
        if (missing) {
          wave1[1] <- NA_real_
          wave2[5] <- NA_real_
        }
        rows[[length(rows) + 1L]] <- c(subject, 1, wave1)
        rows[[length(rows) + 1L]] <- c(subject, 2, wave2)
      }
      subject <- subject + 1L
    }
  }
  if (waves == 1L) {
    out <- as.data.frame(do.call(rbind, rows))
    names(out) <- paste0("c", 0:8)
    return(out)
  }
  out <- as.data.frame(do.call(rbind, rows))
  names(out) <- c("id", "wave", paste0("c", 0:4))
  out
}

fit_repeated <- function(kind, extra = list(), estep = "stored", posteriors = TRUE) {
  controls <- .merge_controls(list(
    n_starts = 1L, screen_iter = 0L, n_full = 1L, max_iter = 3L, seed = 7,
    n_threads = 1L, posteriors = posteriors, estep = estep
  ), extra)
  if (identical(kind, "lca")) {
    data <- repeated_frame(1L)
    return(do.call(profylr::fit_lca, c(list(data, names(data), n_states = 2L), controls)))
  }
  data <- repeated_frame(2L)
  indicators <- paste0("c", 0:4)
  if (identical(kind, "lca-wave")) {
    return(do.call(profylr::fit_lca, c(list(data, indicators, "id", "wave", n_states = 2L), controls)))
  }
  do.call(profylr::fit_lta, c(list(data, indicators, "id", "wave", n_states = 2L), controls))
}

require_repeated <- function(result, threads) {
  unique <- as.integer(result$n_patterns)
  analyzed <- as.integer(result$n_analyzed)
  if (unique <= 32L || analyzed <= unique) stop("repeated fixture does not have U>32 and counts>1", call. = FALSE)
  blocks <- (unique + 31L) %/% 32L
  if (blocks <= threads) stop("pattern blocks are not greater than estep_threads", call. = FALSE)
}

cover_blocked <- function() {
  plain <- fit_repeated("lca")
  serial <- fit_repeated("lca", list(patterns = "compress", estep_threads = 1L))
  require_repeated(serial$result, 8L)
  close_fit(plain$result, serial$result)
  stored <- lapply(c(2L, 4L, 8L), function(threads) {
    fit_repeated("lca", list(patterns = "compress", estep_threads = threads))
  })
  for (i in seq_along(stored)) {
    require_repeated(stored[[i]]$result, c(2L, 4L, 8L)[[i]])
    if (!same_bits(pack_fit(stored[[i]]), pack_fit(serial))) {
      stop("stored compress threads are not bitwise-equal", call. = FALSE)
    }
    close_fit(plain$result, stored[[i]]$result)
  }
  plain_omit <- fit_repeated("lca", estep = "streaming", posteriors = FALSE)
  serial_omit <- fit_repeated("lca", list(patterns = "compress", estep_threads = 1L),
                              estep = "streaming", posteriors = FALSE)
  require_repeated(serial_omit$result, 8L)
  close_fit(plain_omit$result, serial_omit$result, FALSE)
  omitted <- lapply(c(2L, 4L, 8L), function(threads) {
    fit_repeated("lca", list(patterns = "compress", estep_threads = threads),
                 estep = "streaming", posteriors = FALSE)
  })
  for (i in seq_along(omitted)) {
    require_repeated(omitted[[i]]$result, c(2L, 4L, 8L)[[i]])
    for (field in c("gamma", "filtered", "xi")) {
      if (!identical(omitted[[i]]$result[[field]], numeric(0))) {
        stop("repeated omitted posterior is not empty", call. = FALSE)
      }
    }
    close_fit(serial_omit$result, omitted[[i]]$result, FALSE)
    close_fit(plain_omit$result, omitted[[i]]$result, FALSE)
  }
  if (!same_bits(pack_fit(omitted[[2]], FALSE), pack_fit(omitted[[1]], FALSE)) ||
      !same_bits(pack_fit(omitted[[3]], FALSE), pack_fit(omitted[[1]], FALSE))) {
    stop("omitted streaming threads disagree", call. = FALSE)
  }
  for (kind in c("lca-wave", "lta")) {
    plain_wave <- fit_repeated(kind, estep = "streaming")
    serial_wave <- fit_repeated(kind, list(patterns = "compress", estep_threads = 1L), estep = "streaming")
    if (as.integer(serial_wave$result$n_waves) < 2L) stop("repeated multi-wave fixture", call. = FALSE)
    require_repeated(serial_wave$result, 8L)
    close_fit(plain_wave$result, serial_wave$result)
    threaded <- lapply(c(2L, 4L, 8L), function(threads) {
      fit_repeated(kind, list(patterns = "compress", estep_threads = threads), estep = "streaming")
    })
    for (i in seq_along(threaded)) {
      require_repeated(threaded[[i]]$result, c(2L, 4L, 8L)[[i]])
      close_fit(serial_wave$result, threaded[[i]]$result)
      close_fit(plain_wave$result, threaded[[i]]$result)
    }
    if (!same_bits(pack_fit(threaded[[2]]), pack_fit(threaded[[1]])) ||
        !same_bits(pack_fit(threaded[[3]]), pack_fit(threaded[[1]]))) {
      stop("multi-wave streaming threads disagree ", kind, call. = FALSE)
    }
  }
}

record <- Sys.getenv("LTA_PATTERN_RECORD", unset = "")
base <- Sys.getenv("LTA_PATTERN_BASE", unset = "")
if (!nzchar(record) && !nzchar(base)) {
  message("skipped: set LTA_PATTERN_RECORD or LTA_PATTERN_BASE to run this comparison")
  quit(status = 0)
}
if (nzchar(record) && nzchar(base)) stop("set exactly one of LTA_PATTERN_RECORD and LTA_PATTERN_BASE", call. = FALSE)
found <- source_identity()
if (nzchar(record)) {
  if (!identical(found[["commit"]], record_commit) || !identical(found[names(record_hashes)], record_hashes)) {
    stop("record tree is not c8feb1b with its sources", call. = FALSE)
  }
  saveRDS(list(
    dup = pack_fit(fit_dup()), dup_stream = pack_fit(fit_dup(estep = "streaming")),
    wide = pack_fit(fit_wide()), lta = pack_fit(fit_lta_fix())
  ), record)
} else {
  if (identical(found[["commit"]], record_commit) || identical(found[names(head_hashes)], record_hashes) ||
      !identical(found[names(head_hashes)], head_hashes)) {
    stop("compare sources are not this PR's copies", call. = FALSE)
  }
  git_out(loaded_root(), c("cat-file", "-e", "HEAD:profylr/tests/unrun-pattern-compression-equivalence.R"))
  recorded <- readRDS(base)
  plain <- list(
    dup = fit_dup(list(patterns = "none")),
    dup_stream = fit_dup(list(patterns = "none"), estep = "streaming"),
    wide = fit_wide(list(patterns = "none")),
    lta = fit_lta_fix(list(patterns = "none"))
  )
  bare <- fit_dup()
  if (!is.null(bare$result$patterns) || !is.null(bare$result$n_patterns) ||
      !is.null(bare$result$pattern_id)) {
    stop("default fit exported a patterns key", call. = FALSE)
  }
  for (name in names(plain)) {
    if (!same_bits(pack_fit(plain[[name]]), recorded[[name]])) {
      stop("patterns=none does not match the record ", name, call. = FALSE)
    }
    if (!is.null(plain[[name]]$result$patterns) || !is.null(plain[[name]]$result$n_patterns) ||
        !is.null(plain[[name]]$result$pattern_id)) {
      stop("default fit exported pattern metadata", call. = FALSE)
    }
  }
  compressed <- fit_dup(list(patterns = "compress"))
  if (as.integer(compressed$result$n_patterns) >= as.integer(compressed$result$n_analyzed)) {
    stop("duplicate fixture did not collapse", call. = FALSE)
  }
  if (length(compressed$result$pattern_id) != as.integer(compressed$result$n_subjects)) {
    stop("pattern_id length", call. = FALSE)
  }
  close_fit(plain$dup$result, compressed$result)
  same_codes(plain$dup$result$viterbi, compressed$result$viterbi, "viterbi")
  same_codes(plain$dup$result$modal, compressed$result$modal, "modal")
  lta_compressed <- fit_lta_fix(list(patterns = "compress"))
  close_fit(plain$lta$result, lta_compressed$result)
  same_codes(plain$lta$result$viterbi, lta_compressed$result$viterbi, "lta viterbi")
  same_codes(plain$lta$result$modal, lta_compressed$result$modal, "lta modal")
  if (as.integer(plain$lta$result$n_waves) < 2L) stop("multi-wave fixture", call. = FALSE)
  pid <- compressed$result$pattern_id
  if (!any(pid < 0L)) stop("excluded subject is not pattern -1", call. = FALSE)
  width <- as.integer(compressed$result$n_waves)
  if (length(compressed$result$viterbi) != as.integer(compressed$result$n_subjects) * width ||
      length(compressed$result$modal) != length(compressed$result$viterbi)) {
    stop("expanded state length", call. = FALSE)
  }
  seen <- list()
  for (i in seq_along(pid)) {
    if (pid[[i]] < 0L) next
    sl <- compressed$result$viterbi[((i - 1L) * width + 1L):(i * width)]
    md <- compressed$result$modal[((i - 1L) * width + 1L):(i * width)]
    key <- as.character(pid[[i]])
    if (!is.null(seen[[key]]) && (!identical(seen[[key]]$v, sl) || !identical(seen[[key]]$m, md))) {
      stop("pattern did not expand to the same modal and Viterbi", call. = FALSE)
    }
    seen[[key]] <- list(v = sl, m = md)
  }
  streamed <- fit_dup(list(patterns = "compress"), estep = "streaming")
  close_fit(plain$dup$result, streamed$result)
  omitted <- fit_dup(list(patterns = "compress"), posteriors = FALSE, estep = "streaming")
  if (!isFALSE(omitted$result$posteriors_computed)) stop("omitted streaming posterior flag", call. = FALSE)
  close_fit(plain$dup$result, omitted$result, FALSE)
  for (field in c("gamma", "filtered", "xi")) {
    if (!identical(omitted$result[[field]], numeric(0))) stop("omitted posterior is not empty", call. = FALSE)
  }
  if (!identical(omitted$result$patterns, "compress")) stop("compressed fit omitted the patterns key", call. = FALSE)
  pooled <- fit_dup(list(patterns = "compress", n_threads = 2L, estep_threads = 8L))
  if (!same_bits(pack_fit(pooled), pack_fit(compressed))) stop("start pool changed the compressed fit", call. = FALSE)
  dup_threads <- lapply(c(2L, 4L, 8L), function(threads) fit_dup(list(patterns = "compress", estep_threads = threads)))
  close_fit(plain$dup$result, dup_threads[[1]]$result)
  if (!same_bits(pack_fit(dup_threads[[2]]), pack_fit(dup_threads[[1]])) ||
      !same_bits(pack_fit(dup_threads[[3]]), pack_fit(dup_threads[[1]]))) {
    stop("threaded compress with repeated patterns disagrees", call. = FALSE)
  }
  lta_threads <- lapply(c(2L, 4L, 8L), function(threads) {
    fit_lta_fix(list(patterns = "compress", estep_threads = threads))
  })
  close_fit(plain$lta$result, lta_threads[[1]]$result)
  if (!same_bits(pack_fit(lta_threads[[2]]), pack_fit(lta_threads[[1]])) ||
      !same_bits(pack_fit(lta_threads[[3]]), pack_fit(lta_threads[[1]]))) {
    stop("multi-wave threaded compress disagrees", call. = FALSE)
  }
  omit_threads <- lapply(c(2L, 4L, 8L), function(threads) {
    fit_dup(list(patterns = "compress", estep_threads = threads), posteriors = FALSE, estep = "streaming")
  })
  for (fit in omit_threads) {
    for (field in c("gamma", "filtered", "xi")) {
      if (!identical(fit$result[[field]], numeric(0))) stop("threaded omitted posterior is not empty", call. = FALSE)
    }
  }
  close_fit(plain$dup$result, omit_threads[[1]]$result, FALSE)
  if (!same_bits(pack_fit(omit_threads[[2]], FALSE), pack_fit(omit_threads[[1]], FALSE)) ||
      !same_bits(pack_fit(omit_threads[[3]], FALSE), pack_fit(omit_threads[[1]], FALSE))) {
    stop("threaded omitted streaming disagrees", call. = FALSE)
  }
  stream_threads <- lapply(c(2L, 4L, 8L), function(threads) {
    fit_wide(list(patterns = "compress", estep_threads = threads), estep = "streaming")
  })
  close_fit(plain$wide$result, stream_threads[[1]]$result)
  if (!same_bits(pack_fit(stream_threads[[2]]), pack_fit(stream_threads[[1]])) ||
      !same_bits(pack_fit(stream_threads[[3]]), pack_fit(stream_threads[[1]]))) {
    stop("threaded streaming compress disagrees", call. = FALSE)
  }
  blrt_frame <- function() {
    # Same separated repeated patterns as the Python harness. The observed
    # fits have to be usable so the bootstrap runs. B stays at 2.
    data.frame(
      u = c(rep(0, 16), rep(1, 16), 0, 1, 0),
      v = c(rep(0, 16), rep(1, 16), 1, 0, NA_real_)
    )
  }
  blrt_of <- function(mode) {
    data <- blrt_frame()
    controls <- list(
      patterns = mode, n_starts = 4L, n_full = 2L, screen_iter = 0L,
      max_iter = 40L, seed = 23L, n_threads = 1L
    )
    h0 <- do.call(profylr::fit_lca, c(list(data, c("u", "v"), n_states = 1L), controls))
    h1 <- do.call(profylr::fit_lca, c(list(data, c("u", "v"), n_states = 2L), controls))
    profylr::blrt_check(h0, h1, B = 2L, max_retries = 1L, n_threads = 2L,
                        experimental = TRUE, max_iter = 40L, seed = 23)
  }
  blrt_compress <- blrt_of("compress")
  blrt_plain <- blrt_of("none")
  for (field in c("b", "f", "B", "n_ok", "ran", "lr_observed", "p")) {
    if (is.null(blrt_compress[[field]]) || is.null(blrt_plain[[field]])) {
      stop("BLRT missing ", field, call. = FALSE)
    }
  }
  if (!isTRUE(blrt_compress$ran) || !isTRUE(blrt_plain$ran)) stop("BLRT did not run", call. = FALSE)
  if (as.integer(blrt_compress$n_ok) < 1L || as.integer(blrt_plain$n_ok) < 1L) {
    stop("BLRT kept no draws", call. = FALSE)
  }
  if (!is.finite(blrt_compress$lr_observed) || !is.finite(blrt_plain$lr_observed)) {
    stop("BLRT lr_observed is not finite", call. = FALSE)
  }
  if (!is.finite(blrt_compress$p) || !is.finite(blrt_plain$p)) stop("BLRT p is not finite", call. = FALSE)
  for (field in c("b", "f", "B", "n_ok", "ran")) {
    if (!identical(blrt_compress[[field]], blrt_plain[[field]])) stop("BLRT field ", field, call. = FALSE)
  }
  close_numeric(blrt_compress$lr_observed, blrt_plain$lr_observed, "lr_observed")
  close_numeric(blrt_compress$p, blrt_plain$p, "blrt p")
  cover_blocked()
  wide <- lapply(c(1L, 2L, 4L, 8L), function(threads) fit_wide(list(patterns = "compress", estep_threads = threads)))
  names(wide) <- c("1", "2", "4", "8")
  n_patterns <- as.integer(wide[["1"]]$result$n_patterns)
  if (n_patterns < 513L || n_patterns %% 32L == 0L) stop("wide fixture does not cross distinct pattern blocks", call. = FALSE)
  nblocks <- (n_patterns + 31L) %/% 32L
  assignment <- function(nthreads) {
    spans <- character()
    cursor <- 0L
    left_blocks <- nblocks
    left_threads <- min(nthreads, nblocks)
    while (left_threads > 0L) {
      take <- left_blocks %/% left_threads
      spans <- c(spans, paste0(cursor, ":", cursor + take))
      cursor <- cursor + take
      left_blocks <- left_blocks - take
      left_threads <- left_threads - 1L
    }
    paste(spans, collapse = ",")
  }
  if (length(unique(c(assignment(2L), assignment(4L), assignment(8L)))) != 3L) {
    stop("2/4/8 threads share a pattern-block assignment", call. = FALSE)
  }
  close_fit(plain$wide$result, wide[["1"]]$result)
  for (threads in c("4", "8")) {
    if (!same_bits(pack_fit(wide[[threads]]), pack_fit(wide[["2"]]))) {
      stop("compressed thread counts disagree ", threads, call. = FALSE)
    }
  }
  raised <- tryCatch({
    profylr::fit_lpa(frames()$gauss, "y", n_states = 2L, covariance = "EEI", patterns = "compress",
                     n_starts = 1L, max_iter = 2L, screen_iter = 0L)
    FALSE
  }, error = function(e) grepl("categorical models only", conditionMessage(e), fixed = TRUE))
  if (!isTRUE(raised)) stop("gaussian compress was accepted", call. = FALSE)
  mixed_raised <- tryCatch({
    profylr::fit_lta(frames()$mixed, c("u", "y"), "id", "wave", n_states = 2L, patterns = "compress",
                     n_starts = 1L, max_iter = 2L, screen_iter = 0L)
    FALSE
  }, error = function(e) grepl("categorical models only", conditionMessage(e), fixed = TRUE))
  if (!isTRUE(mixed_raised)) stop("mixed compress was accepted", call. = FALSE)
}
