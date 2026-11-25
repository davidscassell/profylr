## Resubmission

profylr remains version 0.1.0. The r-devel-windows incoming pretest
rejected the previous submission. All 10 failures were in
`tests/testthat/test-persistence.R`, in the fresh-process round trip.
The child `Rscript` exited with status 5 and did not print
`fresh-process identical`. Debian passed.

This resubmission fixes that Windows test harness. `system2(env = ...)`
places `R_LIBS=...` between `Rscript` and its arguments, so `Rscript`
treats the assignment as the script file. The child script now calls
`.libPaths()` with the parent's library paths before `library(profylr)`.
Paths in that script use `normalizePath(..., winslash = "/")` and
`dput()`. The script path passed to `system2` is quoted with `shQuote`.
`R_TESTS` is unset only while the child runs, then restored. A failure
prints the status, the captured output, and the warnings.

Since the rejected upload, a fit with fewer analyzed subjects than
`n_states` is refused with an error before the core runs. One observed
row cannot identify two states. That behavior was not in the rejected
tarball.

## Test environments

GitHub Actions run
https://github.com/davidscassell/profilr-dev-ci/actions/runs/45006258857
used `R CMD check --as-cran --no-manual`. Each of these three jobs
finished with Status: 2 NOTEs, 0 ERROR, 0 WARNING. The notes are a new
submission and missing pandoc, so `README.md` and `NEWS.md` were not
converted.

* Windows Server 2022, R release 4.6.2 (2026-10-29 ucrt), gcc 14.3.0.
* Windows Server 2022, R devel r90926 (2026-11-17 ucrt).
* Ubuntu, R release 4.6.2 (2026-10-29).

Earlier local check: Ubuntu 24.04, R 4.3.3, g++ 13.3.0, with `lavaan`
and `semTools` installed. That run was Status: 6 NOTEs, 0 ERROR,
0 WARNING. Notes that come from the machine, not from the package:

* New submission.

* `checking for future file timestamps`: the machine could not verify
  the current time.

* `checking top-level files`: `pandoc` was not installed, so `README.md`
  and `NEWS.md` were not converted.

* `checking compilation flags used`: `-mno-omit-leaf-frame-pointer` is
  part of this R build's own `CXXFLAGS`. The package does not set it.

* `checking HTML version of manual`: `tidy` was not installed.

* `checking installed package size`: the shared library is about 29Mb
  because R compiles with `-g` and `lta.cpp` is one large translation
  unit. Its text segment is under 1Mb; the rest of the file is debug
  info. `inst/include` is about 6.8Mb because Eigen 3.4.0 is vendored
  there and is required to compile. Debug info was not stripped, and
  the Eigen headers were not pruned.

`-ffp-contract=off` is required so fused multiply-adds do not change
the likelihood. `configure` tests the compiler and writes the flag into
the generated Makevars. The shipped `Makevars.in` does not contain it.
A compiler that rejects the flag fails configuration.
