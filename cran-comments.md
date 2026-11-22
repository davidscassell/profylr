## Test environments

* Local Ubuntu 24.04, R 4.3.3, g++ 13.3.0

## R CMD check --as-cran

Checked with `lavaan` and `semTools` installed. Result: `Status: 6 NOTEs`.
No ERROR and no WARNING. Notes below that come from the machine, not
from the package, are listed here.

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
