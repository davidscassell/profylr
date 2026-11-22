# profylr 0.1.0

## 0.1.0

* A fit with fewer analyzed subjects than `n_states` stops before the
  core runs. One observed row cannot identify two states.
* Depends is `R (>= 4.3.0)`. The package was checked on R 4.3.3.
* `-ffp-contract=off` is written by `configure` / `configure.win` when
  the compiler accepts it. The shipped Makevars template does not
  contain that flag. Configuration fails if the compiler rejects it.
* The best-log-likelihood replication check stays
  `abs(ll - best) <= replicate_tol`. The default `replicate_tol` is now
  `0.1`, chosen to be comparable to the approximately 0.1 tolerance
  Muthén described on the Mplus discussion board (2018); Mplus does not
  document an exact value (User's Guide v8 pp. 465-466;
  https://statmodel.com/download/StartsUpdate.pdf;
  https://www.statmodel.com/discussion/messages/13/6319.html).
  It does not scale with N or with the log-likelihood.
  `logliks_replicate()` applies the same absolute test.
  `B`, the start schedule, `max_iter`, `tol`, and the compiler flags
  are unchanged.
