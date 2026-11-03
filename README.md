# profylr

Latent profile, latent class, and latent transition models for R. Release 1 uses the shared C++ EM core, also wrapped by pyfylr.

```r
library(profylr)
fit <- fit_lta(data, c("u1", "y"), id = "id", wave = "wave", n_states = 2,
               family = list(u1 = "bernoulli", y = "gaussian"), covariance = "VVV")
glance(fit)
tidy(fit)
augment(fit)
plot_profiles(fit)
```

Indicators, covariates, and distal outcomes are separate roles. Covariates are refused in this release. Distal outcomes are Release 3 and require a timing: `initial`, `final`, `wave`, or `transition`. `estimate_distal()` checks that declaration and stops. Modal-assignment ANOVA and logistic regression are not provided.

Ordinal indicators are Release 2. Poisson and negative binomial indicators come later. A mixed model is one Gaussian block times locally independent categorical indicators.

The fit has `engine = "cpp-em"`, `draws = NULL`, and `interval_type = "none"`. State posteriors are plug-in quantities and are not parameter uncertainty.

This release does not score a new sample from a fitted model. `augment`, `posterior_states`, and `decode_states` describe the fitted sample. `prepare_panel(..., schema =)` reuses the training category coding and does not apply fitted parameters. The Python estimators `predict` and `predict_proba` do score new rows. That language difference is accepted here; an R prediction method for new data is deferred. Python fitted-model persistence is also deferred: `save_model` there stores a selection or a comparison table, not a fitted model.

Requires R 4.3.3 or newer. Release 1 supports Linux x86_64 and Apple Silicon. Windows and Intel Mac are deferred and are not supported release targets.

Build from the monorepo with `R CMD INSTALL profylr`. Eigen 3.4.0 is vendored in the package. `tools/sync_core.sh` refreshes that copy from `core/`.

Repository: <https://github.com/davidscassell/profilr>
