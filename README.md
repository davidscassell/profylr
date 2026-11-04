# profylr

Latent profile, latent class, and latent transition models for R. Release 1 uses a C++ EM core that is vendored in this package. The same core is wrapped by [pyfylr](https://github.com/davidscassell/pyfylr).

**BLRT is experimental and unvalidated.** `selection_rule()` keeps `blrt = FALSE`, and a fitted model keeps `validated = FALSE`. Do not use the bootstrap likelihood-ratio test for automatic model selection.

## Install

This repository is the package. Eigen 3.4.0 is included under `inst/include`. From a checkout, with R 4.3.3 or newer and a C++17 compiler:

```bash
R CMD INSTALL .
```

Release 1 supports Linux x86_64 and Apple Silicon. Windows and Intel Mac are deferred and are not supported release targets.

The defaults are 40 random starts, a screen of 10, 4 full starts, `max_iter = 200`, and `tol = 1e-7`.

## Example

```r
library(profylr)

set.seed(1)
ex <- data.frame(
  y1 = c(rnorm(30, -1), rnorm(30, 1)),
  y2 = c(rnorm(30, -1), rnorm(30, 1))
)
fit <- fit_lpa(ex, c("y1", "y2"), n_states = 2, covariance = "EEI")
glance(fit)
```

A longitudinal call has the same defaults:

```r
fit_lta(data, c("u1", "y"), id = "id", wave = "wave", n_states = 2,
        family = list(u1 = "bernoulli", y = "gaussian"), covariance = "VVV")
```

Indicators, covariates, and distal outcomes are separate roles. Covariates are refused in this release. Distal outcomes are Release 3 and require a timing: `initial`, `final`, `wave`, or `transition`. `estimate_distal()` checks that declaration and stops. Modal-assignment ANOVA and logistic regression are not provided.

Ordinal indicators are Release 2. Poisson and negative binomial indicators come later. A mixed model is one Gaussian block times locally independent categorical indicators.

The fit has `engine = "cpp-em"`, `draws = NULL`, and `interval_type = "none"`. State posteriors are plug-in quantities and are not parameter uncertainty.

This release does not score a new sample from a fitted model. `augment`, `posterior_states`, and `decode_states` describe the fitted sample. `prepare_panel(..., schema =)` reuses the training category coding and does not apply fitted parameters. The Python estimators `predict` and `predict_proba` do score new rows. That language difference is accepted here; an R prediction method for new data is deferred. Python fitted-model persistence is also deferred: `save_model` there stores a selection or a comparison table, not a fitted model.

Repository: <https://github.com/davidscassell/profylr>
