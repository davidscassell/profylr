\name{lca_spec}
\alias{lca_spec}
\title{lca spec}
\description{
  lca spec for a profylr fit. Release 1 estimates indicators only.
  Covariates are refused. Defaults, tolerances, and the bootstrap
  count are the values documented in the package specification.
}
\usage{
lca_spec(n_states, ...)
}
\arguments{
  \item{n_states}{Number of latent states.}
  \item{...}{Further specification arguments. See lta_spec.}
}
\value{
  A latent class specification.
}
