# stanpop

<!-- badges: start -->
  [![R-CMD-check](https://github.com/MansMeg/stanpop/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MansMeg/stanpop/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**stanpop** is an R package that fits Bayesian *poll-of-polls* models using Stan.  
It provides a clean, reusable API for preparing polling data, building Stan inputs, running inference, and extracting diagnostics and outputs.

## What’s in this repo?

- Stan model definitions (poll-of-polls variants)
- R code for:
  - validating and standardizing polling datasets
  - building model-specific Stan data lists
  - fitting models (Stan backend)
  - extracting latent trends, house effects, posterior predictive checks
  - diagnostics and generic plots/exports

## Repository structure

- `R/` — package functions
- `inst/stan_models/` — Stan model files (`.stan`)
- `tests/testthat/` — unit + integration tests

## Installation

Development install:

```r
remotes::install_github("mansmeg/stanpop")
```

## Quick run 

TODO


## Testing

We keep tests fast by default.

**Unit tests** (always run): validation, indexing, Stan data builders, deterministic helpers
**Stan integration tests** (optional): short sampling runs on toy data

Run unit tests:

```r
devtools::test()
```

Run integration tests as well:

```bash
ADA2_RUN_STAN_TESTS=true R -q -e 'devtools::test()'
```

## Versioning & reproducibility

Every fit records:

* `stanpop` version
* model identifier / Stan file hash
* data hash
* run timestamp and sampling config






