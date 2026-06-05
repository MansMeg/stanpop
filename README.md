# stanpop

<!-- badges: start -->
[![R-CMD-check](https://github.com/MansMeg/stanpop/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MansMeg/stanpop/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/MansMeg/stanpop/actions/workflows/R-CMD-check-stan.yaml/badge.svg)](https://github.com/MansMeg/stanpop/actions/workflows/R-CMD-check-stan.yaml)
[![Codecov test coverage](https://codecov.io/gh/MansMeg/stanpop/graph/badge.svg)](https://app.codecov.io/gh/MansMeg/stanpop)
<!-- badges: end -->

`stanpop` is an R package for Bayesian poll-of-polls models written in Stan.
It turns polling data into a reusable `polls_data` object, builds the Stan data
for a selected model, runs inference through RStan or CmdStanR, and returns a
`poll_of_polls` object with draws, diagnostics, plots, metadata, and save/load
helpers.

The package currently ships model families under `inst/stan_models/`, curated
polling/election data for Sweden, Germany, and Spain, and regression tests for
the data builders, model fits, backend behavior, serialization, diagnostics,
and warm-started refits.

## Start here

Most workflows have four steps:

1. Put polling observations into a `polls_data` object.
2. Choose parties, a Stan model, a time scale, known states, and model options.
3. Fit with `poll_of_polls()`.
4. Inspect the latent state, diagnostics, predictions, plots, or serialized fit.

The important object types are:

- `polls_data`: validated poll shares plus house, publication date, fieldwork
  dates, sample size, and poll id.
- `stan_polls_data`: the model-specific Stan input created from `polls_data`.
- `poll_of_polls`: the fitted model object returned by `poll_of_polls()`.
- `latent_state`: extracted posterior draws for party support over time.

## Installation

`stanpop` requires R 4.1 or newer and a working Stan toolchain. RStan is the
default backend; CmdStanR is also supported.

```r
install.packages("remotes")
remotes::install_github("MansMeg/stanpop", dependencies = TRUE)
```

CmdStanR users also need CmdStan installed:

```r
cmdstanr::install_cmdstan()
```

If Stan packages need the Stan repository, use:

```r
options(repos = c(
  mcstan = "https://mc-stan.org/r-packages/",
  CRAN = "https://cloud.r-project.org"
))
```

## Quick example

This is a small smoke-test style fit using the bundled test data. It is meant
for checking that the package and Stan backend work, not for substantive
analysis.

```r
library(stanpop)
library(tibble)
library(lubridate)

data("x_test")
data("pd_test")

truth <- as.data.frame(x_test[3:4])
names(truth) <- c("x3", "x4")

polls <- simulate_polls(
  x = truth,
  pd = pd_test,
  npolls = 40,
  time_scale = "week",
  start_date = "2010-01-01"
)

known_idx <- c(44, 72)
known_state <- cbind(
  tibble(date = as.Date("2010-01-01") + weeks(known_idx - 1)),
  truth[known_idx, ]
)

cfg <- list(
  sigma_kappa_hyper = 0.03,
  use_industry_bias = 1L,
  use_house_bias = 0L,
  use_design_effects = 0L,
  use_multivariate_version = 2L,
  use_softmax = 1L
)

pop <- poll_of_polls(
  y = c("x3", "x4"),
  model = "model8k5",
  polls_data = polls,
  time_scale = "week",
  known_state = known_state,
  hyper_parameters = cfg,
  iter = 20,
  warmup = 10,
  chains = 1,
  refresh = 0,
  seed = 4711,
  cache_dir = NULL
)

print(pop)
latent <- latent_state(pop)
plot_poll_of_polls(pop, y = "x3", collection_period = TRUE)
```

For CmdStanR, pass CmdStanR sampling arguments instead of RStan arguments:

```r
pop <- poll_of_polls(
  y = c("x3", "x4"),
  model = "model8k5",
  polls_data = polls,
  time_scale = "week",
  known_state = known_state,
  hyper_parameters = cfg,
  backend = "cmdstanr",
  iter_warmup = 10,
  iter_sampling = 10,
  chains = 1,
  parallel_chains = 1,
  refresh = 0,
  seed = 4711,
  cache_dir = NULL
)
```

## Working with real polls

Use `polls_data()` when bringing in a new poll table:

```r
pd <- polls_data(
  y = your_polls[, c("party_a", "party_b", "party_c")],
  house = factor(your_polls$house),
  publish_date = as.Date(your_polls$publish_date),
  start_date = as.Date(your_polls$start_date),
  end_date = as.Date(your_polls$end_date),
  n = your_polls$n,
  poll_id = your_polls$poll_id
)
```

The validator expects:

- party columns to be named numeric proportions, usually on the 0-1 scale;
- one house, publication date, start date, end date, and sample size per poll;
- publication dates on or after collection end dates;
- collection end dates on or after collection start dates;
- unique poll ids.

`known_state` is a data frame with a `date` column and one column per modeled
party. Election results are the usual known state.

`time_scale` can be `"day"`, `"week"`, `"month"`, or `"year"` (but only week has been rigorously tested). 
Models that use `step_scale_t`, such as e.g. `model8k5`, can also use `time_scale_overrides` to fit
a mostly coarse time grid with selected daily windows.

## Common tasks

```r
# Extract posterior draws from a fitted model
draws <- extract(pop, pars = "x_pred")

# Extract latent party support over time
state <- latent_state(pop)
state_on_dates <- get_latent_state_for_dates(pop, as.Date(c("2010-05-08")))

# Inspect diagnostics
get_model_diagnostics(pop)
Rhat_is_above_m(pop, m = 1.1)
Rhat_is_NA_by_parameter_block(pop)

# Save and reload a self-describing stanpop object
save_pop(pop, "fit.rds", overwrite = TRUE)
pop <- load_pop("fit.rds")

# Refit from an existing object, reusing stored arguments and warm-start state
pop2 <- refit_poll_of_polls(pop, polls_data = updated_polls)
```

## Repository map

- `R/`: package functions, S3 methods, Stan data builders, diagnostics,
  plotting, backend adapters, save/load, and refit helpers.
- `inst/stan_models/`: bundled Stan models. The current checked-in model files
  include `model8k1`-`model8k9` and `model8m1`-`model8m10`.
- `data/`: packaged `.rda` data, including curated poll and election datasets.
- `data-raw/`: scripts and source files used to create package data.
- `tests/testthat/`: unit tests plus optional Stan integration and backend
  smoke tests.
- `man/`: generated Rd files from roxygen comments.

## Model and backend notes

- `backend = "rstan"` is the default and accepts RStan arguments such as
  `iter`, `warmup`, `chains`, `cores`, `control`, and `refresh`.
- `backend = "cmdstanr"` accepts CmdStanR `$sample()` arguments such as
  `iter_warmup`, `iter_sampling`, `chains`, `parallel_chains`, `adapt_delta`,
  and `refresh`. RStan-only argument names are rejected for this backend.
- `compile_args` is passed to `cmdstanr::cmdstan_model()` and ignored for
  RStan.
- `poll_of_polls()` caches fits by a hash of the model code, data, package
  version, backend, compile arguments, and sampler arguments. Set
  `cache_dir = NULL` to disable caching.
- `save_pop()` writes a wrapped payload with format metadata. Use `load_pop()`
  rather than `readRDS()` so the file format is checked.
- CmdStanR-backed fits are materialized before saving so reloaded objects do
  not depend on temporary CmdStan output files.

Model file names use the convention documented in `inst/stan_models/README.md`:
the number is the major model family, the letter is the minor model branch, and
the final number is the patch-level variant.

## Testing

Fast tests run by default:

```r
devtools::test()
```

Stan integration tests are opt-in because they compile models and run short
sampling jobs:

```bash
STANPOP_RUN_STAN_TESTS=true R -q -e 'devtools::test()'
```

CmdStanR-specific integration tests are separately gated:

```bash
STANPOP_RUN_STAN_TESTS=true \
STANPOP_RUN_CMDSTANR_TESTS=true \
R -q -e 'devtools::test()'
```

Some heavier RStan model-family checks are also opt-in:

```bash
STANPOP_RUN_STAN_TESTS=true \
STANPOP_RUN_8K_RSTAN_TESTS=true \
R -q -e 'devtools::test()'
```

Before merging for production, a good minimum is:

```bash
R CMD check .
R -q -e 'devtools::test()'
```

Run the Stan-gated tests when model code, Stan data construction, backend
handling, serialization, or refit behavior changed.

## Reproducibility

Every fitted `poll_of_polls` object records the package version, selected model,
backend, Stan code, fit timestamp, Git SHA when available, input arguments,
sampling arguments, model data, diagnostics, and a cache hash. Use explicit
seeds and save fits with `save_pop()` when results need to be shared or
reloaded later.
