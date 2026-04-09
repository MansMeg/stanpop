#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  res <- list(mode = NULL)
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (identical(arg, "--mode")) {
      i <- i + 1L
      if (i > length(args)) {
        stop("Missing value for --mode", call. = FALSE)
      }
      res$mode <- args[[i]]
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1L
  }
  res
}

parsed <- parse_args(args)
mode <- parsed$mode

if (is.null(mode) || !mode %in% c("cmdstanr_only", "mixed")) {
  stop("Use --mode with one of: cmdstanr_only, mixed", call. = FALSE)
}

suppressPackageStartupMessages({
  library(pkgload)
  if (identical(mode, "mixed")) {
    library(rstan)
  }
  library(cmdstanr)
})

pkgload::load_all(".", quiet = TRUE)
source("tests/testthat/helper-model8-mixed-smoke.R", local = globalenv())

case <- make_model8_mixed_smoke_case(npolls = 12)
cfg <- list(
  sigma_kappa_hyper = 0.03,
  use_industry_bias = 1L,
  use_house_bias = 0L,
  use_design_effects = 0L,
  use_multivariate_version = 2L,
  use_softmax = 1L
)

fit_args <- list(
  y = case$parties,
  model = "model8k5",
  polls_data = case$polls_data,
  time_scale = case$time_scale,
  time_scale_overrides = case$time_scale_overrides,
  known_state = case$known_state,
  hyper_parameters = cfg,
  chains = 1,
  refresh = 0,
  seed = 4711,
  cache_dir = NULL
)

run_diagnostic <- function(mode, fit_args) {
  cat("MODE:", mode, "\n")

  if (identical(mode, "mixed")) {
    cat("STEP: rstan fit\n")
    pop_rstan <- suppressWarnings(
      suppressMessages(
        do.call(
          poll_of_polls,
          c(fit_args, list(
            backend = "rstan",
            iter = 10,
            warmup = 5
          ))
        )
      )
    )
    nu_rstan <- get_num_upars(pop_rstan)
    lp_rstan <- log_prob(pop_rstan, rep(0, nu_rstan))
    stopifnot(is.finite(lp_rstan))
    cat("INFO: rstan log_prob at zero =", format(lp_rstan, digits = 16), "\n")
  }

  cat("STEP: cmdstanr fit with model methods\n")
  pop_cmdstanr <- suppressWarnings(
    suppressMessages(
      do.call(
        poll_of_polls,
        c(fit_args, list(
          backend = "cmdstanr",
          iter_sampling = 5,
          iter_warmup = 5,
          compile_args = list(compile_model_methods = TRUE)
        ))
      )
    )
  )
  nu_cmdstanr <- get_num_upars(pop_cmdstanr)
  lp_cmdstanr <- log_prob(pop_cmdstanr, rep(0, nu_cmdstanr))
  stopifnot(is.finite(lp_cmdstanr))
  cat("INFO: cmdstanr log_prob at zero =", format(lp_cmdstanr, digits = 16), "\n")
  cat("RESULT: success\n")
}

tryCatch(
  run_diagnostic(mode, fit_args),
  error = function(e) {
    cat("RESULT: error\n")
    cat(conditionMessage(e), "\n")
    quit(save = "no", status = 1L)
  }
)
