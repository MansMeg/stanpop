#!/usr/bin/env Rscript

# Create compact smoke-test fixtures for both Stan backends.
#
# Usage:
#   Rscript rpackage/tests/testthat/files/create-smoke_test_pops_v0_7_3.R [rstan_source] [cmdstanr_source]
#
# Defaults:
#   rstan_source    = tmp/pop_rstan.rds
#   cmdstanr_source = tmp/pop_cmdstanr.rds
#   outputs         = rpackage/tests/testthat/files/test_pop_v0_7_3_rstan.rds
#                     rpackage/tests/testthat/files/test_pop_v0_7_3_cmdstanr.rds

default_source_path <- function(preferred, fallback = NULL) {
  if (file.exists(preferred)) {
    return(preferred)
  }
  if (!is.null(fallback) && file.exists(fallback)) {
    return(fallback)
  }

  stop(
    "Could not find source file. Tried '", preferred, "'",
    if (!is.null(fallback)) paste0(" and '", fallback, "'") else "",
    ".",
    call. = FALSE
  )
}

thin_rstan_fit <- function(stan_fit, keep_draws = 10L, keep_chain = 1L) {
  keep_draws <- as.integer(keep_draws)
  keep_chain <- as.integer(keep_chain)

  stopifnot(inherits(stan_fit, "stanfit"))
  stopifnot(length(stan_fit@sim$samples) >= keep_chain)

  warmup_end <- stan_fit@sim$warmup2[[keep_chain]]
  sample_length <- length(stan_fit@sim$samples[[keep_chain]][[1L]])
  keep_start <- warmup_end + 1L
  keep_stop <- min(sample_length, keep_start + keep_draws - 1L)
  keep_idx <- keep_start:keep_stop

  stan_fit@sim$samples <- list(
    lapply(stan_fit@sim$samples[[keep_chain]], function(x) x[keep_idx])
  )
  stan_fit@sim$chains <- 1L
  stan_fit@sim$iter <- length(keep_idx)
  stan_fit@sim$warmup <- 0L
  stan_fit@sim$thin <- 1L
  stan_fit@sim$n_save <- length(keep_idx)
  stan_fit@sim$warmup2 <- 0L
  stan_fit@sim$permutation <- list(seq_along(keep_idx))

  stan_fit@stan_args <- stan_fit@stan_args[keep_chain]
  stan_fit@stan_args[[1L]]$iter <- length(keep_idx)
  stan_fit@stan_args[[1L]]$warmup <- 0L
  stan_fit@stan_args[[1L]]$thin <- 1L

  stan_fit
}

thin_cmdstanr_fit <- function(stan_fit, keep_draws = 10L, keep_chain = 1L) {
  keep_draws <- as.integer(keep_draws)
  keep_chain <- as.integer(keep_chain)

  stopifnot(inherits(stan_fit, "CmdStanMCMC"))
  if (!requireNamespace("posterior", quietly = TRUE)) {
    stop("Package 'posterior' is required to thin cmdstanr fixtures.", call. = FALSE)
  }

  private <- stan_fit$.__enclos_env__$private
  iteration_ids <- seq_len(min(keep_draws, dim(private$draws_)[1]))

  private$draws_ <- posterior::subset_draws(
    private$draws_,
    iteration = iteration_ids,
    chain = keep_chain
  )

  if (!is.null(private$sampler_diagnostics_)) {
    private$sampler_diagnostics_ <- posterior::subset_draws(
      private$sampler_diagnostics_,
      iteration = iteration_ids,
      chain = keep_chain
    )
  }

  private$warmup_draws_ <- NULL
  private$warmup_sampler_diagnostics_ <- NULL

  if (is.list(private$metadata_)) {
    private$metadata_$num_chains <- 1L
    private$metadata_$iter_sampling <- length(iteration_ids)
    private$metadata_$iter_warmup <- 0L
    private$metadata_$save_warmup <- 0L
    private$metadata_$thin <- 1L

    for (name in c("id", "init", "step_size_adaptation", "step_size")) {
      value <- private$metadata_[[name]]
      if (!is.null(value) && length(value) >= keep_chain) {
        private$metadata_[[name]] <- value[keep_chain]
      }
    }

    if (is.data.frame(private$metadata_$time) &&
        nrow(private$metadata_$time) >= keep_chain) {
      private$metadata_$time <- private$metadata_$time[keep_chain, , drop = FALSE]
    }
  }

  if (!is.null(private$return_codes_) && length(private$return_codes_) >= keep_chain) {
    private$return_codes_ <- private$return_codes_[keep_chain]
  }

  if (is.list(private$inv_metric_) && length(private$inv_metric_) >= keep_chain) {
    private$inv_metric_ <- private$inv_metric_[keep_chain]
  }

  if (is.list(private$profiles_) && length(private$profiles_) >= keep_chain) {
    private$profiles_ <- private$profiles_[keep_chain]
  }

  stan_fit
}

update_stan_arguments <- function(pop, keep_draws = 10L) {
  if (is.null(pop$stan_arguments)) {
    return(pop)
  }

  if ("chains" %in% names(pop$stan_arguments)) {
    pop$stan_arguments$chains <- 1L
  }
  if ("parallel_chains" %in% names(pop$stan_arguments)) {
    pop$stan_arguments$parallel_chains <- 1L
  }
  if ("warmup" %in% names(pop$stan_arguments)) {
    pop$stan_arguments$warmup <- 0L
  }
  if ("iter" %in% names(pop$stan_arguments)) {
    pop$stan_arguments$iter <- as.integer(keep_draws)
  }
  if ("thin" %in% names(pop$stan_arguments)) {
    pop$stan_arguments$thin <- 1L
  }
  if ("iter_warmup" %in% names(pop$stan_arguments)) {
    pop$stan_arguments$iter_warmup <- 0L
  }
  if ("iter_sampling" %in% names(pop$stan_arguments)) {
    pop$stan_arguments$iter_sampling <- as.integer(keep_draws)
  }

  pop
}

thin_pop_fixture <- function(pop, backend, keep_draws = 10L, keep_chain = 1L) {
  if (identical(backend, "rstan")) {
    pop$stan_fit <- thin_rstan_fit(pop$stan_fit, keep_draws = keep_draws, keep_chain = keep_chain)
  } else if (identical(backend, "cmdstanr")) {
    pop$stan_fit <- thin_cmdstanr_fit(pop$stan_fit, keep_draws = keep_draws, keep_chain = keep_chain)
  } else {
    stop("Unsupported backend '", backend, "'.", call. = FALSE)
  }

  pop$backend <- backend
  pop <- update_stan_arguments(pop, keep_draws = keep_draws)
}

write_fixture <- function(pop, output_path) {
  if (file.exists(output_path)) {
    file.remove(output_path)
  }

  stanpop::save_pop(pop, output_path, compress = "xz")
  size_mb <- round(file.info(output_path)$size / 1024^2, 2)
  message("Wrote ", output_path, " (", size_mb, " MB)")
}

build_fixture <- function(source_path, output_paths, backend, keep_draws = 10L, keep_chain = 1L) {
  message("Reading ", backend, " source pop from: ", source_path)
  pop <- stanpop::load_pop(source_path)
  pop <- thin_pop_fixture(pop, backend = backend, keep_draws = keep_draws, keep_chain = keep_chain)

  for (output_path in output_paths) {
    write_fixture(pop, output_path)
  }
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  rstan_source <- if (length(args) >= 1L) {
    args[[1L]]
  } else {
    default_source_path("tmp/pop_rstan.rds")
  }

  cmdstanr_source <- if (length(args) >= 2L) {
    args[[2L]]
  } else {
    default_source_path("tmp/pop_cmdstanr.rds")
  }

  build_fixture(
    source_path = rstan_source,
    output_paths = c(
      "rpackage/tests/testthat/files/test_pop_v0_7_3_rstan.rds"
    ),
    backend = "rstan"
  )

  build_fixture(
    source_path = cmdstanr_source,
    output_paths = c(
      "rpackage/tests/testthat/files/test_pop_v0_7_3_cmdstanr.rds"
    ),
    backend = "cmdstanr"
  )
}

if (sys.nframe() == 0L) {
  main()
}
