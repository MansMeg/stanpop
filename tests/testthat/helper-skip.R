skip_if_no_stan_tests <- function(env_var = "STANPOP_RUN_STAN_TESTS") {
  if (!identical(Sys.getenv(env_var), "true")) {
    testthat::skip(paste0(
      "Stan integration tests are disabled (set ", env_var, "=true)."
    ))
  }
}

skip_if_no_cmdstanr <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    testthat::skip("Package 'cmdstanr' is not installed.")
  }
  ok <- tryCatch({
    !is.null(cmdstanr::cmdstan_path()) &&
      nzchar(cmdstanr::cmdstan_path()) &&
      !is.na(cmdstanr::cmdstan_version())
  }, error = function(e) FALSE)
  if (!ok) {
    testthat::skip("CmdStan is not configured for cmdstanr.")
  }
}

assert_rstan_available <- function() {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' must be installed for mixed backend tests.", call. = FALSE)
  }
}

skip_if_no_cmdstanr_tests <- function(
  env_var = "STANPOP_RUN_CMDSTANR_TESTS"
) {
  if (!identical(Sys.getenv(env_var), "true")) {
    testthat::skip(paste0(
      "CmdStanR integration tests are disabled (set ",
      env_var,
      "=true)."
    ))
  }
}

skip_if_no_cmdstanr_model_methods_tests <- skip_if_no_cmdstanr_tests

skip_if_no_rstan_tests <- function(env_var = "STANPOP_RUN_RSTAN_TESTS") {
  if (!identical(Sys.getenv(env_var, unset = "true"), "true")) {
    testthat::skip(paste0(
      "RStan integration tests are disabled (set ",
      env_var,
      "=true)."
    ))
  }
}

skip_if_no_8k_rstan_tests <- function(
  env_var = "STANPOP_RUN_8K_RSTAN_TESTS"
) {
  if (!identical(Sys.getenv(env_var, unset = "false"), "true")) {
    testthat::skip(paste0(
      "Model 8k RStan integration tests are disabled (set ",
      env_var,
      "=true)."
    ))
  }
}
