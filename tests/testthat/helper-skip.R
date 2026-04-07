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
