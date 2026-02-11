skip_if_no_stan_tests <- function(env_var = "STANPOP_RUN_STAN_TESTS") {
  if (!identical(Sys.getenv(env_var), "true")) {
    testthat::skip(paste0(
      "Stan integration tests are disabled (set ", env_var, "=true)."
    ))
  }
}

