
# Helper to access non-exported functions in a controlled way
get_internal <- function(name, pkg = "stanpop") {
  getFromNamespace(name, pkg)
}

sampler_state_test_stan_file <- function() {
  testthat::test_path("stan_code", "sampler_state_test.stan")
}
