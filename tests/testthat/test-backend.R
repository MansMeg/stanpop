context("backend")

test_that("backend_sample with rstan returns a stanfit", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()
  fit <- backend_sample(
    backend = "rstan",
    sample_arguments = list(
      file = sampler_state_test_stan_file(),
      data = list(),
      iter = 1,
      warmup = 0,
      chains = 1,
      refresh = 0
    )
  )
  expect_s4_class(fit, "stanfit")
})
