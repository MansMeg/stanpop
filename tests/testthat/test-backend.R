context("backend")

test_that("backend_sample with rstan returns a stanfit", {
  skip_if_no_stan_tests()
  fit <- backend_sample(
    backend = "rstan",
    stan_arguments = list(
      model_code = "parameters { real y; } model { y ~ normal(0, 1); }",
      data = list(),
      iter = 1,
      warmup = 0,
      chains = 1,
      refresh = 0
    )
  )
  expect_s4_class(fit, "stanfit")
})
