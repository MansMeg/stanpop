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

test_that("translate_rstan_to_cmdstanr_sample maps core arguments", {
  stan_file <- tempfile(fileext = ".stan")
  writeLines("parameters { real y; } model { y ~ normal(0, 1); }", stan_file)

  translated <- translate_rstan_to_cmdstanr_sample(
    list(
      file = stan_file,
      data = list(N = 10L),
      chains = 2L,
      cores = 2L,
      iter = 100L,
      warmup = 40L,
      thin = 2L,
      seed = 4711,
      init = 0,
      refresh = 5L,
      control = list(
        adapt_delta = 0.95,
        max_treedepth = 12L,
        stepsize = 0.2,
        metric = "diag_e",
        adapt_engaged = FALSE
      )
    )
  )

  expect_true(is.list(translated))
  expect_identical(names(translated), c("stan_file", "sample_args"))
  expect_identical(translated$stan_file, stan_file)
  expect_identical(translated$sample_args$chains, 2L)
  expect_identical(translated$sample_args$parallel_chains, 2L)
  expect_identical(translated$sample_args$iter_warmup, 40L)
  expect_identical(translated$sample_args$iter_sampling, 60L)
  expect_identical(translated$sample_args$thin, 2L)
  expect_identical(translated$sample_args$seed, 4711)
  expect_identical(translated$sample_args$init, 0)
  expect_identical(translated$sample_args$refresh, 5L)
  expect_identical(translated$sample_args$adapt_delta, 0.95)
  expect_identical(translated$sample_args$max_treedepth, 12L)
  expect_identical(translated$sample_args$step_size, 0.2)
  expect_identical(translated$sample_args$metric, "diag_e")
  expect_identical(translated$sample_args$adapt_engaged, FALSE)
})

test_that("translate_rstan_to_cmdstanr_sample disables adaptation when warmup is zero", {
  stan_file <- tempfile(fileext = ".stan")
  writeLines("parameters { real y; } model { y ~ normal(0, 1); }", stan_file)

  translated <- translate_rstan_to_cmdstanr_sample(
    list(
      file = stan_file,
      data = list(),
      chains = 1L,
      iter = 3L,
      warmup = 0L,
      refresh = 0L
    )
  )

  expect_identical(translated$sample_args$iter_warmup, 0L)
  expect_identical(translated$sample_args$iter_sampling, 3L)
  expect_identical(translated$sample_args$adapt_engaged, FALSE)
})
