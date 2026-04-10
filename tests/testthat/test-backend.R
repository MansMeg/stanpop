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

test_that("backend_get_sampler_state preserves dense inverse metrics for cmdstanr-like fits", {
  backend_get_sampler_state <- get_internal("backend_get_sampler_state")
  backend_get_adaptation_info <- get_internal("backend_get_adaptation_info")

  dense_metric <- matrix(c(1.0, 0.25, 0.25, 2.0), nrow = 2, byrow = TRUE)
  mock_fit <- list(
    inv_metric = function(matrix = TRUE) {
      list(dense_metric)
    },
    sampler_diagnostics = function(inc_warmup = FALSE, format = "draws_array") {
      out <- array(
        c(0.125, 0.125, 0.125),
        dim = c(3, 1, 1),
        dimnames = list(NULL, NULL, "stepsize__")
      )
      out
    }
  )

  state <- backend_get_sampler_state("cmdstanr", mock_fit)

  expect_length(state, 1)
  expect_named(state[[1]], c("adaption_terminated", "step_size", "inv_metric", "metric_type"))
  expect_identical(state[[1]]$metric_type, "dense_e")
  expect_equal(state[[1]]$inv_metric, dense_metric, tolerance = 0)
  expect_equal(state[[1]]$step_size, 0.125, tolerance = 0)

  ai <- backend_get_adaptation_info("cmdstanr", mock_fit)
  expect_equal(ai[[1]]$diag_inv_mass_matrix, diag(dense_metric), tolerance = 0)
})

test_that("backend_get_sampler_state with rstan returns reusable sampler state", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  backend_get_sampler_state <- get_internal("backend_get_sampler_state")
  backend_sample <- get_internal("backend_sample")
  fit <- suppressWarnings(
    backend_sample(
      backend = "rstan",
      sample_arguments = list(
        file = sampler_state_test_stan_file(),
        data = list(),
        iter = 10,
        warmup = 5,
        chains = 1,
        seed = 4711,
        refresh = 0
      )
    )
  )

  state <- backend_get_sampler_state("rstan", fit)

  expect_length(state, 1)
  expect_named(state[[1]], c("adaption_terminated", "step_size", "inv_metric", "metric_type"))
  expect_true(isTRUE(state[[1]]$adaption_terminated))
  expect_true(is.finite(state[[1]]$step_size))
  expect_identical(state[[1]]$metric_type, "diag_e")
  expect_type(state[[1]]$inv_metric, "double")
  expect_length(state[[1]]$inv_metric, rstan::get_num_upars(fit))
})

test_that("backend_get_sampler_state with cmdstanr returns reusable sampler state", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()

  backend_get_sampler_state <- get_internal("backend_get_sampler_state")
  backend_sample <- get_internal("backend_sample")
  fit <- backend_sample(
    backend = "cmdstanr",
    sample_arguments = list(
      data = list(),
      chains = 1,
      parallel_chains = 1,
      iter_warmup = 5,
      iter_sampling = 5,
      seed = 4711,
      refresh = 0,
      show_messages = FALSE,
      show_exceptions = FALSE
    ),
    stan_file = sampler_state_test_stan_file()
  )

  state <- backend_get_sampler_state("cmdstanr", fit)

  expect_length(state, 1)
  expect_named(state[[1]], c("adaption_terminated", "step_size", "inv_metric", "metric_type"))
  expect_true(is.na(state[[1]]$adaption_terminated))
  expect_true(is.finite(state[[1]]$step_size))
  expect_identical(state[[1]]$metric_type, "diag_e")
  expect_type(state[[1]]$inv_metric, "double")
  expect_length(state[[1]]$inv_metric, 2)
})
