context("backend")

test_that("backend_relist_flat_draw_to_init reconstructs init objects from flat draws", {
  backend_relist_flat_draw_to_init <- get_internal("backend_relist_flat_draw_to_init")

  draw <- stats::setNames(
    c(0.1, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
    c("alpha", "beta[1]", "beta[2]", "gamma[1,1]", "gamma[2,1]", "gamma[1,2]", "gamma[2,2]")
  )
  skeleton <- list(
    alpha = 0,
    beta = numeric(2),
    gamma = matrix(0, nrow = 2, ncol = 2)
  )

  res <- backend_relist_flat_draw_to_init(draw, skeleton)

  expect_identical(res$alpha, 0.1)
  expect_equal(res$beta, c(1.0, 2.0), tolerance = 0)
  expect_equal(
    res$gamma,
    matrix(c(3.0, 4.0, 5.0, 6.0), nrow = 2, ncol = 2),
    tolerance = 0
  )
})

test_that("backend_relist_flat_draw_to_init ignores non-parameter extras in the draw", {
  backend_relist_flat_draw_to_init <- get_internal("backend_relist_flat_draw_to_init")

  draw <- stats::setNames(
    c(0.1, 1.0, 2.0, NA_real_),
    c("alpha", "beta[1]", "beta[2]", "lp__")
  )
  skeleton <- list(
    alpha = 0,
    beta = numeric(2)
  )

  res <- backend_relist_flat_draw_to_init(draw, skeleton)

  expect_identical(res$alpha, 0.1)
  expect_equal(res$beta, c(1.0, 2.0), tolerance = 0)
})

test_that("backend_build_init_skeleton_from_variable_names reconstructs parameter shapes", {
  backend_build_init_skeleton_from_variable_names <- get_internal("backend_build_init_skeleton_from_variable_names")

  skeleton <- backend_build_init_skeleton_from_variable_names(
    c(
      "alpha",
      "beta[1]", "beta[2]",
      "gamma[1,1]", "gamma[2,1]", "gamma[1,2]", "gamma[2,2]",
      "lp__", "stepsize__"
    )
  )

  expect_named(skeleton, c("alpha", "beta", "gamma"))
  expect_identical(skeleton$alpha, NA_real_)
  expect_equal(dim(skeleton$beta), 2L, tolerance = 0)
  expect_equal(dim(skeleton$gamma), c(2L, 2L), tolerance = 0)
})

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

test_that("backend_get_last_draws_for_init with rstan returns the final post-warmup draw", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  backend_sample <- get_internal("backend_sample")
  backend_get_last_draws_for_init <- get_internal("backend_get_last_draws_for_init")
  fit <- suppressWarnings(
    backend_sample(
      backend = "rstan",
      sample_arguments = list(
        file = sampler_state_constrained_test_stan_file(),
        data = list(),
        iter = 10,
        warmup = 5,
        chains = 1,
        seed = 4711,
        refresh = 0
      )
    )
  )

  res <- backend_get_last_draws_for_init("rstan", fit)
  draws <- rstan::extract(fit, permuted = FALSE, inc_warmup = FALSE)
  variable_names <- dimnames(draws)[[3]]
  expected_last <- as.numeric(draws[dim(draws)[1], 1, ])
  expected_first <- as.numeric(draws[1, 1, ])
  names(expected_last) <- variable_names
  names(expected_first) <- variable_names

  expect_length(res, 1)
  expect_named(res[[1]], c("sigma", "y"))
  expect_true(res[[1]]$sigma > 0)
  expect_equal(res[[1]]$sigma, unname(expected_last[["sigma"]]), tolerance = 0)
  expect_equal(
    as.numeric(res[[1]]$y),
    unname(expected_last[grepl("^y\\[", names(expected_last))]),
    tolerance = 0
  )
  expect_false(
    identical(
      unname(expected_first[c("sigma", "y[1]", "y[2]")]),
      c(res[[1]]$sigma, as.numeric(res[[1]]$y))
    )
  )
})

test_that("backend_get_last_draws_for_init output can be passed back to rstan as init", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  backend_sample <- get_internal("backend_sample")
  backend_get_last_draws_for_init <- get_internal("backend_get_last_draws_for_init")
  fit <- suppressWarnings(
    backend_sample(
      backend = "rstan",
      sample_arguments = list(
        file = sampler_state_constrained_test_stan_file(),
        data = list(),
        iter = 10,
        warmup = 5,
        chains = 1,
        seed = 4711,
        refresh = 0
      )
    )
  )

  init_values <- backend_get_last_draws_for_init("rstan", fit)
  refit <- suppressWarnings(
    backend_sample(
      backend = "rstan",
      sample_arguments = list(
        file = sampler_state_constrained_test_stan_file(),
        data = list(),
        iter = 2,
        warmup = 1,
        chains = 1,
        seed = 4712,
        refresh = 0,
        init = init_values
      )
    )
  )

  expect_s4_class(refit, "stanfit")
  expect_equal(rstan::get_inits(refit), init_values, tolerance = 0)
})

test_that("backend_get_last_draws_for_init output can be passed from rstan to cmdstanr as init", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()

  backend_sample <- get_internal("backend_sample")
  backend_get_last_draws_for_init <- get_internal("backend_get_last_draws_for_init")
  fit <- suppressWarnings(
    backend_sample(
      backend = "rstan",
      sample_arguments = list(
        file = sampler_state_constrained_test_stan_file(),
        data = list(),
        iter = 10,
        warmup = 5,
        chains = 1,
        seed = 4711,
        refresh = 0
      )
    )
  )

  init_values <- backend_get_last_draws_for_init("rstan", fit)
  refit <- backend_sample(
    backend = "cmdstanr",
    sample_arguments = list(
      data = list(),
      chains = 1,
      parallel_chains = 1,
      iter_warmup = 1,
      iter_sampling = 3,
      seed = 4712,
      refresh = 0,
      show_messages = FALSE,
      show_exceptions = FALSE,
      init = init_values
    ),
    stan_file = sampler_state_constrained_test_stan_file()
  )

  init_from_fit <- refit$init()
  expect_length(init_from_fit, 1)
  expect_equal(
    lapply(init_from_fit, unlist, use.names = TRUE),
    lapply(init_values, unlist, use.names = TRUE),
    tolerance = 1e-12
  )
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

test_that("backend_get_last_draws_for_init with cmdstanr returns init-ready last draws", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  backend_get_last_draws_for_init <- get_internal("backend_get_last_draws_for_init")
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

  res <- backend_get_last_draws_for_init("cmdstanr", fit)
  draws <- as.array(fit$draws(variables = "y", inc_warmup = FALSE, format = "draws_array"))

  expect_length(res, 1)
  expect_named(res[[1]], "y")
  expect_equal(as.numeric(res[[1]]$y), as.numeric(draws[dim(draws)[1], 1, ]), tolerance = 0)
})

test_that("backend_get_last_draws_for_init output can be passed back to cmdstanr as init", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  backend_get_last_draws_for_init <- get_internal("backend_get_last_draws_for_init")
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
    stan_file = sampler_state_constrained_test_stan_file()
  )

  init_values <- backend_get_last_draws_for_init("cmdstanr", fit)
  refit <- backend_sample(
    backend = "cmdstanr",
    sample_arguments = list(
      data = list(),
      chains = 1,
      parallel_chains = 1,
      iter_warmup = 1,
      iter_sampling = 3,
      seed = 4712,
      refresh = 0,
      show_messages = FALSE,
      show_exceptions = FALSE,
      init = init_values
    ),
    stan_file = sampler_state_constrained_test_stan_file()
  )

  init_from_fit <- refit$init()
  expect_length(init_from_fit, 1)
  expect_equal(
    lapply(init_from_fit, unlist, use.names = TRUE),
    lapply(init_values, unlist, use.names = TRUE),
    tolerance = 1e-12
  )
})
