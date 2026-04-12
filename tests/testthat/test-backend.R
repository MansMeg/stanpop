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

test_that("backend_extract_parameter_roots_from_stan_code keeps only parameters-block roots", {
  backend_extract_parameter_roots_from_stan_code <- get_internal("backend_extract_parameter_roots_from_stan_code")

  stan_code <- paste(
    "  parameters   { // leading and internal whitespace should be ignored",
    "  real alpha;",
    "  vector[2] beta;",
    "  array[3] real gamma;",
    "}",
    "generated quantities {",
    "  real min_x_pred;",
    "}",
    sep = "\n"
  )

  roots <- backend_extract_parameter_roots_from_stan_code(stan_code)

  expect_setequal(roots, c("alpha", "beta", "gamma"))
  expect_false("min_x_pred" %in% roots)
})

test_that("backend_extract_parameter_roots_from_stan_code parses the test Stan examples", {
  backend_extract_parameter_roots_from_stan_code <- get_internal("backend_extract_parameter_roots_from_stan_code")

  expect_setequal(
    backend_extract_parameter_roots_from_stan_code(
      paste(readLines(sampler_state_test_stan_file(), warn = FALSE), collapse = "\n")
    ),
    "y"
  )
  expect_setequal(
    backend_extract_parameter_roots_from_stan_code(
      paste(readLines(sampler_state_constrained_test_stan_file(), warn = FALSE), collapse = "\n")
    ),
    c("sigma", "y")
  )
  expect_setequal(
    backend_extract_parameter_roots_from_stan_code(
      paste(
        readLines(testthat::test_path("stan_code", "cov_reg_to_chol.stan"), warn = FALSE),
        collapse = "\n"
      )
    ),
    c("beta", "sigma", "mu")
  )
})

test_that("backend_extract_parameter_roots_from_stan_code parses the packaged Stan models", {
  backend_extract_parameter_roots_from_stan_code <- get_internal("backend_extract_parameter_roots_from_stan_code")
  stan_models_dir <- dirname(get_pop_stan_model_file_path(supported_pop_models()[1]))

  stan_files <- list.files(
    stan_models_dir,
    pattern = "\\.stan$",
    full.names = TRUE
  )

  expect_true(dir.exists(stan_models_dir))
  expect_true(length(stan_files) > 0L)

  for(stan_file in stan_files) {
    stan_code <- paste(readLines(stan_file, warn = FALSE), collapse = "\n")
    roots <- backend_extract_parameter_roots_from_stan_code(stan_code)

    expect_true(length(roots) > 0L, info = basename(stan_file))
    expect_identical(anyDuplicated(roots), 0L, info = basename(stan_file))
    expect_true(all(nzchar(roots)), info = basename(stan_file))
    expect_true(all(c("x_unknown", "sigma_x") %in% roots), info = basename(stan_file))
    expect_false("min_x_pred" %in% roots, info = basename(stan_file))
    expect_false("min_x_pred_all" %in% roots, info = basename(stan_file))
  }
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

test_that("backend_relist_flat_draw_to_init omits parameter roots with only missing values", {
  backend_relist_flat_draw_to_init <- get_internal("backend_relist_flat_draw_to_init")

  draw <- stats::setNames(
    c(0.1, NA_real_, NA_real_),
    c("alpha", "beta[1]", "beta[2]")
  )
  skeleton <- list(
    alpha = 0,
    beta = numeric(2)
  )

  expect_warning(
    res <- backend_relist_flat_draw_to_init(draw, skeleton),
    "parameter root\\(s\\): beta"
  )

  expect_identical(res$alpha, 0.1)
  expect_false("beta" %in% names(res))
})

test_that("backend_relist_flat_draw_to_init omits zero-size parameter roots", {
  backend_relist_flat_draw_to_init <- get_internal("backend_relist_flat_draw_to_init")

  draw <- stats::setNames(
    c(0.1),
    c("alpha")
  )
  skeleton <- list(
    alpha = 0,
    x_unknown = numeric(0)
  )

  res <- backend_relist_flat_draw_to_init(draw, skeleton)

  expect_identical(res$alpha, 0.1)
  expect_false("x_unknown" %in% names(res))
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

test_that("backend_capture_warm_start_state stores the refit warm_start_state", {
  backend_capture_warm_start_state <- get_internal("backend_capture_warm_start_state")

  # Pin the exact warm_start_state bundle stored on a poll_of_polls object for later refits.
  testthat::local_mocked_bindings(
    backend_get_last_draws_for_init = function(...) {
      list(list(alpha = 0.1))
    },
    backend_get_sampler_state = function(...) {
      list(list(step_size = 0.25, inv_metric = c(1, 2), metric_type = "diag_e"))
    },
    backend_get_init_skeleton = function(...) {
      list(list(alpha = 0))
    },
    backend_get_num_upars = function(...) 2L,
    .package = "stanpop"
  )

  res <- backend_capture_warm_start_state(
    "cmdstanr",
    structure(list(), class = "mock_fit")
  )

  expect_named(res, c("init", "init_complete", "sampler_state", "init_skeleton", "num_upars"))
  expect_identical(res$init, list(list(alpha = 0.1)))
  expect_true(res$init_complete)
  expect_identical(res$sampler_state[[1]]$metric_type, "diag_e")
  expect_identical(res$init_skeleton, list(list(alpha = 0)))
  expect_identical(res$num_upars, 2L)
})

test_that("backend_capture_warm_start_state disables cached init reuse when last draws are incomplete", {
  backend_capture_warm_start_state <- get_internal("backend_capture_warm_start_state")

  testthat::local_mocked_bindings(
    backend_get_last_draws_for_init = function(...) {
      list(list(alpha = 0.1))
    },
    backend_get_sampler_state = function(...) {
      list(list(step_size = 0.25, inv_metric = c(1, 2), metric_type = "diag_e"))
    },
    backend_get_init_skeleton = function(...) {
      list(list(alpha = 0, beta = numeric(2)))
    },
    backend_get_num_upars = function(...) 2L,
    .package = "stanpop"
  )

  expect_warning(
    res <- backend_capture_warm_start_state(
      "cmdstanr",
      structure(list(), class = "mock_fit")
    ),
    "Automatic init reuse will be disabled"
  )

  expect_null(res$init)
  expect_false(res$init_complete)
  expect_identical(res$sampler_state[[1]]$step_size, 0.25)
  expect_identical(res$num_upars, 2L)
})

test_that("backend_build_init_skeleton_from_variable_names can ignore generated quantities", {
  backend_build_init_skeleton_from_variable_names <- get_internal("backend_build_init_skeleton_from_variable_names")

  skeleton <- backend_build_init_skeleton_from_variable_names(
    c("alpha", "beta[1]", "beta[2]", "min_x_pred", "lp__", "stepsize__"),
    parameter_roots = c("alpha", "beta")
  )

  expect_named(skeleton, c("alpha", "beta"))
  expect_false("min_x_pred" %in% names(skeleton))
})

test_that("backend_get_cmdstanr_parameter_roots prefers Stan code from the runset", {
  backend_get_cmdstanr_parameter_roots <- get_internal("backend_get_cmdstanr_parameter_roots")

  mock_fit <- list(
    runset = list(
      stan_code = function() {
        c(
          "parameters {",
          "  real alpha;",
          "  vector[2] beta;",
          "}",
          "generated quantities {",
          "  real min_x_pred;",
          "}"
        )
      }
    ),
    metadata = function() {
      list(
        model_params = c("alpha", "beta[1]", "beta[2]", "min_x_pred")
      )
    }
  )

  roots <- backend_get_cmdstanr_parameter_roots(
    mock_fit,
    variable_names = c("alpha", "beta[1]", "beta[2]", "min_x_pred", "lp__")
  )

  expect_identical(roots, c("alpha", "beta"))
})

test_that("backend_get_cmdstanr_parameter_roots falls back to metadata and draw names", {
  backend_get_cmdstanr_parameter_roots <- get_internal("backend_get_cmdstanr_parameter_roots")

  metadata_fit <- list(
    runset = list(
      stan_code = function() stop("stan code unavailable")
    ),
    metadata = function() {
      list(
        sampler_diagnostics = c("accept_stat__", "stepsize__", "treedepth__"),
        model_params = c("lp__", "accept_stat__", "stepsize__", "alpha[1]", "alpha[2]", "beta", "min_x_pred")
      )
    }
  )

  expect_identical(
    backend_get_cmdstanr_parameter_roots(
      metadata_fit,
      variable_names = c("alpha[1]", "alpha[2]", "beta", "min_x_pred", "lp__", "accept_stat__", "stepsize__")
    ),
    c("alpha", "beta", "min_x_pred")
  )

  variable_fit <- list(
    runset = list(
      stan_code = function() stop("stan code unavailable")
    ),
    metadata = function() {
      list()
    }
  )

  expect_identical(
    backend_get_cmdstanr_parameter_roots(
      variable_fit,
      variable_names = c("alpha[1]", "alpha[2]", "beta", "min_x_pred", "lp__", "stepsize__")
    ),
    c("alpha", "beta", "min_x_pred")
  )
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

test_that("rstan last draw and sampler state can fully warm-start cmdstanr on a constrained model", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()

  backend_sample <- get_internal("backend_sample")
  backend_get_last_draws_for_init <- get_internal("backend_get_last_draws_for_init")
  backend_get_sampler_state <- get_internal("backend_get_sampler_state")
  backend_get_rstan_init_skeleton <- get_internal("backend_get_rstan_init_skeleton")
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
  state <- backend_get_sampler_state("rstan", fit)
  skeleton <- backend_get_rstan_init_skeleton(fit)

  expect_length(init_values, 1)
  expect_setequal(names(init_values[[1]]), names(skeleton[[1]]))
  expect_false(any(vapply(init_values[[1]], function(x) anyNA(x), logical(1))))
  expect_identical(state[[1]]$metric_type, "diag_e")

  refit <- backend_sample(
    backend = "cmdstanr",
    sample_arguments = list(
      data = list(),
      chains = 1,
      parallel_chains = 1,
      iter_warmup = 0,
      iter_sampling = 3,
      adapt_engaged = FALSE,
      seed = 4712,
      refresh = 0,
      show_messages = FALSE,
      show_exceptions = FALSE,
      init = init_values,
      inv_metric = state[[1]]$inv_metric,
      metric = state[[1]]$metric_type,
      step_size = state[[1]]$step_size
    ),
    stan_file = sampler_state_constrained_test_stan_file()
  )

  init_from_fit <- refit$init()
  refit_state <- backend_get_sampler_state("cmdstanr", refit)
  expect_length(init_from_fit, 1)
  expect_equal(
    lapply(init_from_fit, unlist, use.names = TRUE),
    lapply(init_values, unlist, use.names = TRUE),
    tolerance = 1e-12
  )
  expect_identical(refit_state[[1]]$metric_type, state[[1]]$metric_type)
  expect_equal(refit_state[[1]]$inv_metric, state[[1]]$inv_metric, tolerance = 1e-12)
  expect_equal(refit_state[[1]]$step_size, state[[1]]$step_size, tolerance = 1e-12)
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

test_that("backend_get_num_upars with rstan falls back to sampler state when the stanfit model is invalid", {
  backend_get_num_upars <- get_internal("backend_get_num_upars")

  # A deserialized stanfit can lose model-object-dependent queries like get_num_upars()
  # even though the saved inverse metric is still recoverable from the stanfit object.
  testthat::local_mocked_bindings(
    get_num_upars = function(...) {
      stop("the model object is not created or not valid")
    },
    .package = "rstan"
  )
  testthat::local_mocked_bindings(
    backend_get_sampler_state = function(...) {
      list(
        list(inv_metric = c(1, 2, 3), metric_type = "diag_e"),
        list(inv_metric = c(4, 5, 6), metric_type = "diag_e")
      )
    },
    .package = "stanpop"
  )

  expect_identical(
    backend_get_num_upars("rstan", structure(list(), class = "mock_stan_fit")),
    3L
  )
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
