context("diagnostics")

test_that("parse_adaption_information parses diagonal inverse metrics", {
  parse_adaption_information <- get_internal("parse_adaption_information")

  x <- paste(
    "# Adaptation terminated",
    "# Step size = 0.125",
    "#",
    "# Diagonal elements of inverse mass matrix:",
    "# 1.5, 2.5, 3.5",
    sep = "\n"
  )

  res <- parse_adaption_information(x)

  expect_true(isTRUE(res$adaption_terminated))
  expect_equal(res$step_size, 0.125, tolerance = 0)
  expect_identical(res$metric_type, "diag_e")
  expect_equal(res$inv_metric, c(1.5, 2.5, 3.5), tolerance = 0)
  expect_equal(res$diag_inv_mass_matrix, c(1.5, 2.5, 3.5), tolerance = 0)
})

test_that("parse_adaption_information parses dense inverse metrics", {
  parse_adaption_information <- get_internal("parse_adaption_information")

  x <- paste(
    "# Adaptation terminated",
    "# Step size = 0.2",
    "#",
    "# Elements of inverse mass matrix:",
    "# 1.0, 0.25",
    "# 0.25, 2.0",
    sep = "\n"
  )

  res <- parse_adaption_information(x)

  expect_true(isTRUE(res$adaption_terminated))
  expect_equal(res$step_size, 0.2, tolerance = 0)
  expect_identical(res$metric_type, "dense_e")
  expect_equal(
    res$inv_metric,
    matrix(c(1.0, 0.25, 0.25, 2.0), nrow = 2, byrow = TRUE),
    tolerance = 0
  )
  expect_equal(res$diag_inv_mass_matrix, c(1.0, 2.0), tolerance = 0)
})

test_that("parse_adaption_information parses real rstan adaptation output", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  parse_adaption_information <- get_internal("parse_adaption_information")
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

  raw_ai <- rstan::get_adaptation_info(fit)[[1]]
  res <- parse_adaption_information(raw_ai)
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)[[1]]

  expect_true(isTRUE(res$adaption_terminated))
  expect_true(is.finite(res$step_size))
  expect_equal(unname(res$step_size), unname(sampler_params[1, "stepsize__"]), tolerance = 1e-6)
  expect_identical(res$metric_type, "diag_e")
  expect_type(res$inv_metric, "double")
  expect_length(res$inv_metric, rstan::get_num_upars(fit))
  expect_equal(res$diag_inv_mass_matrix, res$inv_metric, tolerance = 0)
})
