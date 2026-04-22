context("plot")

plot_fixture_path <- function(backend) {
  testthat::test_path("files", paste0("test_pop_v0_7_3_", backend, ".rds"))
}

load_plot_fixture_pop <- function(backend) {
  fixture_path <- plot_fixture_path(backend)
  if(!file.exists(fixture_path)) {
    testthat::skip(paste0("Fixture not available: ", basename(fixture_path)))
  }
  if(identical(backend, "cmdstanr")) {
    testthat::skip_if_not_installed("cmdstanr")
  }

  load_pop(fixture_path)
}

expect_plot_parameters_bayesplot_matrix_matches_fixture <- function(backend) {
  testthat::skip_if_not_installed("posterior")

  plot_parameters_bayesplot <- get_internal("plot_parameters_bayesplot")
  pop <- load_plot_fixture_pop(backend)
  vars <- c("x_pred[2,1]", "x_pred[1,1]", "sigma_x[1]")
  labels <- c("latent_t2", "latent_t1", "sigma_party1")

  post_matrix <- plot_parameters_bayesplot(
    x = pop,
    bayeplot_FUN = function(x, ...) x,
    params = vars,
    params_plot_name = labels
  )

  x_pred <- extract(pop, pars = "x_pred")[[1]]
  sigma_x <- extract(pop, pars = "sigma_x")[[1]]

  expect_equal(as.numeric(post_matrix[, "latent_t2"]), x_pred[, 2, 1], tolerance = 0)
  expect_equal(as.numeric(post_matrix[, "latent_t1"]), x_pred[, 1, 1], tolerance = 0)
  expect_equal(as.numeric(post_matrix[, "sigma_party1"]), sigma_x[, 1], tolerance = 0)
  expect_identical(colnames(post_matrix), labels)
}

expect_traceplot_uses_selected_draws <- function(backend) {
  testthat::skip_if_not_installed("bayesplot")
  testthat::skip_if_not_installed("posterior")

  pop <- load_plot_fixture_pop(backend)
  vars <- c("x_pred[2,1]", "sigma_x[1]")

  testthat::local_mocked_bindings(
    mcmc_trace = function(x, ...) {
      list(draws = x, dots = list(...))
    },
    .package = "bayesplot"
  )

  res <- traceplot(pop, pars = vars, facet_args = list(ncol = 1))

  expect_s3_class(res$draws, "draws_array")
  expect_identical(posterior::variables(res$draws), vars)
  expect_identical(res$dots$facet_args, list(ncol = 1))
}

expect_x_change_matches_manual <- function(backend, type) {
  pop <- load_plot_fixture_pop(backend)
  x <- extract(pop, pars = "x_pred")$x_pred
  dims <- dim(x)
  n <- 1L

  if(identical(type, "diff")) {
    mat <- x[, (1 + n):dims[2], ] - x[, 1:(dims[2] - n), ]
  } else {
    mat <- x[, (1 + n):dims[2], ] / x[, 1:(dims[2] - n), ]
  }

  expected <- pop$time_line$time_line
  for(i in seq_along(pop$y)) {
    expected[[pop$y[i]]] <- c(NA_real_, colMeans(mat[, , i]))
  }

  actual <- extract_pop_empirical_posterior_mean_x_change(pop, type = type)

  expect_equal(actual, expected, tolerance = 0)
}

expect_known_state_overlay_equivalent <- function(backend) {
  pop <- load_plot_fixture_pop(backend)
  y <- pop$y[[1]]
  known_state_df <- pop$known_state[, c("date", y), drop = FALSE]

  pop_build <- suppressWarnings(
    ggplot2::ggplot_build(
      ggplot2::ggplot() + geom_known_state(pop, y, size = 0.75)
    )
  )
  df_build <- suppressWarnings(
    ggplot2::ggplot_build(
      ggplot2::ggplot() + geom_known_state(known_state_df, y, size = 0.75)
    )
  )

  expect_equal(pop_build$data, df_build$data, tolerance = 0)
}

test_that("plot works for polls data object", {

  skip("TODO: Test that there is a warning if not the whole latent state is plotted that also propose how to change time_range to show the whole LS")


})



test_that("plot works for poll_of_polls data object", {

  skip("TODO: Test that there is a warning if not the whole latent state is plotted that also propose how to change time_range to show the whole LS")

})

test_that("plot_parameters_bayesplot uses exact requested variables for the saved rstan fixture", {
  expect_plot_parameters_bayesplot_matrix_matches_fixture("rstan")
})

test_that("plot_parameters_bayesplot uses exact requested variables for the saved cmdstanr fixture", {
  expect_plot_parameters_bayesplot_matrix_matches_fixture("cmdstanr")
})

test_that("traceplot uses backend-neutral draws for the saved rstan fixture", {
  expect_traceplot_uses_selected_draws("rstan")
})

test_that("traceplot uses backend-neutral draws for the saved cmdstanr fixture", {
  expect_traceplot_uses_selected_draws("cmdstanr")
})

test_that("extract_pop_empirical_posterior_mean_x_change matches manual diff computation for the saved rstan fixture", {
  expect_x_change_matches_manual("rstan", "diff")
})

test_that("extract_pop_empirical_posterior_mean_x_change matches manual diff computation for the saved cmdstanr fixture", {
  expect_x_change_matches_manual("cmdstanr", "diff")
})

test_that("extract_pop_empirical_posterior_mean_x_change matches manual ratio computation for the saved rstan fixture", {
  expect_x_change_matches_manual("rstan", "ratio")
})

test_that("extract_pop_empirical_posterior_mean_x_change matches manual ratio computation for the saved cmdstanr fixture", {
  expect_x_change_matches_manual("cmdstanr", "ratio")
})

test_that("extract_pop_election_period warns that the helper is deprecated and still returns an election_period column", {
  pop <- load_plot_fixture_pop("rstan")

  expect_warning(
    actual <- extract_pop_election_period(pop),
    "extract_pop_election_period\\(\\) is deprecated\\."
  )

  expect_s3_class(actual, "data.frame", exact = FALSE)
  expect_true("election_period" %in% names(actual))
})

test_that("geom_known_state builds equivalent layers for poll_of_polls and explicit known-state data on the saved rstan fixture", {
  expect_known_state_overlay_equivalent("rstan")
})

test_that("geom_known_state builds equivalent layers for poll_of_polls and explicit known-state data on the saved cmdstanr fixture", {
  expect_known_state_overlay_equivalent("cmdstanr")
})

test_that("known_state_overlay_data sorts rows and drops missing dates", {
  known_state_overlay_data <- get_internal("known_state_overlay_data")
  pop <- load_plot_fixture_pop("rstan")
  y <- pop$y[[1]]
  original <- pop$known_state[, c("date", y), drop = FALSE]

  x <- original[c(3, 1, 2), , drop = FALSE]
  x$date[[2]] <- as.Date(NA)

  expect_equal(
    known_state_overlay_data(x, y),
    tibble::tibble(
      date = sort(original$date[c(2, 3)]),
      value = original[[y]][c(2, 3)][order(original$date[c(2, 3)])]
    ),
    tolerance = 0
  )
})

test_that("known_state_overlay_data errors when required columns are missing", {
  known_state_overlay_data <- get_internal("known_state_overlay_data")
  pop <- load_plot_fixture_pop("rstan")
  y <- pop$y[[1]]

  expect_error(
    known_state_overlay_data(pop$known_state["date"], y),
    "Known-state overlay data is missing required column\\(s\\):"
  )
})

test_that("known_state_overlay_data errors on duplicate dates", {
  known_state_overlay_data <- get_internal("known_state_overlay_data")
  pop <- load_plot_fixture_pop("rstan")
  y <- pop$y[[1]]
  x <- pop$known_state[c(1, 1), c("date", y), drop = FALSE]

  expect_error(
    known_state_overlay_data(x, y),
    "Known-state overlay data must not contain duplicate dates\\."
  )
})
