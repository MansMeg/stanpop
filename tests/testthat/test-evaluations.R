context("evaluations")

evaluation_fixture_path <- function(backend) {
  testthat::test_path("files", paste0("test_pop_v0_7_3_", backend, ".rds"))
}

load_evaluation_fixture_pop <- function(backend) {
  fixture_path <- evaluation_fixture_path(backend)
  if(!file.exists(fixture_path)) {
    testthat::skip(paste0("Fixture not available: ", basename(fixture_path)))
  }
  if(identical(backend, "cmdstanr")) {
    testthat::skip_if_not_installed("cmdstanr")
  }

  load_pop(fixture_path)
}

expect_last_evaluation_info_matches_pop <- function(backend) {
  pop <- load_evaluation_fixture_pop(backend)
  model_time_to <- unname(time_range(pop$time_line)["to"])
  evaluation_date <- max(pop$known_state$date[pop$known_state$date <= model_time_to])
  expected <- tibble::tibble(
    model_time_to = model_time_to,
    evaluation_date = evaluation_date,
    gap_days = as.integer(model_time_to - evaluation_date)
  )

  expect_equal(get_last_evaluation_info(pop), expected, tolerance = 0)
}

test_that("get_last_evaluation_info returns the latest usable known state for the saved rstan fixture", {
  expect_last_evaluation_info_matches_pop("rstan")
})

test_that("get_last_evaluation_info returns the latest usable known state for the saved cmdstanr fixture", {
  expect_last_evaluation_info_matches_pop("cmdstanr")
})

test_that("get_last_evaluation_info returns missing evaluation values when known_state is NULL", {
  pop <- load_evaluation_fixture_pop("rstan")
  pop$known_state <- NULL

  expect_equal(
    get_last_evaluation_info(pop),
    tibble::tibble(
      model_time_to = unname(time_range(pop$time_line)["to"]),
      evaluation_date = as.Date(NA),
      gap_days = NA_integer_
    ),
    tolerance = 0
  )
})

test_that("get_last_evaluation_info returns missing evaluation values when all known states are after the model end date", {
  pop <- load_evaluation_fixture_pop("rstan")
  model_time_to <- unname(time_range(pop$time_line)["to"])
  pop$known_state$date <- rep(model_time_to + 1, nrow(pop$known_state))

  expect_equal(
    get_last_evaluation_info(pop),
    tibble::tibble(
      model_time_to = model_time_to,
      evaluation_date = as.Date(NA),
      gap_days = NA_integer_
    ),
    tolerance = 0
  )
})
