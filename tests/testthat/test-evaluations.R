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

expect_known_state_periods_match_pop <- function(pop) {
  model_time_to <- unname(time_range(pop$time_line)["to"])
  known_dates <- pop$known_state$date
  known_dates <- sort(unique(known_dates[!is.na(known_dates) & known_dates <= model_time_to]))

  expected <- tibble::tibble(
    period_index = seq_along(known_dates),
    evaluation_date = known_dates,
    previous_evaluation_date = c(as.Date(NA), known_dates[-length(known_dates)]),
    period_from = c(as.Date(NA), known_dates[-length(known_dates)]),
    period_to = known_dates,
    is_last = seq_along(known_dates) == length(known_dates)
  )

  expect_equal(get_known_state_periods(pop), expected, tolerance = 0)
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

test_that("get_known_state_periods returns the expected periods for the saved rstan fixture", {
  pop <- load_evaluation_fixture_pop("rstan")
  expect_known_state_periods_match_pop(pop)
})

test_that("get_known_state_periods returns the expected periods for the saved cmdstanr fixture", {
  pop <- load_evaluation_fixture_pop("cmdstanr")
  expect_known_state_periods_match_pop(pop)
})

test_that("get_known_state_periods sorts, deduplicates, and drops unusable dates", {
  pop <- load_evaluation_fixture_pop("rstan")
  model_time_to <- unname(time_range(pop$time_line)["to"])
  original_dates <- pop$known_state$date
  expected_dates <- sort(unique(c(original_dates[[3]], original_dates[[1]], original_dates[[2]])))
  pop$known_state <- pop$known_state[c(3, 1, 3, 2, 2, 1), , drop = FALSE]
  pop$known_state$date <- c(
    model_time_to + 5,
    original_dates[[3]],
    as.Date(NA),
    original_dates[[1]],
    original_dates[[3]],
    original_dates[[2]]
  )

  expect_equal(
    get_known_state_periods(pop),
    tibble::tibble(
      period_index = 1:3,
      evaluation_date = expected_dates,
      previous_evaluation_date = c(as.Date(NA), expected_dates[1:2]),
      period_from = c(as.Date(NA), expected_dates[1:2]),
      period_to = expected_dates,
      is_last = c(FALSE, FALSE, TRUE)
    ),
    tolerance = 0
  )
})

test_that("get_known_state_periods returns a zero-row tibble when there are no usable dates", {
  pop <- load_evaluation_fixture_pop("rstan")
  model_time_to <- unname(time_range(pop$time_line)["to"])
  pop$known_state$date <- rep(model_time_to + 1, nrow(pop$known_state))

  expect_equal(
    get_known_state_periods(pop),
    tibble::tibble(
      period_index = integer(),
      evaluation_date = as.Date(character()),
      previous_evaluation_date = as.Date(character()),
      period_from = as.Date(character()),
      period_to = as.Date(character()),
      is_last = logical()
    ),
    tolerance = 0
  )
})
