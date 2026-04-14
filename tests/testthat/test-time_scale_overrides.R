context("time_scale_overrides")

test_that("normalize_time_scale_overrides uses the default scale when no overrides are supplied", {
  normalize_time_scale_overrides <- get_internal("normalize_time_scale_overrides")
  tr <- time_range(c("2020-01-01", "2020-01-05"))

  expect_silent(
    schedule <- normalize_time_scale_overrides(
      time_scale = "week",
      time_scale_overrides = NULL,
      model_time_range = tr
    )
  )

  expect_s3_class(schedule, "tbl_df")
  checkmate::expect_names(names(schedule), identical.to = c("date", "time_scale", "time_scale_days"))
  expect_identical(schedule$date, seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = 1))
  expect_identical(schedule$time_scale, rep("week", 5))
  expect_identical(schedule$time_scale_days, rep(7L, 5))
})


test_that("normalize_time_scale_overrides applies inclusive override ranges", {
  normalize_time_scale_overrides <- get_internal("normalize_time_scale_overrides")
  tr <- time_range(c("2020-01-01", "2020-01-10"))
  overrides <- tibble::tibble(
    from = as.Date(c("2020-01-03", "2020-01-08")),
    to = as.Date(c("2020-01-05", "2020-01-09")),
    time_scale = c("day", "day")
  )

  expect_silent(
    schedule <- normalize_time_scale_overrides(
      time_scale = "week",
      time_scale_overrides = overrides,
      model_time_range = tr
    )
  )

  expected_scales <- c("week", "week", "day", "day", "day", "week", "week", "day", "day", "week")
  expect_identical(schedule$time_scale, expected_scales)
  expect_identical(schedule$time_scale_days, c(7L, 7L, 1L, 1L, 1L, 7L, 7L, 1L, 1L, 7L))
})


test_that("normalize_time_scale_overrides reuses validator errors", {
  normalize_time_scale_overrides <- get_internal("normalize_time_scale_overrides")
  tr <- time_range(c("2020-01-01", "2020-01-10"))
  overlaps <- tibble::tibble(
    from = as.Date(c("2020-01-01", "2020-01-05")),
    to = as.Date(c("2020-01-05", "2020-01-07")),
    time_scale = c("day", "day")
  )

  expect_error(
    normalize_time_scale_overrides(
      time_scale = "week",
      time_scale_overrides = overlaps,
      model_time_range = tr
    ),
    regexp = "inclusive|overlapping"
  )
})


test_that("normalize_time_scale_overrides currently only allows daily override rows", {
  normalize_time_scale_overrides <- get_internal("normalize_time_scale_overrides")
  tr <- time_range(c("2020-01-01", "2020-01-10"))
  overrides <- tibble::tibble(
    from = as.Date("2020-01-03"),
    to = as.Date("2020-01-05"),
    time_scale = "month"
  )

  expect_error(
    normalize_time_scale_overrides(
      time_scale = "week",
      time_scale_overrides = overrides,
      model_time_range = tr
    ),
    regexp = "only supports|time_scale.+day"
  )
})
