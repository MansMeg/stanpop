context("time_line_with_overrides")

test_that("time_line_with_overrides matches time_line without overrides", {
  time_line_with_overrides <- get_internal("time_line_with_overrides")
  tr <- time_range(c("2020-01-01", "2020-01-15"))

  expect_silent(tl_old <- time_line(tr, time_scale = "week"))
  expect_silent(
    tl_new <- time_line_with_overrides(
      model_time_range = tr,
      time_scale = "week",
      time_scale_overrides = NULL
    )
  )

  expect_s3_class(tl_new, "time_line")
  expect_equal(
    tl_new$daily[, c("date", "t", "time_line_date", "time_line_t")],
    tl_old$daily
  )
  expect_identical(tl_new$time_line[, c("date", "t")], tl_old$time_line)
  expect_identical(tl_new$daily$time_scale, rep("week", nrow(tl_new$daily)))
  expect_identical(tl_new$daily$time_scale_days, rep(7L, nrow(tl_new$daily)))
  expect_true(is.na(tl_new$time_line$delta_days[1]))
  expect_true(is.na(tl_new$time_line$step_scale[1]))
  expect_identical(tl_new$time_line$delta_days[-1], c(7L, 7L))
  expect_equal(tl_new$time_line$step_scale[-1], c(1, 1))
})


test_that("time_line_with_overrides inserts daily latent dates inside override windows", {
  time_line_with_overrides <- get_internal("time_line_with_overrides")
  tr <- time_range(c("2020-01-01", "2020-01-15"))
  overrides <- tibble::tibble(
    from = as.Date("2020-01-08"),
    to = as.Date("2020-01-10"),
    time_scale = "day"
  )

  expect_silent(
    tl <- time_line_with_overrides(
      model_time_range = tr,
      time_scale = "week",
      time_scale_overrides = overrides
    )
  )

  expect_identical(
    tl$time_line$date,
    as.Date(c("2019-12-30", "2020-01-06", "2020-01-08", "2020-01-09", "2020-01-10", "2020-01-13"))
  )
  expect_true(is.na(tl$time_line$delta_days[1]))
  expect_true(is.na(tl$time_line$step_scale[1]))
  expect_identical(tl$time_line$delta_days[-1], c(7L, 2L, 1L, 1L, 3L))
  expect_equal(tl$time_line$step_scale[-1], sqrt(c(7, 2, 1, 1, 3) / 7))

  expect_identical(
    tl$daily$time_line_date[tl$daily$date %in% as.Date(c("2020-01-07", "2020-01-08", "2020-01-11", "2020-01-12", "2020-01-13"))],
    as.Date(c("2020-01-06", "2020-01-08", "2020-01-10", "2020-01-10", "2020-01-13"))
  )

  expect_identical(
    tl$daily$time_scale[tl$daily$date %in% as.Date(c("2020-01-07", "2020-01-08", "2020-01-11"))],
    c("week", "day", "week")
  )
})


test_that("time_line_with_overrides handles overrides that start on a weekly anchor", {
  time_line_with_overrides <- get_internal("time_line_with_overrides")
  tr <- time_range(c("2020-01-01", "2020-01-15"))
  overrides <- tibble::tibble(
    from = as.Date("2020-01-06"),
    to = as.Date("2020-01-10"),
    time_scale = "day"
  )

  expect_silent(
    tl <- time_line_with_overrides(
      model_time_range = tr,
      time_scale = "week",
      time_scale_overrides = overrides
    )
  )

  expect_identical(
    tl$time_line$date,
    as.Date(c("2019-12-30", "2020-01-06", "2020-01-07", "2020-01-08", "2020-01-09", "2020-01-10", "2020-01-13"))
  )
  expect_identical(tl$time_line$delta_days[-1], c(7L, 1L, 1L, 1L, 1L, 3L))
  expect_equal(tl$time_line$step_scale[-1], sqrt(c(7, 1, 1, 1, 1, 3) / 7))

  expect_identical(
    tl$daily$time_line_date[tl$daily$date %in% as.Date(c("2020-01-10", "2020-01-11", "2020-01-12", "2020-01-13"))],
    as.Date(c("2020-01-10", "2020-01-10", "2020-01-10", "2020-01-13"))
  )
})

test_that("time_line_with_overrides currently rejects non-daily override rows", {
  time_line_with_overrides <- get_internal("time_line_with_overrides")
  tr <- time_range(c("2020-01-01", "2020-01-12"))
  overrides <- tibble::tibble(
    from = as.Date("2020-01-06"),
    to = as.Date("2020-01-10"),
    time_scale = "week"
  )

  expect_error(
    time_line_with_overrides(
      model_time_range = tr,
      time_scale = "day",
      time_scale_overrides = overrides
    ),
    regexp = "only supports|time_scale.+day"
  )
})
