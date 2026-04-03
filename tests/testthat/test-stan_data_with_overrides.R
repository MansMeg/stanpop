context("stan_data_with_overrides")

test_that("stan_polls_data attaches an override-aware path without changing legacy data", {
  time_line_with_overrides <- get_internal("time_line_with_overrides")

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  set.seed(4711)
  spd <- simulate_polls(
    x = txdf,
    pd = pd_test,
    npolls = 60,
    time_scale = "week",
    start_date = "2010-01-01"
  )
  mtr <- time_range(spd)
  known_state <- tibble::tibble(
    date = as.Date(c(mtr["from"], mtr["from"] + 35)),
    x3 = c(0.25, 0.30),
    x4 = c(0.20, 0.22)
  )
  overrides <- tibble::tibble(
    from = as.Date(mtr["from"] + 20),
    to = as.Date(mtr["from"] + 24),
    time_scale = "day"
  )

  cfg <- list(
    sigma_kappa_hyper = 0.01,
    use_multivariate_version = 0
  )

  expect_silent(
    suppressWarnings(
      suppressMessages(
        sd_base <- stan_polls_data(
          x = spd,
          time_scale = "week",
          y_name = c("x3", "x4"),
          model = "model8k1",
          known_state = known_state,
          hyper_parameters = cfg
        )
      )
    )
  )

  expect_silent(
    suppressWarnings(
      suppressMessages(
        sd_override <- stan_polls_data(
          x = spd,
          time_scale = "week",
          time_scale_overrides = overrides,
          y_name = c("x3", "x4"),
          model = "model8k1",
          known_state = known_state,
          hyper_parameters = cfg
        )
      )
    )
  )

  expect_true(all(c("stan_data_with_overrides", "time_line_with_overrides") %in% names(sd_base)))
  expect_identical(sd_base$stan_data, sd_override$stan_data)
  expect_identical(sd_base$time_line, sd_override$time_line)
  expect_identical(sd_base$stan_data$T, sd_base$stan_data_with_overrides$T)
  expect_identical(sd_base$time_line$time_line$date, sd_base$time_line_with_overrides$time_line$date)

  tl_expected <- time_line_with_overrides(
    model_time_range = mtr,
    time_scale = "week",
    time_scale_overrides = overrides
  )
  expected_delta_days <- tl_expected$time_line$delta_days
  expected_delta_days[is.na(expected_delta_days)] <- 0L
  expected_step_scale <- tl_expected$time_line$step_scale
  expected_step_scale[is.na(expected_step_scale)] <- 0

  expect_identical(sd_override$time_line_with_overrides, tl_expected)
  expect_gt(sd_override$stan_data_with_overrides$T, sd_override$stan_data$T)
  expect_identical(sd_override$stan_data_with_overrides$delta_days_t, as.array(expected_delta_days))
  expect_identical(sd_override$stan_data_with_overrides$step_scale_t, as.array(expected_step_scale))
})

test_that("parallel override-aware path respects known_state filtering", {
  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  set.seed(4711)
  spd <- simulate_polls(
    x = txdf,
    pd = pd_test,
    npolls = 60,
    time_scale = "week",
    start_date = "2010-01-01"
  )
  mtr <- time_range(spd)
  known_state <- tibble::tibble(
    date = as.Date(c(mtr["from"] - 7, mtr["from"] + 35)),
    x3 = c(0.25, 0.30),
    x4 = c(0.20, 0.22)
  )

  expect_silent(
    suppressWarnings(
      suppressMessages(
        sd <- stan_polls_data(
          x = spd,
          time_scale = "week",
          y_name = c("x3", "x4"),
          model = "model8k1",
          known_state = known_state,
          hyper_parameters = list(
            sigma_kappa_hyper = 0.01,
            use_multivariate_version = 0
          )
        )
      )
    )
  )

  expect_true(all(c("stan_data_with_overrides", "time_line_with_overrides") %in% names(sd)))
  expect_identical(sd$stan_data_with_overrides$T_known, sd$stan_data$T_known)
  expect_identical(sd$stan_data_with_overrides$x_known_t, sd$stan_data$x_known_t)
})

# TODO: Build own tests for stan data
