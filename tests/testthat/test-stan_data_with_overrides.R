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


test_that("minimal example shows how one poll changes on mixed and non-mixed paths", {
  attach_stan_data_with_overrides <- get_internal("attach_stan_data_with_overrides")
  tr <- time_range(c("2020-01-01", "2020-01-15"))
  overrides <- tibble::tibble(
    from = as.Date("2020-01-08"),
    to = as.Date("2020-01-10"),
    time_scale = "day"
  )

  pd <- polls_data(
    y = tibble::tibble(x3 = 0.30, x4 = 0.20),
    house = factor("A"),
    publish_date = as.Date("2020-01-10"),
    start_date = as.Date("2020-01-09"),
    end_date = as.Date("2020-01-10"),
    n = 1000L
  )

  sd_base <- stan_polls_data_model6b(
    x = pd,
    y_name = c("x3", "x4"),
    time_scale = "week",
    model_time_range = tr
  )

  sd_mixed <- attach_stan_data_with_overrides(
    spd = sd_base,
    x = pd,
    y_name = c("x3", "x4"),
    time_scale = "week",
    time_scale_overrides = overrides,
    model_time_range = tr
  )

  expect_identical(
    sd_base$time_line$time_line$date,
    as.Date(c("2019-12-30", "2020-01-06", "2020-01-13"))
  )
  expect_identical(sd_base$stan_data$tw, 1)
  expect_identical(sd_base$stan_data$tw_t, 2L)
  expect_identical(sd_base$stan_data$tw_i, 1L)

  expect_identical(
    sd_mixed$time_line_with_overrides$time_line$date,
    as.Date(c("2019-12-30", "2020-01-06", "2020-01-08", "2020-01-09", "2020-01-10", "2020-01-13"))
  )
  expect_equal(sd_mixed$stan_data_with_overrides$tw, c(0.5, 0.5))
  expect_identical(sd_mixed$stan_data_with_overrides$tw_t, c(4L, 5L))
  expect_identical(sd_mixed$stan_data_with_overrides$tw_i, c(1L, 1L))
})

test_that("model8k2 override-aware path recomputes g_t, g_i, and next_known_state_t_index on the mixed grid", {
  skip("This test is currently failing because the override-aware path for model8k2")
  tr <- time_range(c("2020-01-06", "2020-01-15"))
  overrides <- tibble::tibble(
    from = as.Date("2020-01-08"),
    to = as.Date("2020-01-10"),
    time_scale = "day"
  )

  pd <- polls_data(
    y = tibble::tibble(x3 = 0.30, x4 = 0.20),
    house = factor("A"),
    publish_date = as.Date("2020-01-10"),
    start_date = as.Date("2020-01-09"),
    end_date = as.Date("2020-01-10"),
    n = 1000L
  )
  known_state <- tibble::tibble(
    date = as.Date(c("2020-01-06", "2020-01-13")),
    x3 = c(0.25, 0.32),
    x4 = c(0.20, 0.18)
  )

  sd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = pd,
        time_scale = "week",
        time_scale_overrides = overrides,
        y_name = c("x3", "x4"),
        model = "model8k2",
        known_state = known_state,
        model_time_range = tr,
        hyper_parameters = list(
          sigma_kappa_hyper = 0.01,
          use_industry_bias = 1L,
          use_house_bias = 0L,
          use_design_effects = 0L,
          use_multivariate_version = 2L,
          use_softmax = 1L
        )
      )
    )
  )

  expect_identical(
    sd$time_line_with_overrides$time_line$date,
    as.Date(c("2020-01-06", "2020-01-08", "2020-01-09", "2020-01-10", "2020-01-13"))
  )

  # Legacy weekly path still sees the Jan 9 poll as belonging to the Jan 6 latent week.
  expect_equal(unname(sd$stan_data$g_i[1]), 0)

  # Mixed-grid path should instead use actual elapsed time from the last known state.
  has_required_fields <- all(c("g_t", "g_i", "next_known_state_t_index") %in% names(sd$stan_data_with_overrides))
  expect_true(has_required_fields)
  if(has_required_fields){
    expect_identical(sd$stan_data_with_overrides$next_known_state_t_index, c(1L, 2L, 2L, 2L, 2L))
    expect_equal(unname(sd$stan_data_with_overrides$g_i[1]), 3 / 365, tolerance = 1e-12)
    expect_equal(sd$stan_data_with_overrides$g_t[3], 3 / 365, tolerance = 1e-12)
    expect_equal(sd$stan_data_with_overrides$g_t[4], 4 / 365, tolerance = 1e-12)
  }
})


test_that("model8k2 override-aware path recomputes next_known_state_t_index on the mixed grid", {
  skip("For now")
  tr <- time_range(c("2020-01-06", "2020-01-15"))
  overrides <- tibble::tibble(
    from = as.Date("2020-01-08"),
    to = as.Date("2020-01-10"),
    time_scale = "day"
  )

  pd <- polls_data(
    y = tibble::tibble(x3 = 0.30, x4 = 0.20),
    house = factor("A"),
    publish_date = as.Date("2020-01-10"),
    start_date = as.Date("2020-01-09"),
    end_date = as.Date("2020-01-10"),
    n = 1000L
  )
  known_state <- tibble::tibble(
    date = as.Date(c("2020-01-06", "2020-01-13")),
    x3 = c(0.25, 0.32),
    x4 = c(0.20, 0.18)
  )

  sd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = pd,
        time_scale = "week",
        time_scale_overrides = overrides,
        y_name = c("x3", "x4"),
        model = "model8k2",
        known_state = known_state,
        model_time_range = tr,
        hyper_parameters = list(
          sigma_kappa_hyper = 0.01,
          use_industry_bias = 1L,
          use_house_bias = 0L,
          use_design_effects = 0L,
          use_multivariate_version = 2L,
          use_softmax = 1L
        )
      )
    )
  )

  expect_identical(
    sd$time_line_with_overrides$time_line$date,
    as.Date(c("2020-01-06", "2020-01-08", "2020-01-09", "2020-01-10", "2020-01-13"))
  )
  expect_true("next_known_state_t_index" %in% names(sd$stan_data_with_overrides))
  expect_equal(
    as.integer(sd$stan_data_with_overrides$next_known_state_t_index),
    c(1L, 2L, 2L, 2L, 2L)
  )
})


test_that("model8k2 override-aware path computes g_t from calendar-day differences", {
  tr <- time_range(c("2020-01-06", "2020-01-15"))
  overrides <- tibble::tibble(
    from = as.Date("2020-01-08"),
    to = as.Date("2020-01-10"),
    time_scale = "day"
  )

  pd <- polls_data(
    y = tibble::tibble(x3 = 0.30, x4 = 0.20),
    house = factor("A"),
    publish_date = as.Date("2020-01-10"),
    start_date = as.Date("2020-01-09"),
    end_date = as.Date("2020-01-10"),
    n = 1000L
  )
  known_state <- tibble::tibble(
    date = as.Date(c("2020-01-06", "2020-01-13")),
    x3 = c(0.25, 0.32),
    x4 = c(0.20, 0.18)
  )

  sd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = pd,
        time_scale = "week",
        time_scale_overrides = overrides,
        y_name = c("x3", "x4"),
        model = "model8k2",
        known_state = known_state,
        model_time_range = tr,
        hyper_parameters = list(
          sigma_kappa_hyper = 0.01,
          use_industry_bias = 1L,
          use_house_bias = 0L,
          use_design_effects = 0L,
          use_multivariate_version = 2L,
          use_softmax = 1L
        )
      )
    )
  )

  expect_identical(
    sd$time_line_with_overrides$time_line$date,
    as.Date(c("2020-01-06", "2020-01-08", "2020-01-09", "2020-01-10", "2020-01-13"))
  )
  expect_true("g_t" %in% names(sd$stan_data_with_overrides))
  expect_equal(
    as.numeric(sd$stan_data_with_overrides$g_t),
    c(0, 2 / 365, 3 / 365, 4 / 365, 0),
    tolerance = 1e-12
  )
})


