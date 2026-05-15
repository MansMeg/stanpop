context("model8m10 structural bridge")

if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

make_bridge_parser_case <- function() {
  time_line_with_overrides <- get_internal("time_line_with_overrides")
  tl <- time_line_with_overrides(
    model_time_range = time_range(c("2026-06-01", "2026-06-15")),
    time_scale = "week",
    time_scale_overrides = tibble::tibble(
      from = as.Date("2026-06-04"),
      to = as.Date("2026-06-08"),
      time_scale = "day"
    )
  )
  known_t <- get_time_points(tl, as.Date("2026-06-06"))
  delta_days_t <- as.array(ifelse(is.na(tl$time_line$delta_days), 0L, tl$time_line$delta_days))

  list(
    time_line = tl,
    y_name = c("M", "L", "C"),
    stan_data = list(
      T = nrow(tl$time_line),
      P = 3L,
      x_known_t = known_t,
      delta_days_t = delta_days_t
    )
  )
}

test_that("model8m10 is registered and uses the model8m data path", {
  supported_pop_models <- get_internal("supported_pop_models")
  expect_true("model8m10" %in% supported_pop_models())
  expect_true(get_internal("model_supports_time_scale_overrides")("model8m10"))

  case <- make_model8_mixed_smoke_case(npolls = 20)
  spd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = case$polls_data,
        y_name = case$parties,
        model = "model8m10",
        time_scale = case$time_scale,
        known_state = case$known_state
      )
    )
  )

  expect_s3_class(spd, "model8m")
  expect_true(all(c(
    "structural_bridge_type",
    "structural_bridge_active_t",
    "structural_bridge_B",
    "structural_bridge_party",
    "structural_bridge_delta_x",
    "structural_bridge_epsilon",
    "structural_bridge_sigma_scale"
  ) %in% names(spd$stan_data)))
})

test_that("model8m10 no-bridge defaults are Stan-ready", {
  case <- make_model8_mixed_smoke_case(npolls = 20)
  spd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = case$polls_data,
        y_name = case$parties,
        model = "model8m10",
        time_scale = case$time_scale,
        known_state = case$known_state
      )
    )
  )
  sd <- spd$stan_data

  expect_identical(sd$structural_bridge_type, 0L)
  expect_length(sd$structural_bridge_active_t, sd$T)
  expect_true(is.array(sd$structural_bridge_active_t))
  expect_true(all(sd$structural_bridge_active_t == 0L))
  expect_identical(sd$structural_bridge_B, 1L)
  expect_identical(sd$structural_bridge_party, as.array(1L))
  expect_identical(dim(sd$structural_bridge_delta_x), c(sd$T, 1L))
  expect_true(all(sd$structural_bridge_delta_x == 0))
  expect_length(sd$structural_bridge_sigma_scale, sd$P)
  expect_true(all(sd$structural_bridge_sigma_scale == 1))
})


test_that("direct structural bridge active vectors cannot cover known states", {
  case <- make_model8_mixed_smoke_case(npolls = 20)
  spd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = case$polls_data,
        y_name = case$parties,
        model = "model8m10",
        time_scale = case$time_scale,
        known_state = case$known_state
      )
    )
  )
  sd <- spd$stan_data
  active_t <- rep(0L, sd$T)
  active_t[sd$x_known_t[1]] <- 1L
  delta_x <- matrix(0.0, nrow = sd$T, ncol = 1L)
  delta_x[active_t == 1L, 1] <- 0.01

  expect_error(
    suppressWarnings(
      suppressMessages(
        stan_polls_data(
          x = case$polls_data,
          y_name = case$parties,
          model = "model8m10",
          time_scale = case$time_scale,
          known_state = case$known_state,
          hyper_parameters = list(
            structural_bridge_type = 1L,
            structural_bridge_active_t = active_t,
            structural_bridge_B = 1L,
            structural_bridge_party = 1L,
            structural_bridge_delta_x = delta_x,
            structural_bridge_sigma_scale = rep(1, sd$P)
          )
        )
      )
    ),
    "known states"
  )
})

test_that("model8m10 accepts high-level bridge hyperparameters in stan_polls_data", {
  case <- make_model8_mixed_smoke_case(npolls = 20)
  known_date <- case$known_state$date[1]
  hp <- list(
    structural_bridge_window = c(known_date - 28L, known_date - 7L),
    structural_bridge_x_drift = data.frame(
      y = case$parties[2],
      from_x = 0.025,
      to_x = 0.043
    ),
    structural_bridge_sigma_scale = stats::setNames(c(1, 0.5), case$parties)
  )

  spd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = case$polls_data,
        y_name = case$parties,
        model = "model8m10",
        time_scale = case$time_scale,
        time_scale_overrides = tibble::tibble(
          from = c(known_date - 28L, known_date - 1L),
          to = c(known_date - 7L, known_date + 1L),
          time_scale = c("day", "day")
        ),
        known_state = case$known_state,
        hyper_parameters = hp
      )
    )
  )
  sd <- spd$stan_data

  expect_identical(sd$structural_bridge_type, 1L)
  expect_true(any(sd$structural_bridge_active_t == 1L))
  expect_true(all(sd$structural_bridge_active_t[sd$x_known_t] == 0L))
  expect_equal(sum(sd$structural_bridge_delta_x[, 1]), 0.043 - 0.025)
  expect_equal(sd$structural_bridge_sigma_scale, c(1, 0.5))
})

test_that("high-level bridge windows containing known states keep those states inactive", {
  case <- make_model8_mixed_smoke_case(npolls = 20)
  known_date <- case$known_state$date[1]

  spd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = case$polls_data,
        y_name = case$parties,
        model = "model8m10",
        time_scale = case$time_scale,
        time_scale_overrides = tibble::tibble(
          from = known_date - 14L,
          to = known_date + 14L,
          time_scale = "day"
        ),
        known_state = case$known_state,
        hyper_parameters = list(
          structural_bridge_window = c(known_date - 14L, known_date + 14L),
          structural_bridge_x_drift = data.frame(
            y = case$parties[2],
            from_x = 0.025,
            to_x = 0.043
          )
        )
      )
    )
  )

  expect_true(all(spd$stan_data$structural_bridge_active_t[spd$stan_data$x_known_t] == 0L))
  expect_true(any(spd$stan_data$structural_bridge_active_t == 1L))
  expect_equal(sum(spd$stan_data$structural_bridge_delta_x[, 1]), 0.043 - 0.025)
})

test_that("structural bridge parser applies drift after from and through to", {
  parse_structural_bridge <- get_internal("parse_structural_bridge")
  case <- make_bridge_parser_case()
  hp <- list(
    structural_bridge_window = c(as.Date("2026-06-04"), as.Date("2026-06-08")),
    structural_bridge_x_drift = data.frame(
      y = "L",
      from_x = 0.025,
      to_x = 0.043
    )
  )

  res <- parse_structural_bridge(hp, case$time_line, case$y_name, case$stan_data)
  from_t <- get_time_points(case$time_line, as.Date("2026-06-04"))
  to_t <- get_time_points(case$time_line, as.Date("2026-06-08"))
  expected_active <- seq_len(case$stan_data$T) > from_t &
    seq_len(case$stan_data$T) <= to_t
  expected_active[case$stan_data$x_known_t] <- FALSE
  expected_active[case$stan_data$delta_days_t == 0L] <- FALSE
  expected_delta <- rep(0, case$stan_data$T)
  expected_delta[expected_active] <- (0.043 - 0.025) *
    case$stan_data$delta_days_t[expected_active] /
    sum(case$stan_data$delta_days_t[expected_active])

  expect_identical(res$structural_bridge_active_t, as.integer(expected_active))
  expect_identical(res$structural_bridge_B, 1L)
  expect_identical(res$structural_bridge_party, 2L)
  expect_equal(res$structural_bridge_delta_x[, 1], expected_delta)
  expect_equal(sum(res$structural_bridge_delta_x[, 1]), 0.043 - 0.025)
  expect_identical(res$structural_bridge_active_t[case$stan_data$x_known_t], 0L)
  expect_identical(res$structural_bridge_active_t[from_t], 0L)
  expect_identical(res$structural_bridge_active_t[to_t], 1L)
})

test_that("structural bridge parser normalizes named list x-drift input", {
  parse_structural_bridge <- get_internal("parse_structural_bridge")
  case <- make_bridge_parser_case()
  common_hp <- list(
    structural_bridge_window = c(as.Date("2026-06-04"), as.Date("2026-06-08"))
  )
  list_hp <- c(common_hp, list(
    structural_bridge_x_drift = list(
      y = c("L", "C"),
      from_x = c(0.026, 0.049),
      to_x = c(0.044, 0.054)
    )
  ))
  data_frame_hp <- c(common_hp, list(
    structural_bridge_x_drift = data.frame(
      y = c("L", "C"),
      from_x = c(0.026, 0.049),
      to_x = c(0.044, 0.054)
    )
  ))

  list_res <- parse_structural_bridge(list_hp, case$time_line, case$y_name, case$stan_data)
  data_frame_res <- parse_structural_bridge(data_frame_hp, case$time_line, case$y_name, case$stan_data)

  expect_identical(list_res$structural_bridge_party, data_frame_res$structural_bridge_party)
  expect_equal(list_res$structural_bridge_delta_x, data_frame_res$structural_bridge_delta_x)
  expect_identical(list_res$structural_bridge_party, c(2L, 3L))
})

test_that("structural bridge parser allows only one global bridge window", {
  parse_structural_bridge <- get_internal("parse_structural_bridge")
  case <- make_bridge_parser_case()

  expect_error(
    parse_structural_bridge(
      list(
        structural_bridge_window = data.frame(
          from = as.Date(c("2026-06-04", "2026-06-09")),
          to = as.Date(c("2026-06-08", "2026-06-15"))
        ),
        structural_bridge_x_drift = data.frame(
          y = "L",
          from_x = 0.025,
          to_x = 0.043
        )
      ),
      case$time_line,
      case$y_name,
      case$stan_data
    ),
    "rows"
  )

  expect_error(
    parse_structural_bridge(
      list(
        structural_bridge_window = c(as.Date("2026-06-04"), as.Date("2026-06-08")),
        structural_bridge_x_drift = data.frame(
          from = as.Date("2026-06-04"),
          to = as.Date("2026-06-08"),
          y = "L",
          from_x = 0.025,
          to_x = 0.043
        )
      ),
      case$time_line,
      case$y_name,
      case$stan_data
    ),
    "Do not include from/to"
  )

  hp <- list(
    structural_bridge_x_drift = data.frame(
      from = as.Date(c("2026-06-04", "2026-06-09")),
      to = as.Date(c("2026-06-08", "2026-06-15")),
      y = c("L", "C"),
      from_x = c(0.025, 0.30),
      to_x = c(0.043, 0.27)
    )
  )

  expect_error(
    parse_structural_bridge(hp, case$time_line, case$y_name, case$stan_data),
    "Only one global structural bridge window"
  )

  expect_error(
    parse_structural_bridge(
      list(
        structural_bridge_window = c(as.Date("2026-06-04"), as.Date("2026-06-08")),
        structural_bridge_x_drift = list(
          y = c("L", "C"),
          from_x = c(0.025, 0.30)
        )
      ),
      case$time_line,
      case$y_name,
      case$stan_data
    ),
    "Names"
  )
})

test_that("structural bridge window dates must be latent anchor dates", {
  parse_structural_bridge <- get_internal("parse_structural_bridge")
  case <- make_bridge_parser_case()
  hp <- list(
    structural_bridge_window = c(as.Date("2026-06-04"), as.Date("2026-06-09")),
    structural_bridge_x_drift = data.frame(
      y = "L",
      from_x = 0.025,
      to_x = 0.043
    )
  )

  expect_false(as.Date("2026-06-09") %in% case$time_line$time_line$date)
  expect_error(
    parse_structural_bridge(hp, case$time_line, case$y_name, case$stan_data),
    "Add time_scale_overrides.*2026-06-09"
  )
})

test_that("structural bridge sigma scale is ordered and validated", {
  parse_structural_bridge <- get_internal("parse_structural_bridge")
  assert_model_argument_value <- get_internal("assert_model_argument_value")
  case <- make_bridge_parser_case()

  named <- parse_structural_bridge(
    list(structural_bridge_sigma_scale = c(C = 3, M = 1, L = 0.5)),
    case$time_line,
    case$y_name,
    case$stan_data
  )
  unnamed <- parse_structural_bridge(
    list(structural_bridge_sigma_scale = c(1, 0.5, 3)),
    case$time_line,
    case$y_name,
    case$stan_data
  )

  expect_equal(named$structural_bridge_sigma_scale, c(1, 0.5, 3))
  expect_equal(unnamed$structural_bridge_sigma_scale, c(1, 0.5, 3))
  expect_error(
    parse_structural_bridge(
      list(structural_bridge_sigma_scale = c(M = 1, L = 0.5)),
      case$time_line,
      case$y_name,
      case$stan_data
    ),
    "Names"
  )
  expect_error(
    parse_structural_bridge(
      list(structural_bridge_sigma_scale = c(M = 1, L = 0.5, X = 1)),
      case$time_line,
      case$y_name,
      case$stan_data
    ),
    "Names"
  )
  expect_error(
    parse_structural_bridge(
      list(structural_bridge_sigma_scale = c(M = 1, L = 0, C = 1)),
      case$time_line,
      case$y_name,
      case$stan_data
    ),
    "at least 1e-12"
  )
  expect_error(
    parse_structural_bridge(
      list(structural_bridge_sigma_scale = c(1, 0, 3)),
      case$time_line,
      case$y_name,
      case$stan_data
    ),
    "at least 1e-12"
  )
  expect_error(
    assert_model_argument_value(
      "structural_bridge_sigma_scale",
      c(1, 0, 3),
      x = list(),
      stan_data = list(P = 3L)
    ),
    "at least 1e-12"
  )
})

test_that("reserved structural bridge types are rejected on the R side", {
  assert_model_argument_value <- get_internal("assert_model_argument_value")

  expect_error(
    assert_model_argument_value(
      "structural_bridge_type",
      2L,
      x = list(use_softmax = 1L),
      stan_data = list(T = 3L, P = 2L)
    ),
    "reserved"
  )
  expect_error(
    assert_model_argument_value(
      "structural_bridge_type",
      3L,
      x = list(use_softmax = 1L),
      stan_data = list(T = 3L, P = 2L)
    ),
    "reserved"
  )
})

test_that("model8m10 Stan model compiles with rstan", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()
  testthat::skip_if_not_installed("rstan")

  expect_silent(
    rstan::stan_model(file = get_pop_stan_model_file_path("model8m10"))
  )
})

test_that("model8m10 Stan model compiles with cmdstanr", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  tmp_stan_file <- tempfile("model8m10-", fileext = ".stan")
  tmp_model_root <- tools::file_path_sans_ext(tmp_stan_file)
  on.exit(unlink(Sys.glob(paste0(tmp_model_root, "*"))), add = TRUE)
  expect_true(file.copy(get_pop_stan_model_file_path("model8m10"), tmp_stan_file))

  expect_silent(
    cmdstan_model <- cmdstanr::cmdstan_model(
      stan_file = tmp_stan_file,
      force_recompile = TRUE
    )
  )
  expect_true(inherits(cmdstan_model, "CmdStanModel"))
})

test_that("positive bridge drift raises and tightens the pushed party", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_model8_mixed_smoke_case(npolls = 20)
  bridge_from <- case$known_state$date[1] - 56L
  bridge_to <- case$known_state$date[1] - 7L
  target_date <- bridge_to - 7L
  base_cfg <- list(
    use_industry_bias = 0L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L,
    election_period = list(c("2010-05-03", "2010-05-20")),
    use_sigma_ep = 2L,
    ep_inv_x = list(c(3.984064, 3.937008))
  )
  bridge_drift <- data.frame(
    y = "x4",
    from_x = 0.02,
    to_x = 0.12
  )

  fit_pop <- function(cfg, seed) {
    suppressWarnings(
      suppressMessages(
        poll_of_polls(
          y = case$parties,
          model = "model8m10",
          polls_data = case$polls_data,
          time_scale = case$time_scale,
          time_scale_overrides = tibble::tibble(
            from = bridge_from,
            to = bridge_to,
            time_scale = "day"
          ),
          known_state = case$known_state,
          hyper_parameters = cfg,
          iter = 80,
          warmup = 40,
          chains = 1,
          init = "random",
          refresh = 0,
          seed = seed,
          cache_dir = NULL
        )
      )
    )
  }

  no_bridge <- fit_pop(base_cfg, 501)
  loose_bridge <- fit_pop(c(base_cfg, list(
    structural_bridge_window = c(bridge_from, bridge_to),
    structural_bridge_x_drift = bridge_drift,
    structural_bridge_sigma_scale = c(x3 = 1, x4 = 1)
  )), 502)
  tight_bridge <- fit_pop(c(base_cfg, list(
    structural_bridge_window = c(bridge_from, bridge_to),
    structural_bridge_x_drift = bridge_drift,
    structural_bridge_sigma_scale = c(x3 = 1, x4 = 0.2)
  )), 503)

  target_t <- get_time_points(loose_bridge, target_date)
  no_bridge_l <- latent_state(no_bridge)$latent_state[, target_t, "x4"]
  loose_l <- latent_state(loose_bridge)$latent_state[, target_t, "x4"]
  tight_l <- latent_state(tight_bridge)$latent_state[, target_t, "x4"]

  expect_gt(mean(loose_l), mean(no_bridge_l))
  expect_lt(stats::sd(tight_l), stats::sd(loose_l))
})
