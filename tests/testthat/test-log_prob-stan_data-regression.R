context("log_prob stan_data regression")

# Run the line below to run different test suites locally
# See documentation for details.
# Sys.setenv(STANPOP_RUN_STAN_TESTS = "true")
# Sys.setenv(STANPOP_RUN_STAN_TESTS = "false")
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}
run_stan_tests <- FALSE
if(run_stan_tests) Sys.setenv(STANPOP_RUN_STAN_TESTS = "true")


make_simple_log_prob_regression_case <- function() {
  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(
    date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1)
  )
  known_state <- cbind(known_state, txdf[true_idx, ])

  spd <- simulate_polls(
    x = txdf,
    pd = pd_test,
    npolls = 40,
    time_scale = time_scale,
    start_date = "2010-01-01"
  )

  list(
    polls_data = spd,
    known_state = known_state,
    parties = parties,
    time_scale = time_scale
  )
}

make_simple_mixed_log_prob_regression_case <- function() {
  case <- make_simple_log_prob_regression_case()
  case$time_scale_overrides <- tibble::tibble(
    from = as.Date("2010-05-05"),
    to = as.Date("2010-05-10"),
    time_scale = "day"
  )
  case
}

fit_from_stan_data <- function(model, cfg, case, stan_data_name = "stan_data") {
  get_pop_stan_model_file_path <- get_internal("get_pop_stan_model_file_path")
  sd <- stan_polls_data(
    x = case$polls_data,
    time_scale = case$time_scale,
    time_scale_overrides = case$time_scale_overrides,
    y_name = case$parties,
    model = model,
    known_state = case$known_state,
    hyper_parameters = cfg
  )

  fit <- rstan::stan(
    file = get_pop_stan_model_file_path(model),
    data = sd[[stan_data_name]],
    warmup = 0,
    iter = 3,
    chains = 1,
    seed = 4711,
    refresh = 0
  )

  list(stan_data = sd, stan_fit = fit)
}

make_stan_data_case <- function(model, cfg, case) {
  stan_polls_data(
    x = case$polls_data,
    time_scale = case$time_scale,
    time_scale_overrides = case$time_scale_overrides,
    y_name = case$parties,
    model = model,
    known_state = case$known_state,
    hyper_parameters = cfg
  )
}

expect_log_prob_match <- function(lhs,
                                  rhs,
                                  lhs_label = "lhs",
                                  rhs_label = "rhs") {
  nu_lhs <- rstan::get_num_upars(lhs$stan_fit)
  nu_rhs <- rstan::get_num_upars(rhs$stan_fit)
  probe <- seq(from = -0.15, to = 0.15, length.out = nu_lhs)

  expect_equal(nu_rhs, nu_lhs, label = paste(rhs_label, "upars"))
  expect_equal(
    rstan::log_prob(lhs$stan_fit, rep(0, nu_lhs)),
    rstan::log_prob(rhs$stan_fit, rep(0, nu_rhs)),
    tolerance = 1e-8,
    label = paste(lhs_label, "vs", rhs_label, "zero log_prob")
  )
  expect_equal(
    rstan::log_prob(lhs$stan_fit, probe),
    rstan::log_prob(rhs$stan_fit, probe),
    tolerance = 1e-8,
    label = paste(lhs_label, "vs", rhs_label, "probe log_prob")
  )
}

test_that("model8k2 log_prob is stable for a simple stan_data test case", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          res <- fit_from_stan_data(
            model = "model8k2",
            cfg = cfg,
            case = case
          )
        )
      )
    )
  )

  nu <- rstan::get_num_upars(res$stan_fit)
  probe <- seq(from = -0.15, to = 0.15, length.out = nu)
  lp_zero <- rstan::log_prob(res$stan_fit, rep(0, nu))
  lp_probe <- rstan::log_prob(res$stan_fit, probe)

  expect_equal(nu, 207L)
  expect_equal(lp_zero, -1355.345663167916, tolerance = 1e-8)
  expect_equal(lp_probe, -50758.401185041454, tolerance = 1e-8)
})

test_that("model8m2 log_prob is stable for a simple stan_data test case", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper_sd = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L,
    election_period = list(c("2010-05-03", "2010-05-20")),
    use_sigma_ep = 2L,
    ep_inv_x = list(c(3.984064, 3.937008))
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          res <- fit_from_stan_data(
            model = "model8m2",
            cfg = cfg,
            case = case
          )
        )
      )
    )
  )

  nu <- rstan::get_num_upars(res$stan_fit)
  probe <- seq(from = -0.15, to = 0.15, length.out = nu)
  lp_zero <- rstan::log_prob(res$stan_fit, rep(0, nu))
  lp_probe <- rstan::log_prob(res$stan_fit, probe)

  expect_equal(nu, 209L)
  expect_equal(lp_zero, -1357.1835402343254, tolerance = 1e-8)
  expect_equal(lp_probe, -48611.707326402902, tolerance = 1e-8)
})

test_that("model8k2 log_prob matches between legacy and override-aware stan_data without overrides", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  sd <- suppressWarnings(
    suppressMessages(
      make_stan_data_case(
        model = "model8k2",
        cfg = cfg,
        case = case
      )
    )
  )
  has_all_legacy_fields <- all(names(sd$stan_data) %in% names(sd$stan_data_with_overrides))
  expect_true(has_all_legacy_fields)
  if(!has_all_legacy_fields) return(invisible())

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          legacy <- fit_from_stan_data(
            model = "model8k2",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data"
          )
        )
      )
    )
  )
  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          override <- fit_from_stan_data(
            model = "model8k2",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )

  nu_legacy <- rstan::get_num_upars(legacy$stan_fit)
  nu_override <- rstan::get_num_upars(override$stan_fit)
  probe <- seq(from = -0.15, to = 0.15, length.out = nu_legacy)

  expect_equal(nu_override, nu_legacy)
  expect_equal(
    rstan::log_prob(legacy$stan_fit, rep(0, nu_legacy)),
    rstan::log_prob(override$stan_fit, rep(0, nu_override)),
    tolerance = 1e-8
  )
  expect_equal(
    rstan::log_prob(legacy$stan_fit, probe),
    rstan::log_prob(override$stan_fit, probe),
    tolerance = 1e-8
  )
})

test_that("model8m2 log_prob matches between legacy and override-aware stan_data without overrides", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper_sd = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L,
    election_period = list(c("2010-05-03", "2010-05-20")),
    use_sigma_ep = 2L,
    ep_inv_x = list(c(3.984064, 3.937008))
  )

  sd <- suppressWarnings(
    suppressMessages(
      make_stan_data_case(
        model = "model8m2",
        cfg = cfg,
        case = case
      )
    )
  )
  has_all_legacy_fields <- all(names(sd$stan_data) %in% names(sd$stan_data_with_overrides))
  expect_true(has_all_legacy_fields)
  if(!has_all_legacy_fields) return(invisible())

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          legacy <- fit_from_stan_data(
            model = "model8m2",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data"
          )
        )
      )
    )
  )
  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          override <- fit_from_stan_data(
            model = "model8m2",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )

  nu_legacy <- rstan::get_num_upars(legacy$stan_fit)
  nu_override <- rstan::get_num_upars(override$stan_fit)
  probe <- seq(from = -0.15, to = 0.15, length.out = nu_legacy)

  expect_equal(nu_override, nu_legacy)
  expect_equal(
    rstan::log_prob(legacy$stan_fit, rep(0, nu_legacy)),
    rstan::log_prob(override$stan_fit, rep(0, nu_override)),
    tolerance = 1e-8
  )
  expect_equal(
    rstan::log_prob(legacy$stan_fit, probe),
    rstan::log_prob(override$stan_fit, probe),
    tolerance = 1e-8
  )
})

test_that("model8k5 log_prob matches model8k2 on legacy stan_data without overrides", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8k2 <- fit_from_stan_data(
            model = "model8k2",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data"
          )
        )
      )
    )
  )
  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8k5 <- fit_from_stan_data(
            model = "model8k5",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data"
          )
        )
      )
    )
  )

  expect_log_prob_match(
    lhs = model8k2,
    rhs = model8k5,
    lhs_label = "model8k2",
    rhs_label = "model8k5"
  )
})

test_that("model8k5 log_prob matches model8k2 when fed override-aware stan_data without overrides", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  sd <- suppressWarnings(
    suppressMessages(
      make_stan_data_case(
        model = "model8k5",
        cfg = cfg,
        case = case
      )
    )
  )
  has_all_legacy_fields <- all(names(sd$stan_data) %in% names(sd$stan_data_with_overrides))
  expect_true(has_all_legacy_fields)
  if(!has_all_legacy_fields) return(invisible())

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8k2 <- fit_from_stan_data(
            model = "model8k2",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data"
          )
        )
      )
    )
  )
  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8k5 <- fit_from_stan_data(
            model = "model8k5",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )

  expect_log_prob_match(
    lhs = model8k2,
    rhs = model8k5,
    lhs_label = "model8k2 legacy",
    rhs_label = "model8k5 override-aware"
  )
})

test_that("model8m5 log_prob matches model8m2 on legacy stan_data without overrides", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper_sd = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L,
    election_period = list(c("2010-05-03", "2010-05-20")),
    use_sigma_ep = 2L,
    ep_inv_x = list(c(3.984064, 3.937008))
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8m2 <- fit_from_stan_data(
            model = "model8m2",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data"
          )
        )
      )
    )
  )
  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8m5 <- fit_from_stan_data(
            model = "model8m5",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data"
          )
        )
      )
    )
  )

  expect_log_prob_match(
    lhs = model8m2,
    rhs = model8m5,
    lhs_label = "model8m2",
    rhs_label = "model8m5"
  )
})

test_that("model8m5 log_prob matches model8m2 when fed override-aware stan_data without overrides", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper_sd = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L,
    election_period = list(c("2010-05-03", "2010-05-20")),
    use_sigma_ep = 2L,
    ep_inv_x = list(c(3.984064, 3.937008))
  )

  sd <- suppressWarnings(
    suppressMessages(
      make_stan_data_case(
        model = "model8m5",
        cfg = cfg,
        case = case
      )
    )
  )
  has_all_legacy_fields <- all(names(sd$stan_data) %in% names(sd$stan_data_with_overrides))
  expect_true(has_all_legacy_fields)
  if(!has_all_legacy_fields) return(invisible())

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8m2 <- fit_from_stan_data(
            model = "model8m2",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data"
          )
        )
      )
    )
  )
  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8m5 <- fit_from_stan_data(
            model = "model8m5",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )

  expect_log_prob_match(
    lhs = model8m2,
    rhs = model8m5,
    lhs_label = "model8m2 legacy",
    rhs_label = "model8m5 override-aware"
  )
})

test_that("model8k5 should differ from model8k2 on mixed override-aware stan_data", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_mixed_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8k2 <- fit_from_stan_data(
            model = "model8k2",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )
  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8k5 <- fit_from_stan_data(
            model = "model8k5",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )

  nu_k2 <- rstan::get_num_upars(model8k2$stan_fit)
  nu_k5 <- rstan::get_num_upars(model8k5$stan_fit)
  probe <- seq(from = -0.15, to = 0.15, length.out = nu_k2)
  lp_zero_k2 <- rstan::log_prob(model8k2$stan_fit, rep(0, nu_k2))
  lp_zero_k5 <- rstan::log_prob(model8k5$stan_fit, rep(0, nu_k5))
  lp_probe_k2 <- rstan::log_prob(model8k2$stan_fit, probe)
  lp_probe_k5 <- rstan::log_prob(model8k5$stan_fit, probe)

  expect_equal(nu_k5, nu_k2)
  expect_false(isTRUE(all.equal(lp_zero_k2, lp_zero_k5, tolerance = 1e-8)) &&
                 isTRUE(all.equal(lp_probe_k2, lp_probe_k5, tolerance = 1e-8)))
})

test_that("model8m5 should differ from model8m2 on mixed override-aware stan_data", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_mixed_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper_sd = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L,
    election_period = list(c("2010-05-03", "2010-05-20")),
    use_sigma_ep = 2L,
    ep_inv_x = list(c(3.984064, 3.937008))
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8m2 <- fit_from_stan_data(
            model = "model8m2",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )
  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8m5 <- fit_from_stan_data(
            model = "model8m5",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )

  nu_m2 <- rstan::get_num_upars(model8m2$stan_fit)
  nu_m5 <- rstan::get_num_upars(model8m5$stan_fit)
  probe <- seq(from = -0.15, to = 0.15, length.out = nu_m2)
  lp_zero_m2 <- rstan::log_prob(model8m2$stan_fit, rep(0, nu_m2))
  lp_zero_m5 <- rstan::log_prob(model8m5$stan_fit, rep(0, nu_m5))
  lp_probe_m2 <- rstan::log_prob(model8m2$stan_fit, probe)
  lp_probe_m5 <- rstan::log_prob(model8m5$stan_fit, probe)

  expect_equal(nu_m5, nu_m2)
  expect_false(isTRUE(all.equal(lp_zero_m2, lp_zero_m5, tolerance = 1e-8)) &&
                 isTRUE(all.equal(lp_probe_m2, lp_probe_m5, tolerance = 1e-8)))
})

test_that("model8m10 log_prob matches model8m5 on mixed override-aware stan_data", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_mixed_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper_sd = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L,
    election_period = list(c("2010-05-03", "2010-05-20")),
    use_sigma_ep = 2L,
    ep_inv_x = list(c(3.984064, 3.937008))
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8m5 <- fit_from_stan_data(
            model = "model8m5",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )
  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8m10 <- fit_from_stan_data(
            model = "model8m10",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )

  expect_log_prob_match(
    lhs = model8m5,
    rhs = model8m10,
    lhs_label = "model8m5",
    rhs_label = "model8m10"
  )
})

test_that("model8m11 log_prob differs from model8m10 with x-scale election sigma", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_simple_mixed_log_prob_regression_case()
  cfg <- list(
    sigma_kappa_hyper_sd = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L,
    election_period = list(c("2010-05-03", "2010-05-20")),
    use_sigma_ep = 2L,
    ep_inv_x = list(c(3.984064, 3.937008)),
    structural_bridge_type = "x_drift",
    structural_bridge_window = c("2010-05-05", "2010-05-10"),
    structural_bridge_x_target_path = data.frame(
      y = "x3",
      from_x = 0.30,
      to_x = 0.34
    ),
    structural_bridge_sigma_scale = c(x3 = 1, x4 = 0.8)
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8m10 <- fit_from_stan_data(
            model = "model8m10",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )
  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          model8m11 <- fit_from_stan_data(
            model = "model8m11",
            cfg = cfg,
            case = case,
            stan_data_name = "stan_data_with_overrides"
          )
        )
      )
    )
  )

  expect_equal(
    model8m11$stan_data$stan_data_with_overrides,
    model8m10$stan_data$stan_data_with_overrides
  )

  nu_m10 <- rstan::get_num_upars(model8m10$stan_fit)
  nu_m11 <- rstan::get_num_upars(model8m11$stan_fit)
  probe <- seq(from = -0.15, to = 0.15, length.out = nu_m10)

  expect_equal(nu_m11, nu_m10)
  expect_false(isTRUE(all.equal(
    rstan::log_prob(model8m10$stan_fit, probe),
    rstan::log_prob(model8m11$stan_fit, probe),
    tolerance = 1e-8
  )))
})


if(run_stan_tests) Sys.setenv(STANPOP_RUN_STAN_TESTS = "false")
