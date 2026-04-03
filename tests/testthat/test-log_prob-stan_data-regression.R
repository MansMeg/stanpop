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

fit_from_stan_data <- function(model, cfg, case) {
  get_pop_stan_model_file_path <- get_internal("get_pop_stan_model_file_path")
  sd <- stan_polls_data(
    x = case$polls_data,
    time_scale = case$time_scale,
    y_name = case$parties,
    model = model,
    known_state = case$known_state,
    hyper_parameters = cfg
  )

  fit <- rstan::stan(
    file = get_pop_stan_model_file_path(model),
    data = sd$stan_data,
    warmup = 0,
    iter = 3,
    chains = 1,
    seed = 4711,
    refresh = 0
  )

  list(stan_data = sd, stan_fit = fit)
}

test_that("model8k2 log_prob is stable for a simple stan_data test case", {
  skip_if_no_stan_tests()

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
