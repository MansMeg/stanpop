context("model8k5 cmdstanr smoke")

if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

test_that("model8k5 poll_of_polls runs with cmdstanr backend on a mixed latent grid", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr()

  case <- make_model8_mixed_smoke_case(npolls = 12)
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
          pop <- poll_of_polls(
            y = case$parties,
            model = "model8k5",
            polls_data = case$polls_data,
            time_scale = case$time_scale,
            time_scale_overrides = case$time_scale_overrides,
            known_state = case$known_state,
            hyper_parameters = cfg,
            backend = "cmdstanr",
            iter_sampling = 10,
            iter_warmup = 10,
            chains = 1,
            refresh = 0,
            seed = 4711,
            cache_dir = NULL
          )
        )
      )
    )
  )

  expect_identical(pop$model, "model8k5")
  expect_identical(pop$backend, "cmdstanr")
  expect_identical(pop$time_scale_overrides, case$time_scale_overrides)
  expect_identical(pop$input_args$time_scale_overrides, case$time_scale_overrides)
  expect_true(any(abs(pop$stan_data$stan_data$step_scale_t[-1] - 1) > 1e-12))
  expect_true(any(pop$time_line$time_line$date %in% seq(as.Date("2010-05-05"), as.Date("2010-05-10"), by = 1)))
  expect_equal(get_ndraws(pop), 10)

  ls <- latent_state(pop)
  expect_identical(dim(ls$latent_state)[2], nrow(pop$time_line$time_line))
  expect_true(all(is.finite(ls$latent_state)))

  sigma_x_draws <- extract(pop, pars = "sigma_x")$sigma_x
  x_pred_draws <- extract(pop, pars = "x_pred")$x_pred

  expect_true(all(is.finite(sigma_x_draws)))
  expect_true(all(is.finite(x_pred_draws)))
  expect_true(get_num_upars(pop) > 0)
})

test_that("model8k5 gives the same parameter space and log_prob with rstan and cmdstanr", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr()

  case <- make_model8_mixed_smoke_case(npolls = 12)
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  fit_args <- list(
    y = case$parties,
    model = "model8k5",
    polls_data = case$polls_data,
    time_scale = case$time_scale,
    time_scale_overrides = case$time_scale_overrides,
    known_state = case$known_state,
    hyper_parameters = cfg,
    chains = 1,
    refresh = 0,
    seed = 4711,
    cache_dir = NULL
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop_rstan <- do.call(
            poll_of_polls,
            c(fit_args, list(
              backend = "rstan",
              iter = 10,
              warmup = 5
            ))
          )
        )
      )
    )
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop_cmdstanr <- do.call(
            poll_of_polls,
            c(fit_args, list(
              backend = "cmdstanr",
              iter_sampling = 5,
              iter_warmup = 5
            ))
          )
        )
      )
    )
  )

  expect_identical(pop_rstan$time_scale_overrides, pop_cmdstanr$time_scale_overrides)
  expect_identical(pop_rstan$time_line$time_line, pop_cmdstanr$time_line$time_line)
  expect_equal(
    pop_rstan$stan_data$stan_data$step_scale_t,
    pop_cmdstanr$stan_data$stan_data$step_scale_t,
    tolerance = 0
  )

  expect_identical(parameter_names(pop_rstan), parameter_names(pop_cmdstanr))
  expect_identical(
    parameter_names(pop_rstan, rm_idx = TRUE),
    parameter_names(pop_cmdstanr, rm_idx = TRUE)
  )

  nu_rstan <- get_num_upars(pop_rstan)
  nu_cmdstanr <- get_num_upars(pop_cmdstanr)
  probe <- seq(from = -0.15, to = 0.15, length.out = nu_rstan)

  expect_identical(nu_rstan, nu_cmdstanr)
  expect_equal(
    log_prob(pop_rstan, rep(0, nu_rstan)),
    log_prob(pop_cmdstanr, rep(0, nu_cmdstanr)),
    tolerance = 1e-8
  )
  expect_equal(
    log_prob(pop_rstan, probe),
    log_prob(pop_cmdstanr, probe),
    tolerance = 1e-8
  )

  sigma_x_rstan <- extract(pop_rstan, pars = "sigma_x")$sigma_x
  sigma_x_cmdstanr <- extract(pop_cmdstanr, pars = "sigma_x")$sigma_x
  x_pred_rstan <- extract(pop_rstan, pars = "x_pred")$x_pred
  x_pred_cmdstanr <- extract(pop_cmdstanr, pars = "x_pred")$x_pred

  expect_identical(dim(sigma_x_rstan), dim(sigma_x_cmdstanr))
  expect_identical(dim(x_pred_rstan), dim(x_pred_cmdstanr))
})
