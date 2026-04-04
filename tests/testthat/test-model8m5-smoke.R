context("model8m5 smoke")

if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

test_that("model8m5 poll_of_polls runs on a mixed latent grid", {
  skip_if_no_stan_tests()

  case <- make_model8_mixed_smoke_case()
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
          pop <- poll_of_polls(
            y = case$parties,
            model = "model8m5",
            polls_data = case$polls_data,
            time_scale = case$time_scale,
            time_scale_overrides = case$time_scale_overrides,
            known_state = case$known_state,
            hyper_parameters = cfg,
            iter = 20,
            warmup = 10,
            chains = 1,
            refresh = 0,
            seed = 4711,
            cache_dir = NULL
          )
        )
      )
    )
  )

  expect_identical(pop$model, "model8m5")
  expect_identical(pop$time_scale_overrides, case$time_scale_overrides)
  expect_identical(pop$stan_data$stan_data$T, pop$stan_data$stan_data_with_overrides$T)
  expect_true(any(abs(pop$stan_data$stan_data$step_scale_t[-1] - 1) > 1e-12))
  expect_true(any(pop$time_line$time_line$date %in% seq(as.Date("2010-05-05"), as.Date("2010-05-10"), by = 1)))
  expect_equal(get_ndraws(pop), 10)

  sigma_x_draws <- rstan::extract(pop$stan_fit, pars = "sigma_x")$sigma_x
  sigma_ep_draws <- rstan::extract(pop$stan_fit, pars = "sigma_ep")$sigma_ep
  lp_draws <- rstan::extract(pop$stan_fit, pars = "lp__")$lp__

  expect_true(all(is.finite(sigma_x_draws)))
  expect_true(all(is.finite(sigma_ep_draws)))
  expect_true(all(is.finite(lp_draws)))
})
