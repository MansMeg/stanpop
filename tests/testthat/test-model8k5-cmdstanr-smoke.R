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
