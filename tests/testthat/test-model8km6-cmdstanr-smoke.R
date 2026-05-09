context("model8k6/model8m6 cmdstanr smoke")

if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

expect_model8_cmdstanr_smoke <- function(model, hyper_parameters, draws = 5L) {
  skip_if_no_cmdstanr_tests()

  case <- make_model8_mixed_smoke_case(npolls = 8)
  known_date <- case$known_state$date[1]
  case$time_scale_overrides <- tibble::tibble(
    from = known_date - 1L,
    to = known_date + 1L,
    time_scale = "day"
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop <- poll_of_polls(
            y = case$parties,
            model = model,
            polls_data = case$polls_data,
            time_scale = case$time_scale,
            time_scale_overrides = case$time_scale_overrides,
            known_state = case$known_state,
            hyper_parameters = hyper_parameters,
            backend = "cmdstanr",
            iter_sampling = draws,
            iter_warmup = draws,
            chains = 1,
            refresh = 0,
            seed = 4711,
            cache_dir = NULL
          )
        )
      )
    )
  )

  expect_identical(pop$model, model)
  expect_identical(pop$backend, "cmdstanr")
  expect_equal(get_ndraws(pop), draws)
  expect_true(all(c("delta_days_t", "step_scale_t") %in% names(pop$stan_data$stan_data)))
  expect_true(any(abs(pop$stan_data$stan_data$step_scale_t[-1] - 1) > 1e-12))
  expect_true(get_num_upars(pop) > 0)

  x_pred_draws <- extract(pop, pars = "x_pred")$x_pred
  sigma_x_draws <- extract(pop, pars = "sigma_x")$sigma_x
  ls <- latent_state(pop)

  expect_true(all(is.finite(x_pred_draws)))
  expect_true(all(is.finite(sigma_x_draws)))
  expect_true(all(is.finite(ls$latent_state)))

  invisible(pop)
}

test_that("model8k6 compiles and runs with cmdstanr on testdata", {
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  expect_model8_cmdstanr_smoke("model8k6", cfg)
})

test_that("model8m6 compiles and runs with cmdstanr on testdata", {
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

  pop <- expect_model8_cmdstanr_smoke("model8m6", cfg)
  sigma_ep_draws <- extract(pop, pars = "sigma_ep")$sigma_ep
  expect_true(all(is.finite(sigma_ep_draws)))
})
