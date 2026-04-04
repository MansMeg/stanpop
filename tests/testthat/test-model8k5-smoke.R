context("model8k5 smoke")

if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

test_that("model8k5 poll_of_polls runs on a mixed latent grid", {
  skip_if_no_stan_tests()

  case <- make_model8_mixed_smoke_case()
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
  expect_identical(pop$time_scale_overrides, case$time_scale_overrides)
  expect_identical(pop$stan_data$stan_data$T, pop$stan_data$stan_data_with_overrides$T)
  expect_true(any(abs(pop$stan_data$stan_data$step_scale_t[-1] - 1) > 1e-12))
  expect_true(any(pop$time_line$time_line$date %in% seq(as.Date("2010-05-05"), as.Date("2010-05-10"), by = 1)))
  expect_equal(get_ndraws(pop), 10)

  expect_silent(plt <- suppressWarnings(plot_poll_of_polls(pop, y = "x3", collection_period = TRUE)))
  expect_s3_class(plt, "ggplot")
  expect_silent(suppressWarnings(ggplot2::ggplot_build(plt)))

  ls <- latent_state(pop)
  ls_day <- get_latent_state_for_dates(pop, as.Date("2010-05-08"))
  t_mixed_day <- unique(pop$time_line$daily$time_line_t[pop$time_line$daily$date == as.Date("2010-05-08")])

  expect_identical(dim(ls$latent_state)[2], nrow(pop$time_line$time_line))
  expect_identical(ls$time_line$time_line$date, pop$time_line$time_line$date)
  expect_identical(dim(ls_day$latent_state)[2], 1L)
  expect_identical(as.integer(dimnames(ls_day$latent_state)[[2]]), t_mixed_day)
  expect_true(all(is.finite(ls$latent_state)))
  expect_true(all(is.finite(ls_day$latent_state)))

  sigma_x_draws <- rstan::extract(pop$stan_fit, pars = "sigma_x")$sigma_x
  lp_draws <- rstan::extract(pop$stan_fit, pars = "lp__")$lp__

  expect_true(all(is.finite(sigma_x_draws)))
  expect_true(all(is.finite(lp_draws)))
})
