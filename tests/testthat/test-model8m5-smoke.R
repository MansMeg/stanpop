context("model8m5 smoke")

if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

test_that("model8m5 poll_of_polls runs on a mixed latent grid", {
  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()

  case <- make_model8_mixed_smoke_case()
  known_date <- case$known_state$date[1]
  case$time_scale_overrides <- tibble::tibble(
    from = known_date - 1L,
    to = known_date + 1L,
    time_scale = "day"
  )
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
            iter = 40,
            warmup = 20,
            chains = 1,
            init = "random",
            refresh = 0,
            seed = 4711,
            cache_dir = NULL
          )
        )
      )
    )
  )

  expect_identical(pop$model, "model8m5")
  expect_true("time_scale_overrides" %in% names(pop))
  expect_identical(pop$time_scale_overrides, case$time_scale_overrides)
  expect_identical(pop$input_args$time_scale_overrides, case$time_scale_overrides)
  expect_identical(pop$stan_data$stan_data$T, pop$stan_data$stan_data_with_overrides$T)
  expect_true(any(abs(pop$stan_data$stan_data$step_scale_t[-1] - 1) > 1e-12))
  expect_true(any(pop$time_line$time_line$date %in% seq(known_date - 1L, known_date + 1L, by = 1)))
  expect_equal(get_ndraws(pop), 20)

  print_lines <- capture.output(print(pop))
  expect_true(any(grepl("^== Time scale overrides == ?$", print_lines)))
  expect_true(any(grepl(paste0("from: '", known_date - 1L, "'"), print_lines, fixed = TRUE)))
  expect_true(any(grepl(paste0("to: '", known_date + 1L, "'"), print_lines, fixed = TRUE)))
  expect_true(any(grepl("time_scale: day", print_lines, fixed = TRUE)))

  expect_silent(plt <- suppressWarnings(plot_poll_of_polls(pop, y = "x3", collection_period = TRUE)))
  expect_s3_class(plt, "ggplot")
  expect_silent(suppressWarnings(ggplot2::ggplot_build(plt)))

  ls <- latent_state(pop)
  ls_day <- get_latent_state_for_dates(pop, known_date)
  t_mixed_day <- unique(pop$time_line$daily$time_line_t[pop$time_line$daily$date == known_date])

  expect_identical(dim(ls$latent_state)[2], nrow(pop$time_line$time_line))
  expect_identical(ls$time_line$time_line$date, pop$time_line$time_line$date)
  expect_identical(dim(ls_day$latent_state)[2], 1L)
  expect_identical(as.integer(dimnames(ls_day$latent_state)[[2]]), t_mixed_day)
  expect_true(all(is.finite(ls$latent_state)))
  expect_true(all(is.finite(ls_day$latent_state)))

  expect_known_t_fixed_and_neighbors_vary(
    pop = pop,
    known_t = pop$stan_data$stan_data$x_known_t[1]
  )

  sigma_x_draws <- rstan::extract(pop$stan_fit, pars = "sigma_x")$sigma_x
  sigma_ep_draws <- rstan::extract(pop$stan_fit, pars = "sigma_ep")$sigma_ep
  lp_draws <- rstan::extract(pop$stan_fit, pars = "lp__")$lp__

  expect_true(all(is.finite(sigma_x_draws)))
  expect_true(all(is.finite(sigma_ep_draws)))
  expect_true(all(is.finite(lp_draws)))
})
