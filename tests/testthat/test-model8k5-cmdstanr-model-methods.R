context("model8k5 cmdstanr model methods")

if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

test_that("model8k5 gives the same parameter space and log_prob with rstan and cmdstanr", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
#  skip_if_no_cmdstanr()
  assert_rstan_available()
#  skip("Temporarily skipped due to intermittent cmdstanr model-method segfault in CI.")

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
              iter_warmup = 5,
              compile_args = list(compile_model_methods = TRUE)
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
  expect_identical(get_num_pars(pop_rstan), get_num_pars(pop_cmdstanr))

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
  ls_rstan <- latent_state(pop_rstan)
  ls_cmdstanr <- latent_state(pop_cmdstanr)
  md_rstan <- get_model_diagnostics(pop_rstan)
  md_cmdstanr <- get_model_diagnostics(pop_cmdstanr)

  expect_identical(dim(sigma_x_rstan), dim(sigma_x_cmdstanr))
  expect_identical(dim(x_pred_rstan), dim(x_pred_cmdstanr))
  expect_identical(dim(ls_rstan$latent_state), dim(ls_cmdstanr$latent_state))
  expect_identical(dimnames(ls_rstan$latent_state), dimnames(ls_cmdstanr$latent_state))
  expect_identical(names(md_rstan), names(md_cmdstanr))
  expect_true(all(is.finite(unlist(md_rstan))))
  expect_true(all(is.finite(unlist(md_cmdstanr))))
})
