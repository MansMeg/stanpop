context("model8k5 cmdstanr smoke")

if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

normalized_print_lines <- function(x){
  lines <- capture.output(print(x))
  keep_prefixes <- c(
    "Model is fit during the period ",
    "Stan model: ",
    "Number of parameters:",
    "Number of unconstrained parameters:",
    "Parties:",
    "Time scale:"
  )
  keep <- vapply(
    lines,
    FUN.VALUE = logical(1),
    FUN = function(line) any(startsWith(line, keep_prefixes))
  )
  lines[keep]
}

print_diagnostics_lines <- function(x){
  lines <- capture.output(print(x))
  keep_patterns <- c(
    "^== Model diagnostics == ?$",
    "^no_divergent_transistions:",
    "^no_max_treedepth:",
    "^no_low_bfmi_chains:",
    "^mean_chain_step_size:",
    "^mean_chain_inv_mass_matrix_min:",
    "^mean_chain_inv_mass_matrix_max:"
  )
  keep <- vapply(
    lines,
    FUN.VALUE = logical(1),
    FUN = function(line) any(vapply(keep_patterns, grepl, logical(1), x = line))
  )
  lines[keep]
}

test_that("model8k5 poll_of_polls runs with cmdstanr backend on a mixed latent grid", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
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

test_that("model8k5 print output matches across backends after normalization", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()
  assert_rstan_available()

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

  expect_identical(normalized_print_lines(pop_rstan), normalized_print_lines(pop_cmdstanr))
})

test_that("model8k5 pop objects can be saved with save_pop, reloaded, and still extract samples on both backends", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()
  assert_rstan_available()

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

  tmp_rstan <- tempfile(fileext = ".rds")
  tmp_cmdstanr <- tempfile(fileext = ".rds")
  on.exit(unlink(c(tmp_rstan, tmp_cmdstanr)), add = TRUE)

  save_pop(pop_rstan, tmp_rstan)
  save_pop(pop_cmdstanr, tmp_cmdstanr)

  pop_rstan_reload <- load_pop(tmp_rstan)
  pop_cmdstanr_reload <- load_pop(tmp_cmdstanr)

  x_pred_rstan <- extract(pop_rstan_reload, pars = "x_pred")$x_pred
  x_pred_cmdstanr <- extract(pop_cmdstanr_reload, pars = "x_pred")$x_pred
  ls_rstan <- latent_state(pop_rstan_reload)
  ls_cmdstanr <- latent_state(pop_cmdstanr_reload)
  md_rstan <- get_model_diagnostics(pop_rstan_reload)
  md_cmdstanr <- get_model_diagnostics(pop_cmdstanr_reload)

  expect_identical(pop_rstan_reload$backend, "rstan")
  expect_identical(pop_cmdstanr_reload$backend, "cmdstanr")
  expect_true(all(is.finite(x_pred_rstan)))
  expect_true(all(is.finite(x_pred_cmdstanr)))
  expect_identical(dim(x_pred_rstan), dim(x_pred_cmdstanr))
  expect_identical(dim(ls_rstan$latent_state), dim(ls_cmdstanr$latent_state))
  expect_identical(names(md_rstan), names(md_cmdstanr))
  expect_true(all(is.finite(unlist(md_rstan))))
  expect_true(all(is.finite(unlist(md_cmdstanr))))
})

test_that("model8k5 print output includes diagnostics information on both backends", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()
  assert_rstan_available()

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

  diag_lines_rstan <- print_diagnostics_lines(pop_rstan)
  diag_lines_cmdstanr <- print_diagnostics_lines(pop_cmdstanr)

  expect_true(length(diag_lines_rstan) >= 5)
  expect_true(length(diag_lines_cmdstanr) >= 5)
  expect_true(any(grepl("^== Model diagnostics == ?$", diag_lines_rstan)))
  expect_true(any(grepl("^== Model diagnostics == ?$", diag_lines_cmdstanr)))
  expect_true(any(grepl("^mean_chain_step_size:", diag_lines_rstan)))
  expect_true(any(grepl("^mean_chain_step_size:", diag_lines_cmdstanr)))
  expect_true(any(grepl("^no_divergent_transistions:", diag_lines_rstan)))
  expect_true(any(grepl("^no_divergent_transistions:", diag_lines_cmdstanr)))
})
