context("model8i")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(adapop)
}


test_that("Test model 8j1 and 8i2 are identical", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")


  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1))
  known_state <- cbind(known_state, txdf[true_idx,])

  spd <- simulate_polls(x = txdf,
                        pd = pd_test,
                        npolls = 150,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  expect_message(
    suppressWarnings(
      sd <- stan_polls_data(x = spd,
                            time_scale = time_scale,
                            y_name = c("x3", "x4"),
                            model = "model8j1",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                    use_multivariate_version = 0))
    )
  )

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 1L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 2L,
               use_softmax = 1L)

  expect_silent(pop8j1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8j1 <- poll_of_polls(y = parties,
                                                model = "model8j1",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg,
                                                warmup = 0,
                                                iter = 3,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  expect_silent(pop8i2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8i2 <- poll_of_polls(y = parties,
                                                model = "model8i2",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg,
                                                warmup = 0,
                                                iter = 3,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  pn8j1 <- parameter_names(pop8j1)
  pn8i2 <- parameter_names(pop8i2)
  checkmate::expect_subset(pn8j1, pn8i2)

  no_up3 <- get_num_upars(pop8j1)
  no_up2 <- get_num_upars(pop8i2)
  expect_equal(no_up3, no_up2)

  lp1a <- log_prob(pop8j1, rep(0, get_num_upars(pop8j1)))
  lp1b <- log_prob(pop8i2, rep(0, get_num_upars(pop8j1)))
  expect_equal(lp1a, lp1b)

  pars <- rnorm(no_up2)
  lp1a <- log_prob(pop8j1, pars)
  lp1b <- log_prob(pop8i2, pars)
  expect_equal(lp1a, lp1b)

  lsh3 <- latent_state(pop8j1)
  lsh2 <- latent_state(pop8i2)

  cfg_mib <-  list(sigma_kappa_hyper = 0.03,
                   use_industry_bias = 1L,
                   use_multiplicative_industry_bias = 1L,
                   use_house_bias = 0L,
                   use_design_effects = 0L,
                   use_multivariate_version = 2L,
                   use_softmax = 1L)

  expect_silent(pop8j1mib_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8j1mib <- poll_of_polls(y = parties,
                                                model = "model8j1",
                                                polls_data = spd,
                                                time_scale = time_scale,
                                                known_state = known_state,
                                                hyper_parameters = cfg_mib,
                                                warmup = 0,
                                                iter = 3,
                                                chains = 1,
                                                cache_dir = NULL)
                      )
                    )
                  )
  )

  pn8j1 <- parameter_names(pop8j1)
  pn8j1mib <- parameter_names(pop8j1mib)
  checkmate::expect_subset(pn8j1, pn8j1mib)

  no_up3 <- get_num_upars(pop8j1)
  no_up2 <- get_num_upars(pop8j1mib)
  expect_equal(no_up3, no_up2)

  lp1a <- log_prob(pop8j1, rep(0, get_num_upars(pop8j1)))
  lp1b <- log_prob(pop8j1mib, rep(0, get_num_upars(pop8j1)))
  expect_equal(lp1a, lp1b)

  pars <- rnorm(no_up2)
  lp1a <- log_prob(pop8j1, pars)
  lp1b <- log_prob(pop8j1mib, pars)
  expect_failure(expect_equal(lp1a, lp1b))

})

