context("model8i")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)
# adapop:::set_on_github_actions(TRUE)
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(adapop)
}


test_that("Test model 8i1 and 8h3 are identical", {
  # First we test that we get a similar result with 8g and 8g1
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
                            model = "model8i1",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                    use_multivariate_version = 0))
    )
  )

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 2L,
               use_softmax = 1L)

  expect_silent(pop8h3_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8h3 <- poll_of_polls(y = parties,
                                                model = "model8h3",
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

  expect_silent(pop8i1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8i1 <- poll_of_polls(y = parties,
                                                model = "model8i1",
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

  pn8h3 <- parameter_names(pop8h3)
  pn8i1 <- parameter_names(pop8i1)
  checkmate::expect_subset(pn8i1, pn8h3)

  no_up3 <- get_num_upars(pop8h3)
  no_up2 <- get_num_upars(pop8i1)
  expect_equal(no_up3, no_up2)

  lp1a <- log_prob(pop8h3, rep(0, get_num_upars(pop8h3)))
  lp1b <- log_prob(pop8i1, rep(0, get_num_upars(pop8i1)))
  expect_equal(lp1a, lp1b)

  pars <- rnorm(no_up2)
  lp1a <- log_prob(pop8h3, pars)
  lp1b <- log_prob(pop8i1, pars)
  expect_equal(lp1a, lp1b)

  lsh3 <- latent_state(pop8h3)
  lsh2 <- latent_state(pop8i1)

})



test_that("Test model 8i2", {
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
                            model = "model8i2",
                            known_state = known_state,
                            hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                    use_multivariate_version = 0))
    )
  )

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 3L,
               use_softmax = 1L)

  expect_silent(pop8i1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8i1 <- poll_of_polls(y = parties,
                                                model = "model8i1",
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

  pn8i2 <- parameter_names(pop8i2)
  pn8i1 <- parameter_names(pop8i1)
  expect_equal(pn8i1, pn8i2)

  no_up3 <- get_num_upars(pop8i2)
  no_up2 <- get_num_upars(pop8i1)
  expect_equal(no_up3, no_up2)

  lp1a <- log_prob(pop8i2, rep(0, get_num_upars(pop8i2)))
  lp1b <- log_prob(pop8i1, rep(0, get_num_upars(pop8i1)))
  expect_equal(lp1a, lp1b)

  pars <- rnorm(no_up2)
  lp1a <- log_prob(pop8i2, pars)
  lp1b <- log_prob(pop8i1, pars)
  expect_equal(lp1a, lp1b)

  lsh3 <- latent_state(pop8i2)
  lsh2 <- latent_state(pop8i1)

  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 4L,
               use_softmax = 1L)

  expect_silent(pop8i2b_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8i2b <- poll_of_polls(y = parties,
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

  lp1a <- log_prob(pop8i2, rep(0, get_num_upars(pop8i2)))
  lp1b <- log_prob(pop8i2b, rep(0, get_num_upars(pop8i2b)))
  expect_failure(expect_equal(lp1a, lp1b))

})
