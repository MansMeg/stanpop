context("model8m")

# Run the line below to run different test suites locally
# See documentation for details.
# stanpop:::set_test_stan_basic_on_local(TRUE)
# stanpop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

test_that("Test model 8m1 data parsing", {

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

  tl <- time_line(spd, time_scale = time_scale)
  hyper_parameters <- list(sigma_kappa_hyper = 0.01,
                          use_multivariate_version = 0)

  hyper_parameters$election_period <- list(c("2010-05-03", "2010-05-22"),
                                           c("2010-09-01", "2010-09-01"))
  hyper_parameters$use_sigma_ep <- 1L
  hyper_parameters$ep_inv_x <- list(c(3.984064, 3.937008),
                                    c(4.347826, 4.694836))
  parse_election_period <- get_internal("parse_election_period")
  expect_silent(res4 <- parse_election_period(hyper_parameters, tl))
  expect_silent(res5 <- hyper_parameters$ep_inv_x[res4$election_period[res4$election_period>0]])

  expect_message(
    suppressWarnings(
      sd1 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8m1",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                     use_multivariate_version = 0))
    )
  )
  expect_true(sd1$stan_data$EP == 0L)
  expect_true(is.list(sd1$stan_data$ep_inv_x))
  expect_length(sd1$stan_data$ep_inv_x, 0L)


  expect_error(
    suppressWarnings(
      sd2 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8m1",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                     use_multivariate_version = 0,
                                                     use_sigma_ep = 1))
    )
  )

  expect_error(
    suppressWarnings(
      sd2 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8m1",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                     use_multivariate_version = 0,
                                                     use_sigma_ep = 0L,
                                                     election_period = list(c("2010-05-03", "2010-05-22"),c("2010-09-01", "2010-09-01"))))
    )
  )


  expect_error(
    suppressWarnings(suppressMessages(
      sd2 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8m1",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                     use_multivariate_version = 0,
                                                     use_sigma_ep = 1,
                                                     election_period = list(c("2010-05-03", "2010-05-22"),c("2010-09-01", "2010-09-01"))))
    ))
  )

  expect_message(
    suppressWarnings(
      sd2 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8m1",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                     use_multivariate_version = 0,
                                                     use_sigma_ep = 1,
                                                     election_period = list(c("2010-05-03", "2010-05-22"),c("2010-09-01", "2010-09-01")),
                                                     ep_inv_x = list(c(3.984064, 3.937008), c(4.347826, 4.694836))))
    )
  )

})


test_that("Test model 8m1 data parsing", {

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

  tl <- time_line(spd, time_scale = time_scale)
  hyper_parameters <- list(sigma_kappa_hyper = 0.01,
                           use_multivariate_version = 0)

  hyper_parameters$election_period <- list(c("2010-05-03", "2010-05-22"),
                                           c("2010-09-01", "2010-09-01"))
  hyper_parameters$use_sigma_ep <- 1L
  hyper_parameters$ep_inv_x <- list(c(3.984064, 3.937008),
                                    c(4.347826, 4.694836))
  parse_election_period <- get_internal("parse_election_period")
  expect_silent(res4 <- parse_election_period(hyper_parameters, tl))
  expect_silent(res5 <- hyper_parameters$ep_inv_x[res4$election_period[res4$election_period>0]])

  expect_message(
    suppressWarnings(
      sd1 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8m2",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper_sd = 0.01,
                                                     use_multivariate_version = 0))
    )
  )
  expect_equal(sd1$stan_data$sigma_kappa_hyper_mean, 0.0)
  expect_equal(sd1$stan_data$sigma_kappa_hyper_sd, 0.01)
})


test_that("Test that adding election_period give different log_prob", {

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

  election_periods <- list(c("2010-05-03",  "2010-05-20"))
  ep_inv_x <- list(c(3.984064, 3.937008))

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 1L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 2L,
               use_softmax = 1L,
               election_period = election_periods,
               use_sigma_ep = 1L,
               ep_inv_x = ep_inv_x)

  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()
  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8m1 <- poll_of_polls(y = parties,
                                                model = "model8m1",
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

  cfg$use_sigma_ep <- 2L

  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8m2 <- poll_of_polls(y = parties,
                                                model = "model8m1",
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

  cfg$sigma_kappa_hyper_sd <- cfg$sigma_kappa_hyper
  cfg$sigma_kappa_hyper <- NULL

  expect_silent(pop8m1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8m22 <- poll_of_polls(y = parties,
                                                model = "model8m2",
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

  pn8m1 <- parameter_names(pop8m1)
  pn8m2 <- parameter_names(pop8m2)
  checkmate::expect_subset(pn8m1, pn8m2)

  no_up3 <- get_num_upars(pop8m1)
  no_up4 <- get_num_upars(pop8m2)
  expect_lt(no_up3, no_up4)

  lp1a <- log_prob(pop8m1, rep(0, no_up3))
  lp1b <- log_prob(pop8m2, rep(0, no_up4))
  expect_failure(expect_equal(lp1a, lp1b))

  pars <- rnorm(no_up4)
  lp1a <- log_prob(pop8m1, pars[-1])
  lp1b <- log_prob(pop8m2, pars)
  expect_failure(expect_equal(lp1a, lp1b))

  lsh3 <- expect_silent(latent_state(pop8m1))

})


test_that("Test model m3 and m4", {

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

  election_periods <- list(c("2010-05-03",  "2010-05-20"))
  ep_inv_x <- list(c(3.984064, 3.937008))

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper_sd = 0.03,
               use_industry_bias = 1L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 2L,
               use_softmax = 1L,
               election_period = election_periods,
               use_sigma_ep = 2L,
               ep_inv_x = ep_inv_x)

  skip_if_no_stan_tests()
  skip_if_no_rstan_tests()
  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8m3 <- poll_of_polls(y = parties,
                                                model = "model8m3",
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

  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8m2 <- poll_of_polls(y = parties,
                                                model = "model8m2",
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

  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8m4 <- poll_of_polls(y = parties,
                                                model = "model8m4",
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


  pn8m3 <- parameter_names(pop8m3)
  pn8m2 <- parameter_names(pop8m2)
  checkmate::expect_subset(pn8m3, pn8m2)

  no_up3 <- get_num_upars(pop8m3)
  no_up2 <- get_num_upars(pop8m2)
  expect_equal(no_up3, no_up2)

  lp1a <- log_prob(pop8m3, rep(0, no_up2))
  lp1b <- log_prob(pop8m2, rep(0, no_up3))
  expect_equal(lp1a, lp1b)

  pars <- rnorm(no_up2)
  lp1a <- log_prob(pop8m3, pars)
  lp1b <- log_prob(pop8m2, pars)
  expect_equal(lp1a, lp1b)

  lsh3 <- expect_silent(latent_state(pop8m3))

})
