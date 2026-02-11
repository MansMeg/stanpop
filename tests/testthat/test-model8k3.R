context("model8k3")

# Run the line below to run different test suites locally
# See documentation for details.
# stanpop:::set_test_stan_basic_on_local(TRUE)
# stanpop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

test_that("Test model 8k3 data parsing", {

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
  parse_obs_x <- get_internal("parse_obs_x")
  expect_silent(res1 <- parse_obs_x(hyper_parameters, tl, parties))
  expect_true(res1$use_obs_of_x == 0L)
  expect_true(res1$R == 0L)

  obs_x <- data.frame(date = as.Date(c("2010-05-03",  "2010-05-03", "2011-01-01")),
                      y = c("x4", "x3", "x3"),
                      mu = c(0.25, 0.27, 0.22),
                      sigma = c(0.02, 0.03, 0.03),
                      nu = c(10, 30, 10))

  hyper_parameters$obs_x <- obs_x
  expect_silent(res2 <- parse_obs_x(hyper_parameters, tl, parties))
  expect_true(is.null(res2$obs_x))
  expect_true(res2$use_obs_of_x == 1L)
  expect_true(res2$R == 3L)
  expect_identical(res2$obs_of_x_t, c(19L, 19L, 53L))
  expect_identical(res2$obs_of_x_p, c(2L, 1L, 1L))
  expect_identical(res2$obs_of_x_mu, obs_x$mu)
  expect_identical(res2$obs_of_x_sigma, obs_x$sigma)
  expect_identical(res2$obs_of_x_nu, obs_x$nu)

  expect_message(
    suppressWarnings(
      sd1 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8k3",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                     use_multivariate_version = 0))
    )
  )
  expect_true(sd1$stan_data$use_obs_of_x == 0L)
  expect_true(sd1$stan_data$R == 0L)
  expect_length(sd1$stan_data$obs_of_x_t, 0L)
  expect_type(sd1$stan_data$obs_of_x_t, "integer")
  expect_length(sd1$stan_data$obs_of_x_p, 0L)
  expect_type(sd1$stan_data$obs_of_x_p, "integer")
  expect_length(sd1$stan_data$obs_of_x_mu, 0L)
  expect_type(sd1$stan_data$obs_of_x_mu, "double")
  expect_length(sd1$stan_data$obs_of_x_sigma, 0L)
  expect_type(sd1$stan_data$obs_of_x_sigma, "double")
  expect_length(sd1$stan_data$obs_of_x_nu, 0L)
  expect_type(sd1$stan_data$obs_of_x_nu, "double")


  expect_message(
    suppressWarnings(
      sd2 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8k3",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                     use_multivariate_version = 0,
                                                     obs_x = obs_x))
    )
  )

  expect_true(sd2$stan_data$use_obs_of_x == 1L)
  expect_true(sd2$stan_data$R == 3L)
  expect_length(sd2$stan_data$obs_of_x_t, 3L)
  expect_type(sd2$stan_data$obs_of_x_t, "integer")
  expect_identical(sd2$stan_data$obs_of_x_t, c(19L, 19L, 53L))
  expect_length(sd2$stan_data$obs_of_x_p, 3L)
  expect_type(sd2$stan_data$obs_of_x_p, "integer")
  expect_identical(sd2$stan_data$obs_of_x_p, c(2L, 1L, 1L))
  expect_length(sd2$stan_data$obs_of_x_mu, 3L)
  expect_type(sd2$stan_data$obs_of_x_mu, "double")
  expect_identical(sd2$stan_data$obs_of_x_mu, obs_x$mu)
  expect_length(sd2$stan_data$obs_of_x_sigma, 3L)
  expect_type(sd2$stan_data$obs_of_x_sigma, "double")
  expect_identical(sd2$stan_data$obs_of_x_sigma, obs_x$sigma)
  expect_length(sd2$stan_data$obs_of_x_nu, 3L)
  expect_type(sd2$stan_data$obs_of_x_nu, "double")
  expect_identical(sd2$stan_data$obs_of_x_nu, obs_x$nu)
})


test_that("Test model 8k2 and 8k3 are identical", {

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

  obs_x <- data.frame(date = as.Date(c("2010-05-03",  "2010-05-03", "2011-01-01")),
                      y = c("x4", "x3", "x3"),
                      mu = c(0.25, 0.27, 0.22),
                      sigma = c(0.02, 0.03, 0.03),
                      nu = c(10, 30, 10))

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 1L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 2L,
               use_softmax = 1L)

  skip_if_no_stan_tests()
  expect_silent(pop8k2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8k2 <- poll_of_polls(y = parties,
                                                model = "model8k2",
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

  expect_silent(pop8k3_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8k3 <- poll_of_polls(y = parties,
                                                model = "model8k3",
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

  pn8k2 <- parameter_names(pop8k2)
  pn8k3 <- parameter_names(pop8k3)
  checkmate::expect_subset(pn8k2, pn8k3)

  no_up2 <- get_num_upars(pop8k2)
  no_up3 <- get_num_upars(pop8k3)
  expect_equal(no_up3, no_up2)

  lp1a <- log_prob(pop8k2, rep(0, get_num_upars(pop8k2)))
  lp1b <- log_prob(pop8k3, rep(0, get_num_upars(pop8k2)))
  expect_equal(lp1a, lp1b)

  pars <- rnorm(no_up2)
  lp1a <- log_prob(pop8k2, pars)
  lp1b <- log_prob(pop8k3, pars)
  expect_equal(lp1a, lp1b)

})

