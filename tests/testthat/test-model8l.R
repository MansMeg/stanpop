context("model8l")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(adapop)
}


test_that("Test model 8l1 data parsing", {
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

  tl <- time_line(spd, time_scale = time_scale)
  hyper_parameters <- list(sigma_kappa_hyper = 0.01,
                          use_multivariate_version = 0)

  parse_election_period <- adapop:::parse_election_period
  expect_silent(res1 <- parse_election_period(hyper_parameters, tl))
  expect_equal(res1$use_sigma_ep, 0L)
  expect_equal(res1$election_period, rep(0L, nrow(tl$time_line)))

  hyper_parameters$use_sigma_ep <- 0L
  expect_silent(res2 <- parse_election_period(hyper_parameters, tl))
  expect_equal(res2$use_sigma_ep, 0L)
  expect_equal(res2$election_period, rep(0L, nrow(tl$time_line)))

  hyper_parameters$use_sigma_ep <- 1L
  expect_error(res3 <- parse_election_period(hyper_parameters, tl))

  hyper_parameters$use_sigma_ep <- NULL
  hyper_parameters$election_period <- list(c("2010-05-03", "2010-05-22"),
                                           c("2010-09-01", "2010-09-01"))
  expect_error(res4 <- parse_election_period(hyper_parameters, tl))

  hyper_parameters$use_sigma_ep <- 0L
  expect_error(res5 <- parse_election_period(hyper_parameters, tl))

  hyper_parameters$use_sigma_ep <- 1L
  expect_silent(res5 <- parse_election_period(hyper_parameters, tl))
  expect_equal(res5$use_sigma_ep, 1L)
  expect_equal(max(res5$election_period), 2)
  eps <- rep(0L, nrow(tl$time_line))
  eps[19:21] <- 1L
  eps[36] <- 2L
  expect_equal(res5$election_period, eps)

  hyper_parameters <- list(sigma_kappa_hyper = 0.01,
                           use_multivariate_version = 0)
  expect_message(
    suppressWarnings(
      sd1 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8l1",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                     use_multivariate_version = 0))
    )
  )
  expect_true(sd1$stan_data$use_sigma_ep == 0L)
  expect_true(sum(sd1$stan_data$election_period) == 0L)


  expect_error(
    suppressWarnings(
      sd2 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8l1",
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
                             model = "model8l1",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                     use_multivariate_version = 0,
                                                     use_sigma_ep = 0L,
                                                     election_period = list(c("2010-05-03", "2010-05-22"),c("2010-09-01", "2010-09-01"))))
    )
  )


  expect_message(
    suppressWarnings(
      sd2 <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8l1",
                             known_state = known_state,
                             hyper_parameters = list(sigma_kappa_hyper = 0.01,
                                                     use_multivariate_version = 0,
                                                     use_sigma_ep = 1,
                                                     election_period = list(c("2010-05-03", "2010-05-22"),c("2010-09-01", "2010-09-01"))))
    )
  )

})


test_that("Test model 8l1 and 8k1 are identical", {
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

  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8l1 <- poll_of_polls(y = parties,
                                                model = "model8l1",
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

  expect_silent(pop8k1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8k1 <- poll_of_polls(y = parties,
                                                model = "model8k1",
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

  pn8l1 <- parameter_names(pop8l1)
  pn8k1 <- parameter_names(pop8k1)
  checkmate::expect_subset(pn8l1, pn8k1)

  no_up3 <- adapop::get_num_upars(pop8l1)
  no_up2 <- adapop::get_num_upars(pop8k1)
  expect_equal(no_up3, no_up2)

  lp1a <- adapop::log_prob(pop8l1, rep(0, adapop::get_num_upars(pop8l1)))
  lp1b <- adapop::log_prob(pop8k1, rep(0, adapop::get_num_upars(pop8l1)))
  expect_equal(lp1a, lp1b)

  pars <- rnorm(no_up2)
  lp1a <- adapop::log_prob(pop8l1, pars)
  lp1b <- adapop::log_prob(pop8k1, pars)
  expect_equal(lp1a, lp1b)

  lsh3 <- latent_state(pop8l1)
  lsh2 <- latent_state(pop8k1)

})


test_that("Test that adding election_period give different log_prob", {
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

  election_periods <- list(c("2010-05-03",  "2010-05-20"))

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 1L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 2L,
               use_softmax = 1L,
               election_period = election_periods,
               use_sigma_ep = 1L)

  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8l1 <- poll_of_polls(y = parties,
                                                model = "model8l1",
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
                        pop8l2 <- poll_of_polls(y = parties,
                                                model = "model8l1",
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

  cfg$election_period <- NULL
  cfg$use_sigma_ep <- NULL

  expect_silent(pop8k1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8k1 <- poll_of_polls(y = parties,
                                                model = "model8k1",
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

  pn8l1 <- parameter_names(pop8l1)
  pn8l2 <- parameter_names(pop8l2)
  pn8k1 <- parameter_names(pop8k1)
  checkmate::expect_subset(pn8k1, pn8l1)
  checkmate::expect_subset(pn8l1, pn8l2)

  no_up3 <- get_num_upars(pop8l1)
  no_up4 <- get_num_upars(pop8l2)
  no_up2 <- get_num_upars(pop8k1)
  expect_equal(no_up3 - 1L, no_up2)
  expect_equal(no_up3, no_up4 - 1L)

  lp1a <- log_prob(pop8l1, rep(0, get_num_upars(pop8l1)))
  lp1c <- log_prob(pop8l2, rep(0, get_num_upars(pop8l2)))
  lp1b <- log_prob(pop8k1, rep(0, get_num_upars(pop8l1) - 1L))
  expect_failure(expect_equal(lp1a, lp1b))
  expect_failure(expect_equal(lp1a, lp1c))

  pars <- rnorm(no_up3)
  lp1a <- log_prob(pop8l1, pars)
  lp1b <- log_prob(pop8k1, pars[-1])
  expect_failure(expect_equal(lp1a, lp1b))

})


test_that("Test simple prediction with known obs_x and two election period", {
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
                        npolls = 200,
                        time_scale = time_scale,
                        start_date = "2010-01-01")

  mtr <- time_range(spd)
  ltr <- setup_latent_time_ranges(x = NULL, y = c("x3", "x4"), mtr)

  cfg <-  list(use_industry_bias = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 0L,
               use_softmax = 1L,
               use_sigma_ep = 1L,
               election_period = list(c("2010-09-29", "2010-10-29"),
                                      c("2011-01-01", "2011-05-13"),
                                      c("2011-10-01", "2011-11-17")))

  obs_x <- data.frame(date = as.Date(c("2011-11-17", "2011-11-17")),
                      y = c("x3", "x4"),
                      mu = c(0.27, 0.18),
                      sigma = c(0.01, 0.01),
                      nu = c(5, 30))
  spd2 <- spd[spd$poll_info$.publish_date< as.Date("2011-07-01")]
  cfg$obs_x <- obs_x
  # plot(spd2, "x3")
  # plot(spd2, "x4")

  # takes roughly 85 seconds
  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8l1a <- poll_of_polls(y = parties,
                                                 model = "model8l1",
                                                 polls_data = spd2,
                                                 time_scale = time_scale,
                                                 known_state = known_state,
                                                 hyper_parameters = cfg,
                                                 warmup = 1000,
                                                 iter = 2000,
                                                 chains = 1,
                                                 cache_dir = NULL)
                      )
                    )
                  )
  )

  plot(pop8l1a, "x3")
  plot(pop8l1a, "x4")

  # Extract sigma_ep
  expect_silent(sigma_ep <- rstan::extract(pop8l1a$stan_fit, par = "sigma_ep")$sigma_ep)
  # hist(sigma_ep)

  cfg$use_sigma_ep <- 2L
  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8l1b <- poll_of_polls(y = parties,
                                                 model = "model8l1",
                                                 polls_data = spd2,
                                                 time_scale = time_scale,
                                                 known_state = known_state,
                                                 hyper_parameters = cfg,
                                                 warmup = 1000,
                                                 iter = 2000,
                                                 chains = 1,
                                                 cache_dir = NULL)
                      )
                    )
                  )
  )

  plot(pop8l1b, "x3")
  plot(pop8l1b, "x4")

})


test_that("Test exp_logsumexp_minus_eta", {
  skip_if_not(adapop:::test_stan_basic_on_local() | adapop:::test_stan_full_on_local() | adapop:::on_github_actions_test_branch())

  mfp <- adapop:::get_pop_stan_model_file_path("model8l2")
  rstan::expose_stan_functions(rstan::stanc(file = mfp))

  inv_softmax <- function(x){
    log(x) - log(x[length(x)])
  }
  softmax <- function(x){
    exp(x)/sum(exp(x))
  }

  x <- c(0.35, 0.11, 0.03,0.1,0.22,0.18, 0.01)
  inv_x <- 1/x
  eta <- inv_softmax(x)
  # softmax(eta)
  # exp(log(sum(exp(eta)))-eta)
  expect_equal(exp_logsumexp_minus_eta(eta[-length(eta)]), inv_x[-length(inv_x)])
})


test_that("Test model 8l1 and 8l2 are identical", {
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

  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8l1 <- poll_of_polls(y = parties,
                                                model = "model8l1",
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

  expect_silent(pop8l2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8l2 <- poll_of_polls(y = parties,
                                                model = "model8l2",
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

  pn8l1 <- parameter_names(pop8l1)
  pn8l2 <- parameter_names(pop8l2)
  checkmate::expect_subset(pn8l1, pn8l2)

  no_up3 <- adapop::get_num_upars(pop8l1)
  no_up2 <- adapop::get_num_upars(pop8l2)
  expect_equal(no_up3, no_up2)

  lp1a <- adapop::log_prob(pop8l1, rep(0, adapop::get_num_upars(pop8l1)))
  lp1b <- adapop::log_prob(pop8l2, rep(0, adapop::get_num_upars(pop8l1)))
  expect_equal(lp1a, lp1b)

  pars <- rnorm(no_up2)
  lp1a <- adapop::log_prob(pop8l1, pars)
  lp1b <- adapop::log_prob(pop8l2, pars)
  expect_equal(lp1a, lp1b)

  lsh3 <- latent_state(pop8l1)
  lsh2 <- latent_state(pop8l2)

})


test_that("Test that adding election_period give different log_prob", {
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

  election_periods <- list(c("2010-05-03",  "2010-05-20"))

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 1L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 2L,
               use_softmax = 1L,
               election_period = election_periods,
               use_sigma_ep = 1L)

  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8l1 <- poll_of_polls(y = parties,
                                                model = "model8l1",
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

  cfg$sigma_ep_mean <- 0
  cfg$sigma_ep_sd <- 0.02

  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8l2a <- poll_of_polls(y = parties,
                                                model = "model8l2",
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
                        pop8l2b <- poll_of_polls(y = parties,
                                                 model = "model8l2",
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

  pn8l1 <- parameter_names(pop8l1)
  pn8l2a <- parameter_names(pop8l2a)
  pn8l2b <- parameter_names(pop8l2b)
  expect_equal(pn8l1, pn8l2a)
  checkmate::expect_subset(pn8l2a, pn8l2b)

  no_up1 <- get_num_upars(pop8l1)
  no_up2a <- get_num_upars(pop8l2a)
  no_up2b <- get_num_upars(pop8l2b)
  expect_equal(no_up1, no_up2a)
  expect_equal(no_up2a, no_up2b - 1L)

  lp1a <- log_prob(pop8l1, rep(0, get_num_upars(pop8l1)))
  lp2a <- log_prob(pop8l2a, rep(0, get_num_upars(pop8l2a)))
  lp2b <- log_prob(pop8l2b, rep(0, get_num_upars(pop8l2b)))
  expect_failure(expect_equal(lp1a, lp2b))
  expect_failure(expect_equal(lp2a, lp2b))

  pars <- rnorm(no_up2b, sd = 0.01)
  lp1a <- log_prob(pop8l1, pars[-1])
  lp2a <- log_prob(pop8l2a, pars[-1])
  lp2b <- log_prob(pop8l2b, pars)

  expect_failure(expect_equal(lp1a, lp2a))
  expect_failure(expect_equal(lp2a, lp2b))

})


test_that("Test model 8l3", {
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

  election_periods <- list(c("2010-05-03",  "2010-05-20"))

  # Check that we get identical lpd
  cfg <-  list(sigma_kappa_hyper = 0.03,
               use_industry_bias = 1L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 2L,
               use_softmax = 1L,
               election_period = election_periods,
               use_sigma_ep = 1L)


  cfg$sigma_ep_mean <- 0
  cfg$sigma_ep_sd <- 0.02
  cfg$use_sigma_ep <- 2L

  expect_silent(pop8l1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8l2 <- poll_of_polls(y = parties,
                                                 model = "model8l2",
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
                        pop8l3 <- poll_of_polls(y = parties,
                                                 model = "model8l3",
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

  pn8l2 <- parameter_names(pop8l2)
  pn8l3 <- parameter_names(pop8l3)
  expect_equal(pn8l2, pn8l3)

  no_up2 <- get_num_upars(pop8l2)
  no_up3 <- get_num_upars(pop8l3)
  expect_equal(no_up2, no_up3)

  lp2 <- log_prob(pop8l2, rep(0, get_num_upars(pop8l2)))
  lp3 <- log_prob(pop8l3, rep(0, get_num_upars(pop8l3)))
  # expect_failure(expect_equal(lp2, lp3)) This happen to be equal here

  pars <- rnorm(no_up2, sd = 0.1)
  lp2 <- log_prob(pop8l2, pars)
  lp3 <- log_prob(pop8l3, pars)
  expect_failure(expect_equal(lp2, lp3))

  lsh3 <- latent_state(pop8l3)

})
