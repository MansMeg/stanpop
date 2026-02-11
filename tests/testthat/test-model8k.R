context("model8k")

# Run the line below to run different test suites locally
# See documentation for details.
# stanpop:::set_test_stan_basic_on_local(TRUE)
# stanpop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

test_that("Test model 8k1 data parsing", {


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
                             model = "model8k1",
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
                             model = "model8k1",
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




test_that("Test sum to zero constraint for kappa", {

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

  cfg <-  list(sigma_kappa_hyper = 0.03,
               kappa_1_sigma_hyper = 0.01,
               use_industry_bias = 1L,
               use_ar_kappa = 1L,
               estimate_alpha_kappa = 1L,
               estimate_kappa_next = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 0L,
               use_softmax = 1L)

  suppressWarnings(
    suppressMessages(
      sd  <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8k1",
                             known_state = known_state,
                             hyper_parameters = cfg)
    )
  )

  # Simulate new with kappa = 0.05 per year
  kappa_x3_s1 <- 0.01
  kappa_x3_s2 <- 0.025
  kappa_x3_s3 <- 0.025
  kappa_x4_s1 <- -0.01
  kappa_x4_s2 <- -0.025
  kappa_x4_s3 <- -0.025
  is_s1 <- collection_midpoint_dates(spd) <= known_state$date[1]
  is_s2 <- collection_midpoint_dates(spd) <= known_state$date[2] & collection_midpoint_dates(spd) > known_state$date[1]
  is_s3 <- collection_midpoint_dates(spd) > known_state$date[2]

  spd$y[is_s1,"x3"] <-  spd$y[is_s1,"x3"] + sd$stan_data$g_i[is_s1] * kappa_x3_s1
  spd$y[is_s2,"x3"] <-  spd$y[is_s2,"x3"] + sd$stan_data$g_i[is_s2] * kappa_x3_s2
  spd$y[is_s3,"x3"] <-  spd$y[is_s3,"x3"] + sd$stan_data$g_i[is_s3] * kappa_x3_s3
  spd$y[is_s1,"x4"] <-  spd$y[is_s1,"x4"] + sd$stan_data$g_i[is_s1] * kappa_x4_s1
  spd$y[is_s2,"x4"] <-  spd$y[is_s2,"x4"] + sd$stan_data$g_i[is_s2] * kappa_x4_s2
  spd$y[is_s3,"x4"] <-  spd$y[is_s3,"x4"] + sd$stan_data$g_i[is_s3] * kappa_x4_s3

  # Takes 80 seconds
  skip_if_no_stan_tests()
  expect_silent(pop8k1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8k1a <- poll_of_polls(y = parties,
                                                 model = "model8k1",
                                                 polls_data = spd,
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

  expect_output(print(pop8k1a))

  kappa_pred_a <- rstan::extract(pop8k1a$stan_fit)$kappa_pred
  kappa_pred_next_sum_a <- kappa_pred_a[,3,1] + kappa_pred_a[,3,2]
  expect_failure(expect_equal(mean(kappa_pred_next_sum_a), 0))

  cfg$use_constrained_party_kappa_pred <- 1L

  # Takes 80 seconds
  expect_silent(pop8k1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8k1b <- poll_of_polls(y = parties,
                                                 model = "model8k1",
                                                 polls_data = spd,
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

  kappa_pred_b <- rstan::extract(pop8k1b$stan_fit)$kappa_pred
  kappa_pred_next_sum_b <- kappa_pred_b[,3,1] + kappa_pred_b[,3,2]
  expect_equal(mean(kappa_pred_next_sum_b), 0)

})


test_that("Test simple prediction with known obs_x", {

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
               use_softmax = 1L)

  obs_x <- data.frame(date = as.Date(c("2011-11-17", "2011-11-17")),
                      y = c("x3", "x4"),
                      mu = c(0.27, 0.18),
                      sigma = c(0.01, 0.01),
                      nu = c(5, 30))
  spd2 <- spd[spd$poll_info$.publish_date< as.Date("2011-07-01")]
  cfg$obs_x <- obs_x
  # plot(spd2, "x3")
  # plot(spd2, "x4")

  skip_if_no_stan_tests()
  expect_silent(pop8k1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8k1a <- poll_of_polls(y = parties,
                                                 model = "model8k1",
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
  expect_output(print(pop8k1a))

  expect_silent(plot(pop8k1a, "x3"))
  expect_silent(plot(pop8k1a, "x4"))

})


test_that("Test multiplicative industry bias for kappa", {

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

  cfg <-  list(sigma_kappa_hyper = 0.2,
               kappa_1_sigma_hyper = 0.3,
               use_industry_bias = 1L,
               use_ar_kappa = 1L,
               estimate_alpha_kappa = 1L,
               estimate_kappa_next = 0L,
               use_house_bias = 0L,
               use_design_effects = 0L,
               use_multivariate_version = 0L,
               use_multiplicative_industry_bias = 1L,
               use_softmax = 1L)

  suppressWarnings(
    suppressMessages(
      sd  <- stan_polls_data(x = spd,
                             time_scale = time_scale,
                             y_name = c("x3", "x4"),
                             model = "model8k1",
                             known_state = known_state,
                             hyper_parameters = cfg)
    )
  )

  # Simulate new with kappa = 0.05 per year
  kappa_x3_s1 <- log(0.8)
  kappa_x3_s2 <- log(0.9)
  kappa_x3_s3 <- log(0.9)
  kappa_x4_s1 <- log(1.1)
  kappa_x4_s2 <- log(1.2)
  kappa_x4_s3 <- log(1.2)
  is_s1 <- collection_midpoint_dates(spd) <= known_state$date[1]
  is_s2 <- collection_midpoint_dates(spd) <= known_state$date[2] & collection_midpoint_dates(spd) > known_state$date[1]
  is_s3 <- collection_midpoint_dates(spd) > known_state$date[2]

  spd$y[is_s1,"x3"] <-  spd$y[is_s1,"x3"] * exp(sd$stan_data$g_i[is_s1] * kappa_x3_s1)
  spd$y[is_s2,"x3"] <-  spd$y[is_s2,"x3"] * exp(sd$stan_data$g_i[is_s2] * kappa_x3_s2)
  spd$y[is_s3,"x3"] <-  spd$y[is_s3,"x3"] * exp(sd$stan_data$g_i[is_s3] * kappa_x3_s3)
  spd$y[is_s1,"x4"] <-  spd$y[is_s1,"x4"] * exp(sd$stan_data$g_i[is_s1] * kappa_x4_s1)
  spd$y[is_s2,"x4"] <-  spd$y[is_s2,"x4"] * exp(sd$stan_data$g_i[is_s2] * kappa_x4_s2)
  spd$y[is_s3,"x4"] <-  spd$y[is_s3,"x4"] * exp(sd$stan_data$g_i[is_s3] * kappa_x4_s3)

  skip_if_no_stan_tests()
  # Takes 120 seconds
  expect_silent(pop8k1_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8k1a <- poll_of_polls(y = parties,
                                                 model = "model8k1",
                                                 polls_data = spd,
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

  expect_output(print(pop8k1a))

  expect_silent(plot(pop8k1a, "x3"))
  expect_silent(plot(pop8k1a, "x4"))

  kappa_pred_a <- rstan::extract(pop8k1a$stan_fit)$kappa_pred
  kpa11 <- mean(kappa_pred_a[,1,1])
  kpa21 <- mean(kappa_pred_a[,2,1])
  expect_true(kpa11 < kpa21)
  expect_equal(kpa11, kappa_x3_s1, tol = 0.15)
  expect_true(kpa21 < 0)
  expect_equal(kpa21, kappa_x3_s2, tol = 0.15)

  kpa12 <- mean(kappa_pred_a[,1,2])
  kpa22 <- mean(kappa_pred_a[,2,2])
  expect_true(kpa12 < kpa22)
  expect_equal(kpa12, kappa_x4_s1, tol = 0.15)
  expect_true(kpa22 > 0)
  expect_equal(kpa22, kappa_x4_s2, tol = 0.15)

})


