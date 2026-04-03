context("model8 stan_data structure regression")

make_simple_model8_structure_case <- function() {
  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(
    date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1)
  )
  known_state <- cbind(known_state, txdf[true_idx, ])

  spd <- simulate_polls(
    x = txdf,
    pd = pd_test,
    npolls = 40,
    time_scale = time_scale,
    start_date = "2010-01-01"
  )

  list(
    polls_data = spd,
    known_state = known_state,
    parties = parties,
    time_scale = time_scale
  )
}

test_that("model8k2 legacy stan_data keeps its structural shape", {
  case <- make_simple_model8_structure_case()
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  sd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = case$polls_data,
        time_scale = case$time_scale,
        y_name = case$parties,
        model = "model8k2",
        known_state = case$known_state,
        hyper_parameters = cfg
      )
    )
  )

  expect_length(names(sd$stan_data), 76)
  expect_true(all(c(
    "tw_t",
    "x_known_t",
    "x_unknown_t",
    "next_known_state_poll_index",
    "next_known_state_t_index",
    "g_t",
    "g_i",
    "Pp",
    "H",
    "h_i",
    "S",
    "s_i",
    "s_t",
    "use_obs_of_x",
    "R",
    "obs_of_x_t"
  ) %in% names(sd$stan_data)))

  expect_identical(
    unname(unlist(sd$stan_data[c("T", "N", "L", "P", "T_known", "Pp", "S", "H")])),
    c(100L, 40L, 119L, 2L, 2L, 1L, 1L, 1L)
  )
  expect_identical(
    c(
      length(sd$stan_data$g_t),
      length(sd$stan_data$s_t),
      length(sd$stan_data$s_i),
      length(sd$stan_data$next_known_state_t_index),
      length(sd$stan_data$obs_of_x_t)
    ),
    c(100L, 100L, 40L, 100L, 0L)
  )
})

test_that("model8m2 legacy stan_data keeps its structural shape", {
  case <- make_simple_model8_structure_case()
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

  sd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = case$polls_data,
        time_scale = case$time_scale,
        y_name = case$parties,
        model = "model8m2",
        known_state = case$known_state,
        hyper_parameters = cfg
      )
    )
  )

  expect_length(names(sd$stan_data), 83)
  expect_true(all(c(
    "tw_t",
    "x_known_t",
    "x_unknown_t",
    "next_known_state_poll_index",
    "next_known_state_t_index",
    "g_t",
    "g_i",
    "Pp",
    "H",
    "h_i",
    "S",
    "s_i",
    "s_t",
    "use_obs_of_x",
    "R",
    "use_sigma_ep",
    "election_period",
    "EP",
    "ep_inv_x"
  ) %in% names(sd$stan_data)))

  expect_identical(
    unname(unlist(sd$stan_data[c("T", "N", "L", "P", "T_known", "Pp", "S", "H", "EP", "R")])),
    c(100L, 40L, 119L, 2L, 2L, 1L, 1L, 1L, 1L, 0L)
  )
  expect_identical(
    c(
      length(sd$stan_data$g_t),
      length(sd$stan_data$s_t),
      length(sd$stan_data$s_i),
      length(sd$stan_data$next_known_state_t_index),
      length(sd$stan_data$election_period)
    ),
    c(100L, 100L, 40L, 100L, 100L)
  )
})
