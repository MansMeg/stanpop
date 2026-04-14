context("stan_data_with_overrides noop regression")

make_simple_stan_data_with_overrides_noop_case <- function() {
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

assert_noop_override_stan_data_equivalence <- function(sd) {
  has_all_legacy_fields <- all(names(sd$stan_data) %in% names(sd$stan_data_with_overrides))
  expect_true(has_all_legacy_fields)

  if(has_all_legacy_fields){
    expect_setequal(
      setdiff(names(sd$stan_data_with_overrides), names(sd$stan_data)),
      c("delta_days_t", "step_scale_t")
    )
    expect_identical(sd$time_line_with_overrides$time_line$date, sd$time_line$time_line$date)

    for(nm in c(
      "tw_t",
      "x_known_t",
      "x_unknown_t",
      "next_known_state_poll_index",
      "next_known_state_t_index",
      "g_t",
      "g_i",
      "s_i",
      "s_t"
    )) {
      expect_identical(
        sd$stan_data_with_overrides[[nm]],
        sd$stan_data[[nm]],
        label = paste("field", nm)
      )
    }
  }
}

test_that("model8k2 override-aware stan_data matches legacy stan_data when overrides are NULL", {
  case <- make_simple_stan_data_with_overrides_noop_case()
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

  assert_noop_override_stan_data_equivalence(sd)
  if(all(names(sd$stan_data) %in% names(sd$stan_data_with_overrides))){
    expect_identical(sd$stan_data_with_overrides$obs_of_x_t, sd$stan_data$obs_of_x_t)
  }
})

test_that("model8m2 override-aware stan_data matches legacy stan_data when overrides are NULL", {
  case <- make_simple_stan_data_with_overrides_noop_case()
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

  assert_noop_override_stan_data_equivalence(sd)
  if(all(names(sd$stan_data) %in% names(sd$stan_data_with_overrides))){
    expect_identical(sd$stan_data_with_overrides$obs_of_x_t, sd$stan_data$obs_of_x_t)
    expect_identical(sd$stan_data_with_overrides$election_period, sd$stan_data$election_period)
    expect_identical(sd$stan_data_with_overrides$EP, sd$stan_data$EP)
  }
})
