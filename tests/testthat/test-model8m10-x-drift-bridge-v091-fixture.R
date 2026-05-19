context("model8m10 x-drift bridge known-good 2026 v0.9.1 fixture")

translate_v091_x_drift_hyper_parameters <- function(hyper_parameters) {
  if("structural_bridge_x_target_t" %in% model_arguments("model8m10") &&
     !is.null(hyper_parameters$structural_bridge_x_drift)) {
    hyper_parameters$structural_bridge_type <- "x_drift"
    hyper_parameters$structural_bridge_x_target_path <- hyper_parameters$structural_bridge_x_drift
    hyper_parameters$structural_bridge_x_drift <- NULL
  }

  hyper_parameters
}

expect_bridge_fields_identical <- function(actual_stan_data,
                                           expected_stan_data,
                                           bridge_field_names) {
  expect_identical(names(actual_stan_data), names(expected_stan_data))
  expect_identical(names(expected_stan_data), bridge_field_names)

  for(field in bridge_field_names) {
    expect_identical(
      actual_stan_data[[field]],
      expected_stan_data[[field]],
      info = paste("Bridge Stan-data field differs:", field)
    )
  }
}

expect_x_drift_constant_gain_fields_inert <- function(stan_data) {
  if("structural_bridge_party_active_p" %in% names(stan_data)) {
    expect_identical(
      as.integer(stan_data$structural_bridge_party_active_p),
      rep(0L, stan_data$P)
    )
  }
  if("structural_bridge_x_target_t" %in% names(stan_data)) {
    expect_identical(dim(stan_data$structural_bridge_x_target_t), c(stan_data$T, stan_data$P))
    expect_true(all(stan_data$structural_bridge_x_target_t == 0))
  }
  if("structural_bridge_alpha_week" %in% names(stan_data)) {
    expect_identical(stan_data$structural_bridge_alpha_week, 1)
  }
}

test_that("current model8m10 reproduces known-good 2026 v0.9.1 x-drift bridge Stan data", {
  inputs <- readRDS(test_path("files", "model8m10_x_drift_bridge_2026_inputs_v0_9_1.rds"))
  expected <- readRDS(test_path("files", "model8m10_x_drift_bridge_2026_stan_data_v0_9_1.rds"))
  hyper_parameters <- translate_v091_x_drift_hyper_parameters(inputs$hyper_parameters)

  spd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = inputs$polls_data,
        y_name = inputs$y,
        model = inputs$model,
        time_scale = inputs$time_scale,
        time_scale_overrides = inputs$time_scale_overrides,
        known_state = inputs$known_state,
        model_time_range = inputs$model_time_range,
        latent_time_ranges = inputs$latent_time_ranges,
        hyper_parameters = hyper_parameters,
        slow_scales = inputs$slow_scales
      )
    )
  )

  expect_bridge_fields_identical(
    actual_stan_data = spd$stan_data[expected$bridge_field_names],
    expected_stan_data = expected$bridge_stan_data,
    bridge_field_names = expected$bridge_field_names
  )
  expect_identical(spd$stan_data[expected$bridge_field_names], expected$bridge_stan_data)
  expect_x_drift_constant_gain_fields_inert(spd$stan_data)
})


test_that("current model8m10 reproduces simulated v0.9.1 x-drift bridge Stan data", {
  fixture <- readRDS(test_path("files", "model8m10_x_drift_bridge_stan_data_v0_9_1.rds"))
  hyper_parameters <- translate_v091_x_drift_hyper_parameters(fixture$hyper_parameters)

  spd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = fixture$polls_data,
        y_name = fixture$parties,
        model = fixture$model,
        time_scale = fixture$time_scale,
        time_scale_overrides = fixture$time_scale_overrides,
        known_state = fixture$known_state,
        model_time_range = fixture$model_time_range,
        hyper_parameters = hyper_parameters
      )
    )
  )

  expect_bridge_fields_identical(
    actual_stan_data = spd$stan_data[fixture$bridge_field_names],
    expected_stan_data = fixture$bridge_stan_data,
    bridge_field_names = fixture$bridge_field_names
  )
  expect_x_drift_constant_gain_fields_inert(spd$stan_data)
})
