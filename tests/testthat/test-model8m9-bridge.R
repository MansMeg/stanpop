context("model8m9 structural bridge")

model8m9_bridge_fields_identical <- function(actual_stan_data,
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

model8m9_stan_polls_data_from_fixture <- function(fixture) {
  y_name <- if(!is.null(fixture$y)) fixture$y else fixture$parties

  suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = fixture$polls_data,
        y_name = y_name,
        model = "model8m9",
        time_scale = fixture$time_scale,
        time_scale_overrides = fixture$time_scale_overrides,
        known_state = fixture$known_state,
        model_time_range = fixture$model_time_range,
        latent_time_ranges = fixture$latent_time_ranges,
        hyper_parameters = fixture$hyper_parameters,
        slow_scales = fixture$slow_scales
      )
    )
  )
}

expect_model8m9_has_only_drift_bridge_fields <- function(stan_data) {
  expect_true(all(c(
    "structural_bridge_type",
    "structural_bridge_active_t",
    "structural_bridge_B",
    "structural_bridge_party",
    "structural_bridge_delta_x",
    "structural_bridge_epsilon",
    "structural_bridge_sigma_scale"
  ) %in% names(stan_data)))
  expect_false(any(c(
    "structural_bridge_party_active_p",
    "structural_bridge_x_target_t",
    "structural_bridge_alpha_week"
  ) %in% names(stan_data)))
}

test_that("model8m9 is registered as the drift-only 0.9.1 bridge model", {
  supported_pop_models <- get_internal("supported_pop_models")
  get_pop_stan_model_file_path <- get_internal("get_pop_stan_model_file_path")

  expect_true("model8m9" %in% supported_pop_models())
  expect_true(file.exists(get_pop_stan_model_file_path("model8m9")))
  expect_true(get_internal("model_supports_time_scale_overrides")("model8m9"))

  args <- model_arguments("model8m9")
  expect_true(all(c(
    "structural_bridge_type",
    "structural_bridge_active_t",
    "structural_bridge_B",
    "structural_bridge_party",
    "structural_bridge_delta_x",
    "structural_bridge_epsilon",
    "structural_bridge_sigma_scale"
  ) %in% args))
  expect_false(any(c(
    "structural_bridge_party_active_p",
    "structural_bridge_x_target_t",
    "structural_bridge_alpha_week"
  ) %in% args))
})

test_that("model8m9 reproduces known-good 2026 v0.9.1 x-drift bridge Stan data", {
  inputs <- readRDS(test_path("files", "model8m10_x_drift_bridge_2026_inputs_v0_9_1.rds"))
  expected <- readRDS(test_path("files", "model8m10_x_drift_bridge_2026_stan_data_v0_9_1.rds"))

  spd <- model8m9_stan_polls_data_from_fixture(inputs)

  expect_model8m9_has_only_drift_bridge_fields(spd$stan_data)
  model8m9_bridge_fields_identical(
    actual_stan_data = spd$stan_data[expected$bridge_field_names],
    expected_stan_data = expected$bridge_stan_data,
    bridge_field_names = expected$bridge_field_names
  )
})

test_that("model8m9 reproduces simulated v0.9.1 x-drift bridge Stan data", {
  fixture <- readRDS(test_path("files", "model8m10_x_drift_bridge_stan_data_v0_9_1.rds"))

  spd <- model8m9_stan_polls_data_from_fixture(fixture)

  expect_model8m9_has_only_drift_bridge_fields(spd$stan_data)
  model8m9_bridge_fields_identical(
    actual_stan_data = spd$stan_data[fixture$bridge_field_names],
    expected_stan_data = fixture$bridge_stan_data,
    bridge_field_names = fixture$bridge_field_names
  )
})

test_that("model8m9 accepts the current x-drift target-path alias", {
  inputs <- readRDS(test_path("files", "model8m10_x_drift_bridge_2026_inputs_v0_9_1.rds"))
  expected <- readRDS(test_path("files", "model8m10_x_drift_bridge_2026_stan_data_v0_9_1.rds"))
  inputs$hyper_parameters$structural_bridge_type <- "x_drift"
  inputs$hyper_parameters$structural_bridge_x_target_path <-
    inputs$hyper_parameters$structural_bridge_x_drift
  inputs$hyper_parameters$structural_bridge_x_drift <- NULL

  spd <- model8m9_stan_polls_data_from_fixture(inputs)

  expect_model8m9_has_only_drift_bridge_fields(spd$stan_data)
  model8m9_bridge_fields_identical(
    actual_stan_data = spd$stan_data[expected$bridge_field_names],
    expected_stan_data = expected$bridge_stan_data,
    bridge_field_names = expected$bridge_field_names
  )
})

test_that("model8m9 rejects constant-gain bridge data", {
  expect_error(
    model_config(
      model = "model8m9",
      x = list(
        use_softmax = 1L,
        structural_bridge_type = "constant_gain_pull"
      )
    ),
    "model8m9 only supports"
  )
  expect_error(
    model_config(
      model = "model8m9",
      x = list(
        use_softmax = 1L,
        structural_bridge_alpha_week = 0.10
      )
    ),
    "does not accept"
  )
})
