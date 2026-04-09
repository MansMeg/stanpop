context("stan_data v0.4 noop fixture regression")

test_that("model8k2 no-override stan_data matches the v0.4 fixture exactly", {
  fixture <- readRDS(test_path("files", "model8k2_v040_noop_fixture.rds"))

  sd <- suppressWarnings(
    suppressMessages(
      stan_polls_data(
        x = fixture$polls_data,
        time_scale = fixture$time_scale,
        y_name = fixture$parties,
        model = fixture$model,
        known_state = fixture$known_state,
        hyper_parameters = fixture$hyper_parameters
      )
    )
  )

  expect_identical(sd$stan_data, fixture$stan_data)
  expect_identical(sd$stan_data_with_overrides[names(fixture$stan_data)], fixture$stan_data)
})
