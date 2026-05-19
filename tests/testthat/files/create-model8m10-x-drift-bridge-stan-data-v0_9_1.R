#!/usr/bin/env Rscript

# Generate the one-shot model8m10 x-drift structural bridge Stan-data fixture
# from installed stanpop 0.9.1.
#
# Usage:
#   Rscript tests/testthat/files/create-model8m10-x-drift-bridge-stan-data-v0_9_1.R
#
# Optional:
#   --overwrite    Replace an existing fixture deliberately.
#
# Output:
#   tests/testthat/files/model8m10_x_drift_bridge_stan_data_v0_9_1.rds

expected_stanpop_version <- "0.9.1"
fixture_file_name <- "model8m10_x_drift_bridge_stan_data_v0_9_1.rds"

`%||%` <- function(x, y) {
  if(is.null(x)) y else x
}

script_path <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if(length(file_arg) == 1L) {
    normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
  } else {
    normalizePath(sys.frame(1L)$ofile %||% file.path(getwd(), "script.R"), mustWork = FALSE)
  }
})

script_dir <- dirname(script_path)
output_path <- file.path(script_dir, fixture_file_name)
args <- commandArgs(trailingOnly = TRUE)
overwrite <- "--overwrite" %in% args

suppressPackageStartupMessages(library(stanpop))

actual_stanpop_version <- as.character(utils::packageVersion("stanpop"))
if(!identical(actual_stanpop_version, expected_stanpop_version)) {
  stop(
    "This fixture must be generated with installed stanpop ",
    expected_stanpop_version, ", but loaded stanpop ",
    actual_stanpop_version, " from ", find.package("stanpop"), ".",
    call. = FALSE
  )
}

if(file.exists(output_path) && !overwrite) {
  stop(
    "Fixture already exists: ", output_path, "\n",
    "This generator is intended to run once under stanpop 0.9.1. ",
    "Pass --overwrite only if you deliberately want to regenerate it.",
    call. = FALSE
  )
}

parties <- c("M", "L", "C", "KD", "S", "V", "MP", "SD")
polls_y <- data.frame(
  M = c(0.190, 0.200, 0.180),
  L = c(0.040, 0.045, 0.044),
  C = c(0.070, 0.068, 0.065),
  KD = c(0.050, 0.052, 0.054),
  S = c(0.300, 0.290, 0.310),
  V = c(0.080, 0.075, 0.070),
  MP = c(0.050, 0.048, 0.052),
  SD = c(0.200, 0.205, 0.190)
)

polls <- polls_data(
  y = polls_y,
  house = factor(c("A", "B", "A")),
  publish_date = as.Date(c("2026-05-01", "2026-07-01", "2026-09-01")),
  start_date = as.Date(c("2026-04-25", "2026-06-25", "2026-08-25")),
  end_date = as.Date(c("2026-04-30", "2026-06-30", "2026-08-31")),
  n = c(1000L, 1200L, 1500L)
)

known_state <- data.frame(
  date = as.Date("2022-09-11"),
  M = 0.1910,
  L = 0.0461,
  C = 0.0671,
  KD = 0.0534,
  S = 0.3033,
  V = 0.0675,
  MP = 0.0508,
  SD = 0.2054
)

time_scale <- "week"
time_scale_overrides <- data.frame(
  from = as.Date("2026-08-24"),
  to = as.Date("2026-09-13"),
  time_scale = "day"
)
model_time_range <- time_range(c("2022-09-11", "2026-09-13"))

hyper_parameters <- list(
  use_industry_bias = 0L,
  use_house_bias = 0L,
  use_design_effects = 0L,
  use_softmax = 1L,
  use_multivariate_version = 2L,
  structural_bridge_window = as.Date(c("2026-06-08", "2026-09-13")),
  structural_bridge_x_drift = data.frame(
    y = c("L", "KD"),
    from_x = c(0.026, 0.049),
    to_x = c(0.044, 0.054)
  )
)

spd <- suppressWarnings(
  suppressMessages(
    stan_polls_data(
      x = polls,
      y_name = parties,
      model = "model8m10",
      time_scale = time_scale,
      time_scale_overrides = time_scale_overrides,
      known_state = known_state,
      model_time_range = model_time_range,
      hyper_parameters = hyper_parameters
    )
  )
)

stan_data <- spd$stan_data
time_line <- spd$time_line

bridge_field_names <- c(
  "T",
  "P",
  "structural_bridge_type",
  "structural_bridge_active_t",
  "structural_bridge_B",
  "structural_bridge_party",
  "structural_bridge_delta_x",
  "structural_bridge_epsilon",
  "structural_bridge_sigma_scale",
  "delta_days_t",
  "step_scale_t",
  "x_known_t",
  "x_known"
)

bridge_stan_data <- stan_data[bridge_field_names]
active <- as.integer(stan_data$structural_bridge_active_t) == 1L
active_dates <- as.Date(time_line$time_line$date[active])

expected_active_dates <- c(
  seq(as.Date("2026-06-15"), as.Date("2026-08-17"), by = "week"),
  seq(as.Date("2026-08-24"), as.Date("2026-09-13"), by = "day")
)

stopifnot(
  identical(stan_data$structural_bridge_type, 1L),
  identical(stan_data$structural_bridge_B, 2L),
  identical(as.integer(stan_data$structural_bridge_party), c(2L, 4L)),
  identical(active_dates, expected_active_dates),
  isTRUE(all.equal(colSums(stan_data$structural_bridge_delta_x), c(0.018, 0.005), tolerance = 1e-15)),
  sum(active & seq_len(stan_data$T) %in% as.integer(stan_data$x_known_t)) == 0L,
  sum(active & as.numeric(stan_data$delta_days_t) == 0) == 0L
)

fixture <- list(
  format = "stanpop_model8m10_x_drift_bridge_stan_data_fixture",
  format_version = 1L,
  generated_at = Sys.time(),
  stanpop_version = actual_stanpop_version,
  stanpop_path = find.package("stanpop"),
  model = "model8m10",
  parties = parties,
  time_scale = time_scale,
  time_scale_overrides = time_scale_overrides,
  model_time_range = model_time_range,
  known_state = known_state,
  polls_data = polls,
  hyper_parameters = hyper_parameters,
  bridge_field_names = bridge_field_names,
  bridge_stan_data = bridge_stan_data,
  time_line = time_line,
  active_bridge_summary = list(
    active_rows = sum(active),
    first_active_date = min(active_dates),
    last_active_date = max(active_dates),
    active_dates = active_dates,
    delta_x_sums = stats::setNames(
      as.numeric(colSums(stan_data$structural_bridge_delta_x)),
      hyper_parameters$structural_bridge_x_drift$y
    ),
    known_active_rows = sum(active & seq_len(stan_data$T) %in% as.integer(stan_data$x_known_t)),
    zero_day_active_rows = sum(active & as.numeric(stan_data$delta_days_t) == 0)
  )
)

saveRDS(fixture, output_path, compress = "xz")
message("Wrote ", output_path)
