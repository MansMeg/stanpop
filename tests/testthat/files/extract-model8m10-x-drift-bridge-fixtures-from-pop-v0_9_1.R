#!/usr/bin/env Rscript

# Extract model8m10 x-drift structural bridge fixtures from a known-good
# stanpop 0.9.1 pop object.
#
# Usage:
#   Rscript tests/testthat/files/extract-model8m10-x-drift-bridge-fixtures-from-pop-v0_9_1.R [source_pop.rds]
#
# Defaults:
#   source_pop.rds = tmp_popdrift_prior_2026_091.rds
#
# Optional:
#   --overwrite    Replace existing output fixtures deliberately.
#
# Outputs:
#   tests/testthat/files/model8m10_x_drift_bridge_2026_inputs_v0_9_1.rds
#   tests/testthat/files/model8m10_x_drift_bridge_2026_stan_data_v0_9_1.rds

expected_stanpop_version <- "0.9.1"
default_source_file_name <- "tmp_popdrift_prior_2026_091.rds"
inputs_fixture_file_name <- "model8m10_x_drift_bridge_2026_inputs_v0_9_1.rds"
stan_data_fixture_file_name <- "model8m10_x_drift_bridge_2026_stan_data_v0_9_1.rds"

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
args <- commandArgs(trailingOnly = TRUE)
overwrite <- "--overwrite" %in% args
positional_args <- args[!startsWith(args, "--")]

source_path <- if(length(positional_args) >= 1L) {
  positional_args[[1L]]
} else {
  default_source_file_name
}

if(!file.exists(source_path)) {
  stop("Source pop RDS does not exist: ", source_path, call. = FALSE)
}
source_path <- normalizePath(source_path, mustWork = TRUE)

inputs_output_path <- file.path(script_dir, inputs_fixture_file_name)
stan_data_output_path <- file.path(script_dir, stan_data_fixture_file_name)
existing_outputs <- c(inputs_output_path, stan_data_output_path)[file.exists(c(inputs_output_path, stan_data_output_path))]
if(length(existing_outputs) > 0L && !overwrite) {
  stop(
    "Fixture output already exists:\n",
    paste(existing_outputs, collapse = "\n"), "\n",
    "Pass --overwrite only if you deliberately want to regenerate these fixtures.",
    call. = FALSE
  )
}

read_pop_payload <- function(path) {
  payload <- readRDS(path)
  pop <- if(is.list(payload) && !is.null(payload$pop)) payload$pop else payload

  list(
    payload = payload,
    pop = pop
  )
}

payload_stanpop_version <- function(payload, pop) {
  if(is.list(payload) && !is.null(payload$stanpop_version)) {
    return(as.character(payload$stanpop_version))
  }
  if(!is.null(pop$stanpop_version)) {
    return(as.character(pop$stanpop_version))
  }
  NA_character_
}

active_stan_data <- function(pop) {
  x <- pop$stan_data
  if(is.list(x) && !is.null(x$stan_data) && !is.null(x$stan_data$T)) {
    return(x$stan_data)
  }
  if(is.list(x) && !is.null(x$T)) {
    return(x)
  }

  stop("Could not find active Stan data in pop$stan_data.", call. = FALSE)
}

active_time_line <- function(pop) {
  x <- pop$stan_data
  if(is.list(x) && !is.null(x$time_line) && !is.null(x$time_line$time_line)) {
    return(x$time_line)
  }
  if(!is.null(pop$time_line) && !is.null(pop$time_line$time_line)) {
    return(pop$time_line)
  }

  stop("Could not find active time_line in the pop object.", call. = FALSE)
}

file_digest <- function(path) {
  if(requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(file = path, algo = "sha256"))
  }
  NA_character_
}

saved <- read_pop_payload(source_path)
payload <- saved$payload
pop <- saved$pop

fixture_stanpop_version <- payload_stanpop_version(payload, pop)
if(!identical(fixture_stanpop_version, expected_stanpop_version)) {
  stop(
    "Expected source pop to be created by stanpop ", expected_stanpop_version,
    ", but found stanpop_version = ", fixture_stanpop_version, ".",
    call. = FALSE
  )
}
if(!identical(pop$model, "model8m10")) {
  stop("Expected source pop model to be model8m10, but found ", pop$model, ".", call. = FALSE)
}

stan_data <- active_stan_data(pop)
time_line <- active_time_line(pop)
time_line_dates <- as.Date(time_line$time_line$date)

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
missing_bridge_fields <- setdiff(bridge_field_names, names(stan_data))
if(length(missing_bridge_fields) > 0L) {
  stop(
    "Source Stan data are missing expected bridge fields: ",
    paste(missing_bridge_fields, collapse = ", "),
    call. = FALSE
  )
}

bridge_stan_data <- stan_data[bridge_field_names]
active <- as.integer(stan_data$structural_bridge_active_t) == 1L
active_dates <- time_line_dates[active]
bridge_parties <- pop$y[as.integer(stan_data$structural_bridge_party)]

expected_active_dates <- c(
  seq(as.Date("2026-06-15"), as.Date("2026-08-17"), by = "week"),
  seq(as.Date("2026-08-24"), as.Date("2026-09-13"), by = "day")
)

stopifnot(
  identical(stan_data$T, 1307L),
  identical(stan_data$P, 8L),
  identical(stan_data$structural_bridge_type, 1L),
  identical(stan_data$structural_bridge_B, 2L),
  identical(bridge_parties, c("L", "KD")),
  identical(active_dates, expected_active_dates),
  isTRUE(all.equal(colSums(stan_data$structural_bridge_delta_x), c(0.018, 0.005), tolerance = 1e-15)),
  identical(stan_data$structural_bridge_epsilon, 1e-6),
  identical(as.numeric(stan_data$structural_bridge_sigma_scale), rep(1, stan_data$P)),
  sum(active & seq_len(stan_data$T) %in% as.integer(stan_data$x_known_t)) == 0L,
  sum(active & as.numeric(stan_data$delta_days_t) == 0) == 0L
)

source_info <- list(
  source_path = source_path,
  source_basename = basename(source_path),
  source_sha256 = file_digest(source_path),
  source_payload_format = if(is.list(payload)) payload$format %||% NA_character_ else NA_character_,
  source_payload_format_version = if(is.list(payload)) payload$format_version %||% NA_integer_ else NA_integer_,
  source_saved_at = if(is.list(payload)) payload$saved_at %||% NA_character_ else NA_character_,
  source_backend = if(is.list(payload)) payload$backend %||% pop$backend %||% NA_character_ else pop$backend %||% NA_character_,
  source_backend_package_version = if(is.list(payload)) payload$backend_package_version %||% NA_character_ else NA_character_
)

inputs_fixture <- list(
  format = "stanpop_model8m10_x_drift_bridge_2026_inputs_fixture",
  format_version = 1L,
  generated_at = Sys.time(),
  stanpop_version = fixture_stanpop_version,
  source_info = source_info,
  model = pop$model,
  y = pop$y,
  polls_data = pop$polls_data,
  time_scale = pop$time_scale,
  time_scale_overrides = pop$time_scale_overrides,
  known_state = pop$known_state,
  model_time_range = pop$model_time_range,
  latent_time_ranges = pop$latent_time_range,
  hyper_parameters = pop$input_args$hyper_parameters,
  slow_scales = pop$input_args$slow_scales,
  bridge_api = list(
    stanpop_0_9_1_target_path_field = "structural_bridge_x_drift",
    stanpop_0_9_2_target_path_field = "structural_bridge_x_target_path"
  )
)

stan_data_fixture <- list(
  format = "stanpop_model8m10_x_drift_bridge_2026_stan_data_fixture",
  format_version = 1L,
  generated_at = Sys.time(),
  stanpop_version = fixture_stanpop_version,
  source_info = source_info,
  model = pop$model,
  y = pop$y,
  bridge_field_names = bridge_field_names,
  bridge_stan_data = bridge_stan_data,
  time_line_dates = time_line_dates,
  active_bridge_summary = list(
    active_rows = sum(active),
    first_active_date = min(active_dates),
    last_active_date = max(active_dates),
    active_dates = active_dates,
    bridge_parties = bridge_parties,
    delta_x_sums = stats::setNames(
      as.numeric(colSums(stan_data$structural_bridge_delta_x)),
      bridge_parties
    ),
    known_active_rows = sum(active & seq_len(stan_data$T) %in% as.integer(stan_data$x_known_t)),
    zero_day_active_rows = sum(active & as.numeric(stan_data$delta_days_t) == 0)
  )
)

saveRDS(inputs_fixture, inputs_output_path, compress = "xz")
saveRDS(stan_data_fixture, stan_data_output_path, compress = "xz")

message("Wrote ", inputs_output_path)
message("Wrote ", stan_data_output_path)
