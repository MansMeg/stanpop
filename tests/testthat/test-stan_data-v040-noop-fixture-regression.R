context("stan_data v0.4 noop fixture regression")

describe_object_storage <- function(x) {
  paste0(
    "typeof=", typeof(x),
    ", class=", paste(class(x), collapse = "/")
  )
}

first_non_identical_field_report <- function(actual, expected) {
  checkmate::assert_list(actual)
  checkmate::assert_list(expected)

  all_names <- union(names(actual), names(expected))
  diff_names <- all_names[!vapply(all_names, function(nm) identical(actual[[nm]], expected[[nm]]), logical(1))]

  if(length(diff_names) < 1) {
    return(c(
      "Objects differ, but no non-identical named field was found.",
      paste0("actual attributes: ", paste(names(attributes(actual)), collapse = ", ")),
      paste0("expected attributes: ", paste(names(attributes(expected)), collapse = ", "))
    ))
  }

  nm <- diff_names[[1]]
  x_actual <- actual[[nm]]
  x_expected <- expected[[nm]]
  equal_by_value <- tryCatch(isTRUE(all.equal(x_actual, x_expected)), error = function(e) FALSE)

  lines <- c(
    paste0("First non-identical field: ", nm),
    paste0("actual:   ", describe_object_storage(x_actual)),
    paste0("expected: ", describe_object_storage(x_expected)),
    paste0("equal by value: ", equal_by_value)
  )

  if(equal_by_value && is.atomic(x_actual) && is.atomic(x_expected) &&
     length(x_actual) <= 10 && length(x_expected) <= 10) {
    lines <- c(
      lines,
      paste0("actual values:   ", paste(x_actual, collapse = ", ")),
      paste0("expected values: ", paste(x_expected, collapse = ", "))
    )
  }

  if(length(diff_names) > 1) {
    lines <- c(
      lines,
      paste0("Additional differing fields: ", paste(diff_names[-1], collapse = ", "))
    )
  }

  lines
}

expect_identical_with_field_report <- function(actual, expected, label) {
  if(identical(actual, expected)) {
    testthat::succeed()
    return(invisible(NULL))
  }

  details <- paste(first_non_identical_field_report(actual, expected), collapse = "\n")
  testthat::fail(paste(label, details, sep = "\n"))
}

expect_fixture_stan_data_match <- function(actual, expected, label) {
  expect_identical(names(actual), names(expected), label = paste(label, "field names"))

  for(nm in names(expected)) {
    if(nm == "tw") {
      expect_equal(
        actual[[nm]],
        expected[[nm]],
        tolerance = 1e-15,
        label = paste(label, "field", nm)
      )
    } else {
      expect_identical_with_field_report(
        actual = stats::setNames(list(actual[[nm]]), nm),
        expected = stats::setNames(list(expected[[nm]]), nm),
        label = paste(label, "field", nm)
      )
    }
  }
}

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

  expect_fixture_stan_data_match(
    sd$stan_data,
    fixture$stan_data,
    label = "Expected sd$stan_data to be identical to fixture$stan_data."
  )
  expect_fixture_stan_data_match(
    sd$stan_data_with_overrides[names(fixture$stan_data)],
    fixture$stan_data,
    label = "Expected sd$stan_data_with_overrides[names(fixture$stan_data)] to be identical to fixture$stan_data."
  )
})
