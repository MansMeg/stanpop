context("diagnostics")

test_that("parse_adaption_information parses diagonal inverse metrics", {
  parse_adaption_information <- get_internal("parse_adaption_information")

  x <- paste(
    "# Adaptation terminated",
    "# Step size = 0.125",
    "#",
    "# Diagonal elements of inverse mass matrix:",
    "# 1.5, 2.5, 3.5",
    sep = "\n"
  )

  res <- parse_adaption_information(x)

  expect_true(isTRUE(res$adaption_terminated))
  expect_equal(res$step_size, 0.125, tolerance = 0)
  expect_identical(res$metric_type, "diag_e")
  expect_equal(res$inv_metric, c(1.5, 2.5, 3.5), tolerance = 0)
  expect_equal(res$diag_inv_mass_matrix, c(1.5, 2.5, 3.5), tolerance = 0)
})

test_that("parse_adaption_information parses dense inverse metrics", {
  parse_adaption_information <- get_internal("parse_adaption_information")

  x <- paste(
    "# Adaptation terminated",
    "# Step size = 0.2",
    "#",
    "# Elements of inverse mass matrix:",
    "# 1.0, 0.25",
    "# 0.25, 2.0",
    sep = "\n"
  )

  res <- parse_adaption_information(x)

  expect_true(isTRUE(res$adaption_terminated))
  expect_equal(res$step_size, 0.2, tolerance = 0)
  expect_identical(res$metric_type, "dense_e")
  expect_equal(
    res$inv_metric,
    matrix(c(1.0, 0.25, 0.25, 2.0), nrow = 2, byrow = TRUE),
    tolerance = 0
  )
  expect_equal(res$diag_inv_mass_matrix, c(1.0, 2.0), tolerance = 0)
})
