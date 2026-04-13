context("pop io")

make_mock_pop_for_io <- function(backend = "rstan") {
  structure(
    list(
      y = "x",
      model = "model8k1",
      backend = backend
    ),
    class = c("pop_model8k1", "poll_of_polls")
  )
}

test_that("save_pop stores a wrapped poll_of_polls payload and load_pop restores it", {
  pop <- make_mock_pop_for_io()
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  # Silently return the pop
  expect_identical(save_pop(pop, tmp), pop)

  payload <- readRDS(tmp)
  loaded <- load_pop(tmp)

  expect_identical(payload$format, "stanpop_pop")
  expect_identical(payload$format_version, 1L)
  expect_identical(payload$backend, pop$backend)
  expect_identical(payload$pop, pop)
  expect_true(inherits(payload$saved_at, "POSIXt"))
  expect_identical(loaded, pop)
})

test_that("load_pop rejects files not created by save_pop", {
  pop <- make_mock_pop_for_io()
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  saveRDS(pop, file = tmp)

  expect_error(
    load_pop(tmp),
    "File was not created by save_pop\\(\\)\\."
  )
})
