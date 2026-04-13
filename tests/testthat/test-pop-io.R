context("pop io")

make_mock_cmdstanr_fit_for_io <- function(materialized_fit = list(materialized = TRUE)) {
  list(
    save_object = function(file, ...) {
      saveRDS(materialized_fit, file = file)
      invisible(materialized_fit)
    }
  )
}

make_mock_pop_for_io <- function(backend = "rstan", stan_fit = NULL) {
  if(is.null(stan_fit) && identical(backend, "cmdstanr")) {
    stan_fit <- make_mock_cmdstanr_fit_for_io()
  }

  structure(
    list(
      y = "x",
      model = "model8k1",
      backend = backend,
      stan_fit = stan_fit
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

test_that("prepare_pop_for_save is a no-op for rstan", {
  pop_rstan <- make_mock_pop_for_io("rstan")

  expect_identical(prepare_pop_for_save(pop_rstan), pop_rstan)
})

test_that("prepare_cmdstanr_pop_for_save materializes the stored fit", {
  materialized_fit <- list(materialized = TRUE, draws = 42)
  pop_cmdstanr <- make_mock_pop_for_io(
    "cmdstanr",
    stan_fit = make_mock_cmdstanr_fit_for_io(materialized_fit = materialized_fit)
  )

  prepared <- prepare_pop_for_save(pop_cmdstanr)

  expect_identical(prepared$backend, "cmdstanr")
  expect_identical(prepared$stan_fit, materialized_fit)
})

test_that("save_pop stores the materialized cmdstanr-backed pop", {
  pop <- make_mock_pop_for_io("cmdstanr")
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  saved <- save_pop(pop, tmp)

  payload <- readRDS(tmp)
  expect_identical(saved$stan_fit, list(materialized = TRUE))
  expect_identical(payload$backend, "cmdstanr")
  expect_identical(payload$pop$stan_fit, list(materialized = TRUE))
})

test_that("load_pop rejects files not created by save_pop", {
  pop <- make_mock_pop_for_io()
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  saveRDS(pop, file = tmp)

  expect_error(
    load_pop(tmp),
    "Missing 'format' field in save_pop\\(\\) payload\\."
  )
})

test_that("load_pop rejects unsupported save_pop format versions", {
  pop <- make_mock_pop_for_io()
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  payload <- pop_save_payload(pop)
  payload$format_version <- 2L
  saveRDS(payload, file = tmp)

  expect_error(
    load_pop(tmp),
    "Unsupported save_pop\\(\\) format version '2'\\. Expected '1'\\."
  )
})

test_that("load_pop rejects payloads missing the pop field", {
  pop <- make_mock_pop_for_io()
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  payload <- pop_save_payload(pop)
  payload$pop <- NULL
  saveRDS(payload, file = tmp)

  expect_error(
    load_pop(tmp),
    "Missing 'pop' field in save_pop\\(\\) payload\\."
  )
})

test_that("load_pop rejects payloads whose backend disagrees with pop$backend", {
  pop <- make_mock_pop_for_io()
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  payload <- pop_save_payload(pop)
  payload$backend <- "cmdstanr"
  saveRDS(payload, file = tmp)

  expect_error(
    load_pop(tmp),
    "The payload backend does not match pop\\$backend\\."
  )
})
