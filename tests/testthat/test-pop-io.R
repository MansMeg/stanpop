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

test_that("save_pop creates a self-contained cmdstanr file that still extracts after output files are removed", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  case <- make_model8_mixed_smoke_case(npolls = 12)
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop <- poll_of_polls(
            y = case$parties,
            model = "model8k5",
            polls_data = case$polls_data,
            time_scale = case$time_scale,
            time_scale_overrides = case$time_scale_overrides,
            known_state = case$known_state,
            hyper_parameters = cfg,
            backend = "cmdstanr",
            iter_sampling = 5,
            iter_warmup = 5,
            chains = 1,
            refresh = 0,
            seed = 4711,
            cache_dir = NULL
          )
        )
      )
    )
  )

  tmp <- tempfile(fileext = ".rds")
  output_files <- pop$stan_fit$output_files()
  on.exit(unlink(c(tmp, output_files)), add = TRUE)

  expect_true(length(output_files) > 0L)
  expect_true(all(file.exists(output_files)))

  save_pop(pop, tmp)
  unlink(output_files)
  expect_false(any(file.exists(output_files)))

  reloaded <- load_pop(tmp)
  x_pred <- extract(reloaded, pars = "x_pred")$x_pred
  ls <- latent_state(reloaded)
  md <- get_model_diagnostics(reloaded)
  sc <- get_stancode(reloaded)
  sd <- get_stan_date(reloaded)
  recomp <- recompile_stanfit(reloaded)

  expect_true(all(is.finite(x_pred)))
  expect_identical(reloaded$backend, "cmdstanr")
  expect_true(all(is.finite(ls$latent_state)))
  expect_true(all(is.finite(unlist(md))))
  expect_true(nzchar(sc))
  expect_true(inherits(sd, "POSIXt"))
  expect_s4_class(recomp, "stanfit")
})

test_that("reloaded cmdstanr pop can be refit after the original output files are removed", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  case <- make_model8_mixed_smoke_case(npolls = 12)
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop <- poll_of_polls(
            y = case$parties,
            model = "model8k5",
            polls_data = case$polls_data,
            time_scale = case$time_scale,
            time_scale_overrides = case$time_scale_overrides,
            known_state = case$known_state,
            hyper_parameters = cfg,
            backend = "cmdstanr",
            iter_sampling = 5,
            iter_warmup = 5,
            chains = 1,
            refresh = 0,
            seed = 4711,
            cache_dir = NULL
          )
        )
      )
    )
  )

  tmp <- tempfile(fileext = ".rds")
  output_files <- pop$stan_fit$output_files()
  on.exit(unlink(c(tmp, output_files)), add = TRUE)

  save_pop(pop, tmp)
  unlink(output_files)
  expect_false(any(file.exists(output_files)))

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          refit <- refit_poll_of_polls(
            load_pop(tmp),
            iter_warmup = 0,
            iter_sampling = 3,
            adapt_engaged = FALSE,
            refresh = 0,
            seed = 4712,
            cache_dir = NULL
          )
        )
      )
    )
  )

  x_pred <- extract(refit, pars = "x_pred")$x_pred

  expect_identical(refit$backend, "cmdstanr")
  expect_true(all(is.finite(x_pred)))
  expect_true(isTRUE(refit$warm_start_state$init_complete))
})

test_that("reloaded rstan pop can be refit through cmdstanr", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()
  assert_rstan_available()

  case <- make_model8_mixed_smoke_case(npolls = 12)
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop <- poll_of_polls(
            y = case$parties,
            model = "model8k5",
            polls_data = case$polls_data,
            time_scale = case$time_scale,
            time_scale_overrides = case$time_scale_overrides,
            known_state = case$known_state,
            hyper_parameters = cfg,
            backend = "rstan",
            iter = 15,
            warmup = 10,
            chains = 1,
            refresh = 0,
            seed = 4711,
            cache_dir = NULL
          )
        )
      )
    )
  )

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  save_pop(pop, tmp)

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          refit <- refit_poll_of_polls(
            load_pop(tmp),
            iter_warmup = 0,
            iter_sampling = 3,
            adapt_engaged = FALSE,
            refresh = 0,
            seed = 4712,
            cache_dir = NULL
          )
        )
      )
    )
  )

  x_pred <- extract(refit, pars = "x_pred")$x_pred

  expect_identical(refit$backend, "cmdstanr")
  expect_true(all(is.finite(x_pred)))
  expect_true(isTRUE(refit$warm_start_state$init_complete))
})

test_that("poll_of_polls cache uses wrapped save_pop objects for cmdstanr fits", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  case <- make_model8_mixed_smoke_case(npolls = 12)
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  cache_dir <- tempfile("pop-cache-")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  fit_args <- list(
    y = case$parties,
    model = "model8k5",
    polls_data = case$polls_data,
    time_scale = case$time_scale,
    time_scale_overrides = case$time_scale_overrides,
    known_state = case$known_state,
    hyper_parameters = cfg,
    backend = "cmdstanr",
    iter_sampling = 5,
    iter_warmup = 5,
    chains = 1,
    refresh = 0,
    seed = 4711,
    cache_dir = cache_dir
  )

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          pop <- do.call(poll_of_polls, fit_args)
        )
      )
    )
  )

  output_files <- pop$stan_fit$output_files()
  expect_true(length(output_files) > 0L)
  expect_true(all(file.exists(output_files)))

  unlink(output_files)
  expect_false(any(file.exists(output_files)))

  cached_pop <- NULL
  expect_message(
    cached_pop <- suppressWarnings(do.call(poll_of_polls, fit_args)),
    "Cached results used\\."
  )

  x_pred <- extract(cached_pop, pars = "x_pred")$x_pred

  expect_identical(cached_pop$backend, "cmdstanr")
  expect_true(all(is.finite(x_pred)))
})
