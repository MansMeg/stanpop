context("refit")

make_mock_pop_for_refit_helpers <- function(backend = c("cmdstanr", "rstan")){
  backend <- match.arg(backend)
  pd <- polls_data(
    y = data.frame(x = c(0.4, 0.45)),
    house = factor(c("A", "A")),
    publish_date = as.Date(c("2020-01-02", "2020-01-09")),
    start_date = as.Date(c("2020-01-01", "2020-01-08")),
    end_date = as.Date(c("2020-01-02", "2020-01-09")),
    n = c(1000L, 1000L),
    poll_id = c("p1", "p2")
  )

  if(backend == "cmdstanr") {
    sample_args <- list(
      chains = 2,
      iter_warmup = 250,
      iter_sampling = 500,
      seed = 4711
    )
    compile_arguments <- list(stanc_options = list("O1"))
  } else {
    sample_args <- list(
      chains = 2,
      iter = 750,
      warmup = 250,
      cores = 2,
      seed = 4711,
      refresh = 0,
      control = list(
        adapt_delta = 0.9,
        max_treedepth = 11L,
        stepsize = 0.15,
        metric = "diag_e"
      )
    )
    compile_arguments <- NULL
  }

  structure(
    list(
      y = "x",
      model = "model8k5",
      backend = backend,
      compile_arguments = compile_arguments,
      polls_data = pd,
      time_scale = "day",
      time_scale_overrides = NULL,
      known_state = tibble::tibble(date = as.Date("2020-01-01"), x = 0.41),
      model_time_range = time_range(c(as.Date("2020-01-01"), as.Date("2020-01-31"))),
      latent_time_range = list(x = time_range(c(as.Date("2020-01-01"), as.Date("2020-01-31")))),
      stan_arguments = sample_args,
      stan_fit = structure(list(), class = "mock_stan_fit"),
      model_arguments = list(use_softmax = 1L),
      cache_dir = NULL,
      input_args = list(
        y = "x",
        model = "model8k5",
        backend = backend,
        compile_args = compile_arguments,
        time_scale = "day",
        time_scale_overrides = NULL,
        model_time_range = NULL,
        latent_time_ranges = NULL,
        hyper_parameters = list(use_softmax = 1L),
        slow_scales = as.Date("2020-01-15"),
        stan_arguments = sample_args,
        cache_dir = NULL
      )
    ),
    class = c("pop_model8k5", "poll_of_polls")
  )
}

resolve_refit_arguments_for_test <- function(x,
                                             warm_start = list(),
                                             backend = "cmdstanr",
                                             dots = list()) {
  resolve_refit_poll_of_polls_arguments <- get_internal("resolve_refit_poll_of_polls_arguments")
  refit_materialize_warm_start_arguments <- get_internal("refit_materialize_warm_start_arguments")

  resolved <- resolve_refit_poll_of_polls_arguments(
    x = x,
    backend = backend,
    dots = dots,
    warm_start = warm_start
  )
  resolved$sample_args <- refit_materialize_warm_start_arguments(
    x = x,
    backend = resolved$constructor_args$backend,
    sample_args = resolved$sample_args,
    warm_start = resolved$warm_start
  )

  c(resolved$constructor_args, resolved$sample_args)
}

test_that("extract_poll_of_polls_refit_arguments returns constructor arguments only", {
  pop <- make_mock_pop_for_refit_helpers()

  args <- extract_poll_of_polls_refit_arguments(pop)

  expect_s3_class(args, "poll_of_polls_refit_arguments")
  expect_s3_class(args, "poll_of_polls_input_arguments")
  expect_setequal(names(args), setdiff(names(formals(poll_of_polls)), "..."))
  expect_false("stan_arguments" %in% names(args))
  expect_identical(args$polls_data, pop$polls_data)
  expect_identical(args$known_state, pop$known_state)
  expect_identical(args$backend, "cmdstanr")
  expect_identical(args$compile_args, pop$compile_arguments)
  expect_null(args$model_time_range)
  expect_null(args$latent_time_ranges)
  expect_identical(args$slow_scales, as.Date("2020-01-15"))
})

test_that("extract_poll_of_polls_input_arguments is a backward-compatible alias", {
  pop <- make_mock_pop_for_refit_helpers()

  expect_identical(
    unclass(extract_poll_of_polls_input_arguments(pop)),
    unclass(extract_poll_of_polls_refit_arguments(pop))
  )
})

test_that("extract_poll_of_polls_sample_arguments returns stored sample arguments", {
  pop <- make_mock_pop_for_refit_helpers()

  args <- extract_poll_of_polls_sample_arguments(pop)

  expect_s3_class(args, "poll_of_polls_sample_arguments")
  expect_identical(
    unclass(args),
    pop$stan_arguments
  )
})

test_that("extract_poll_of_polls_sample_arguments falls back to nested input args", {
  pop <- make_mock_pop_for_refit_helpers()
  pop$stan_arguments <- NULL

  args <- extract_poll_of_polls_sample_arguments(pop)

  expect_identical(
    unclass(args),
    pop$input_args$stan_arguments
  )
})

test_that("refit_poll_of_polls has a public override-oriented signature", {
  expect_identical(
    names(formals(refit_poll_of_polls)),
    c("x", "warm_start", "backend", "...")
  )
  expect_false("y" %in% names(formals(refit_poll_of_polls)))
  expect_false("model" %in% names(formals(refit_poll_of_polls)))
})

test_that("refit_poll_of_polls defaults to cmdstanr and inherits omitted arguments", {
  pop <- make_mock_pop_for_refit_helpers("rstan")

  testthat::local_mocked_bindings(
    backend_get_last_draws_for_init = function(...) {
      list(list(x = c(0.11, 0.22)), list(x = c(0.33, 0.44)))
    },
    backend_get_sampler_state = function(...) {
      list(
        list(step_size = 0.12, inv_metric = c(1, 2), metric_type = "diag_e"),
        list(step_size = 0.34, inv_metric = c(3, 4), metric_type = "diag_e")
      )
    },
    .package = "stanpop"
  )

  args <- resolve_refit_arguments_for_test(pop)

  expect_identical(args$y, pop$y)
  expect_identical(args$model, pop$model)
  expect_identical(args$polls_data, pop$polls_data)
  expect_identical(args$known_state, pop$known_state)
  expect_identical(args$backend, "cmdstanr")
  expect_identical(args$iter_warmup, 250)
  expect_identical(args$iter_sampling, 500)
  expect_identical(args$parallel_chains, 2)
  expect_identical(args$adapt_delta, 0.9)
  expect_identical(args$max_treedepth, 11L)
  expect_identical(args$init, list(list(x = c(0.11, 0.22)), list(x = c(0.33, 0.44))))
  expect_identical(args$inv_metric, list(c(1, 2), c(3, 4)))
  expect_equal(args$step_size, c(0.12, 0.34))
  expect_identical(args$metric, "diag_e")
})

test_that("refit_poll_of_polls treats explicit NULL constructor arguments as overrides", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")
  args <- resolve_refit_arguments_for_test(
    pop,
    warm_start = list(
      init = NULL,
      inv_metric = NULL,
      step_size = NULL
    ),
    dots = list(
      known_state = NULL,
      compile_args = NULL,
      cache_dir = NULL
    )
  )

  expect_null(args$known_state)
  expect_null(args$compile_args)
  expect_null(args$cache_dir)
})

test_that("refit_poll_of_polls merges sampler overrides and removes NULL entries", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")
  args <- resolve_refit_arguments_for_test(
    pop,
    warm_start = list(
      init = NULL,
      inv_metric = NULL,
      step_size = NULL
    ),
    dots = list(
      chains = 4,
      seed = NULL,
      iter_sampling = 900
    )
  )

  expect_identical(args$chains, 4)
  expect_identical(args$iter_warmup, 250)
  expect_identical(args$iter_sampling, 900)
  expect_false("seed" %in% names(args))
})

test_that("explicit warm-start arguments override reuse defaults", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")

  testthat::local_mocked_bindings(
    backend_get_last_draws_for_init = function(...) {
      stop("last draw reuse should not be called")
    },
    backend_get_sampler_state = function(...) {
      stop("sampler state reuse should not be called")
    },
    .package = "stanpop"
  )

  explicit_init <- list(list(x = c(9, 9)), list(x = c(8, 8)))
  explicit_inv_metric <- list(c(4, 5), c(6, 7))

  args <- resolve_refit_arguments_for_test(
    pop,
    warm_start = list(
      init = explicit_init,
      inv_metric = explicit_inv_metric,
      metric_type = "diag_e",
      step_size = c(0.21, 0.22)
    )
  )

  expect_identical(args$init, explicit_init)
  expect_identical(args$inv_metric, explicit_inv_metric)
  expect_identical(args$metric, "diag_e")
  expect_equal(args$step_size, c(0.21, 0.22))
})

test_that("refit_poll_of_polls can disable default warm-start values via NULL", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")
  args <- resolve_refit_arguments_for_test(
    pop,
    warm_start = list(
      init = NULL,
      inv_metric = NULL,
      step_size = NULL
    )
  )

  expect_false("init" %in% names(args))
  expect_false("inv_metric" %in% names(args))
  expect_false("step_size" %in% names(args))
})

test_that("refit_poll_of_polls v1 only supports cmdstanr", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")

  expect_error(
    resolve_refit_arguments_for_test(pop, backend = "rstan"),
    "currently only supports backend = 'cmdstanr'"
  )
  expect_error(
    refit_poll_of_polls(pop, backend = "rstan"),
    "currently only supports backend = 'cmdstanr'"
  )
})

test_that("refit_poll_of_polls errors when warm-start names are supplied in dots", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")

  expect_error(
    refit_poll_of_polls(pop, init = NULL),
    "Warm-start overrides belong in 'warm_start'"
  )
  expect_error(
    refit_poll_of_polls(pop, inv_metric = c(1, 2)),
    "Warm-start overrides belong in 'warm_start'"
  )
  expect_error(
    refit_poll_of_polls(pop, metric_type = "diag_e"),
    "Warm-start overrides belong in 'warm_start'"
  )
})

test_that("refit_poll_of_polls errors when y or model are supplied through dots", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")

  expect_error(
    refit_poll_of_polls(pop, y = "other"),
    "always inherits 'y' and 'model'"
  )
})

test_that("refit_poll_of_polls validates warm_start names", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")

  expect_error(
    refit_poll_of_polls(pop, warm_start = list(metric = "diag_e")),
    "Use 'metric_type' rather than 'metric' in 'warm_start'"
  )
  expect_error(
    refit_poll_of_polls(pop, warm_start = list(unknown = 1)),
    "Unknown 'warm_start' element"
  )
})
