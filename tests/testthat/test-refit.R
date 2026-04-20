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
      time_line = list(slow_scales = as.Date("2020-01-15")),
      stan_arguments = sample_args,
      stan_fit = structure(list(), class = "mock_stan_fit"),
      warm_start_state = NULL,
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
        latent_time_ranges = list(
          x = list(
            from = as.Date("2020-01-01"),
            to = as.Date("2020-01-31")
          )
        ),
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
                                             polls_data = x$polls_data,
                                             backend = "cmdstanr",
                                             compile_args = x$compile_arguments,
                                             cache_dir = x$cache_dir,
                                             warm_start = list(),
                                             dots = list()) {
  resolve_refit_poll_of_polls_arguments <- get_internal("resolve_refit_poll_of_polls_arguments")
  refit_materialize_warm_start_arguments <- get_internal("refit_materialize_warm_start_arguments")

  resolved <- resolve_refit_poll_of_polls_arguments(
    x = x,
    polls_data = polls_data,
    backend = backend,
    compile_args = compile_args,
    cache_dir = cache_dir,
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

# Shared model8k5 regression setup used by the warm-start refit tests below.
# It fits an initial cmdstanr object, perturbs the cached warm_start_state with
# distinctive but still valid values, refits on polls_data with one extra poll,
# and returns both the injected cache and the refit-derived state so the tests
# can separately assert "used as input" and "refreshed on output".
run_model8k5_refit_with_custom_cached_warm_start <- function(iter_warmup,
                                                             iter_sampling,
                                                             adapt_engaged) {
  backend_get_sampler_state <- get_internal("backend_get_sampler_state")
  backend_get_last_draws_for_init <- get_internal("backend_get_last_draws_for_init")
  backend_get_num_upars <- get_internal("backend_get_num_upars")

  case <- make_model8_mixed_smoke_case(npolls = 12)
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  polls_df <- as.data.frame(case$polls_data)
  added_poll <- polls_df[1, , drop = FALSE]
  added_poll$.poll_id <- "extra_poll"
  new_polls_data_df <- dplyr::bind_rows(polls_df, added_poll)
  new_polls_data <- polls_data(
    y = new_polls_data_df[, case$parties, drop = FALSE],
    house = factor(new_polls_data_df$.house, levels = levels(polls_df$.house)),
    publish_date = new_polls_data_df$.publish_date,
    start_date = new_polls_data_df$.start_date,
    end_date = new_polls_data_df$.end_date,
    n = as.integer(new_polls_data_df$.n),
    poll_id = new_polls_data_df$.poll_id
  )

  suppressMessages(
    suppressWarnings(
      capture.output(
        pop <- poll_of_polls(
          y = case$parties,
          model = "model8k5",
          polls_data = case$polls_data,
          time_scale = case$time_scale,
          time_scale_overrides = case$time_scale_overrides,
          known_state = case$known_state,
          hyper_parameters = cfg,
          backend = "cmdstanr",
          iter_warmup = 10,
          iter_sampling = 5,
          chains = 1,
          parallel_chains = 1,
          refresh = 0,
          seed = 4711,
          cache_dir = NULL
        )
      )
    )
  )

  original_state <- backend_get_sampler_state("cmdstanr", pop$stan_fit)
  original_init <- backend_get_last_draws_for_init("cmdstanr", pop$stan_fit)

  custom_init <- original_init
  custom_init[[1]]$sigma_x <- original_init[[1]]$sigma_x * 1.05
  custom_sampler_state <- original_state
  custom_sampler_state[[1]]$inv_metric <- original_state[[1]]$inv_metric * 1.05
  custom_sampler_state[[1]]$step_size <- original_state[[1]]$step_size * 0.75
  pop$warm_start_state$init <- custom_init
  pop$warm_start_state$init_complete <- TRUE
  pop$warm_start_state$sampler_state <- custom_sampler_state

  suppressMessages(
    suppressWarnings(
      capture.output(
        refit <- refit_poll_of_polls(
          pop,
          polls_data = new_polls_data,
          iter_warmup = iter_warmup,
          iter_sampling = iter_sampling,
          adapt_engaged = adapt_engaged,
          refresh = 0,
          seed = 4712,
          cache_dir = NULL
        )
      )
    )
  )

  list(
    pop = pop,
    refit = refit,
    new_polls_data = new_polls_data,
    original_state = original_state,
    original_init = original_init,
    custom_init = custom_init,
    custom_sampler_state = custom_sampler_state,
    refit_state = backend_get_sampler_state("cmdstanr", refit$stan_fit),
    refit_init = backend_get_last_draws_for_init("cmdstanr", refit$stan_fit),
    refit_num_upars = backend_get_num_upars("cmdstanr", refit$stan_fit)
  )
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
  expect_identical(args$model_time_range, pop$model_time_range)
  expect_identical(args$latent_time_ranges, pop$input_args$latent_time_ranges)
  expect_identical(args$hyper_parameters, pop$model_arguments)
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

test_that("refit_poll_of_polls has a narrow refit-oriented signature", {
  expect_identical(
    names(formals(refit_poll_of_polls)),
    c("x", "polls_data", "backend", "compile_args", "cache_dir", "warm_start", "...")
  )
  expect_false("y" %in% names(formals(refit_poll_of_polls)))
  expect_false("model" %in% names(formals(refit_poll_of_polls)))
})

test_that("rerun_poll_of_polls delegates to refit_poll_of_polls with warm start disabled", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")

  testthat::local_mocked_bindings(
    refit_poll_of_polls = function(x, ..., warm_start = list()) {
      list(
        x = x,
        dots = list(...),
        warm_start = warm_start
      )
    },
    .package = "stanpop"
  )

  res <- rerun_poll_of_polls(
    pop,
    polls_data = pop$polls_data,
    iter_sampling = 42,
    seed = 99
  )

  expect_identical(res$x, pop)
  expect_identical(res$dots$polls_data, pop$polls_data)
  expect_identical(res$dots$iter_sampling, 42)
  expect_identical(res$dots$seed, 99)
  expect_identical(
    res$warm_start,
    list(init = NULL, inv_metric = NULL, step_size = NULL)
  )
})

test_that("rerun_poll_of_polls rejects explicit warm_start", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")

  expect_error(
    rerun_poll_of_polls(pop, warm_start = list(init = NULL)),
    "does not accept 'warm_start'"
  )
})

test_that("refit_poll_of_polls defaults to cmdstanr and inherits omitted arguments", {
  pop <- make_mock_pop_for_refit_helpers("rstan")

  testthat::local_mocked_bindings(
    backend_get_last_draws_for_init = function(...) {
      list(list(x = c(0.11, 0.22)), list(x = c(0.33, 0.44)))
    },
    backend_get_init_skeleton = function(...) {
      list(list(x = numeric(2)), list(x = numeric(2)))
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
  expect_identical(args$compile_args, pop$compile_arguments)
  expect_identical(args$cache_dir, pop$cache_dir)
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

test_that("refit_poll_of_polls applies explicit polls_data, compile_args, and cache_dir overrides", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")
  new_polls_data <- polls_data(
    y = data.frame(x = c(0.41, 0.46, 0.47)),
    house = factor(c("A", "A", "B")),
    publish_date = as.Date(c("2020-01-02", "2020-01-09", "2020-01-16")),
    start_date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15")),
    end_date = as.Date(c("2020-01-02", "2020-01-09", "2020-01-16")),
    n = c(1000L, 1000L, 900L),
    poll_id = c("p1", "p2", "p3")
  )
  args <- resolve_refit_arguments_for_test(
    pop,
    polls_data = new_polls_data,
    compile_args = NULL,
    cache_dir = NULL,
    warm_start = list(
      init = NULL,
      inv_metric = NULL,
      step_size = NULL
    )
  )

  expect_identical(args$polls_data, new_polls_data)
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

test_that("refit_poll_of_polls prefers cached warm-start state on x", {
  pop <- make_mock_pop_for_refit_helpers("rstan")
  pop$warm_start_state <- list(
    init = list(list(x = c(0.11, 0.22)), list(x = c(0.33, 0.44))),
    init_complete = TRUE,
    sampler_state = list(
      list(step_size = 0.12, inv_metric = c(1, 2), metric_type = "diag_e"),
      list(step_size = 0.34, inv_metric = c(3, 4), metric_type = "diag_e")
    ),
    init_skeleton = list(list(x = numeric(2)), list(x = numeric(2))),
    num_upars = 2L
  )

  testthat::local_mocked_bindings(
    backend_get_last_draws_for_init = function(...) {
      stop("cached init should be used")
    },
    backend_get_sampler_state = function(...) {
      stop("cached sampler state should be used")
    },
    backend_get_num_upars = function(...) {
      stop("cached num_upars should be used")
    },
    backend_get_init_skeleton = function(...) {
      stop("cached init skeleton should be used")
    },
    poll_of_polls = function(...) {
      list(args = list(...))
    },
    .package = "stanpop"
  )

  res <- refit_poll_of_polls(pop)

  expect_identical(
    res$args$init,
    pop$warm_start_state$init
  )
  expect_identical(
    res$args$inv_metric,
    list(c(1, 2), c(3, 4))
  )
  expect_identical(res$args$metric, "diag_e")
  expect_equal(res$args$step_size, c(0.12, 0.34))
})

test_that("refit_poll_of_polls with init_mode last requires matching chain counts", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")

  testthat::local_mocked_bindings(
    backend_get_last_draws_for_init = function(...) {
      list(
        list(x = c(0.11, 0.22)),
        list(x = c(0.33, 0.44))
      )
    },
    backend_get_init_skeleton = function(...) {
      list(
        list(x = numeric(2)),
        list(x = numeric(2))
      )
    },
    .package = "stanpop"
  )

  expect_error(
    refit_poll_of_polls(
      pop,
      chains = 1
    ),
    "init_mode = 'last'\\) requires matching chain counts"
  )
})

test_that("refit_poll_of_polls skips automatic init reuse when cached init is incomplete", {
  pop <- make_mock_pop_for_refit_helpers("rstan")
  pop$warm_start_state <- list(
    init = NULL,
    init_complete = FALSE,
    sampler_state = list(
      list(step_size = 0.12, inv_metric = c(1, 2), metric_type = "diag_e"),
      list(step_size = 0.34, inv_metric = c(3, 4), metric_type = "diag_e")
    ),
    init_skeleton = list(list(x = numeric(2)), list(x = numeric(2))),
    num_upars = 2L
  )

  testthat::local_mocked_bindings(
    backend_get_last_draws_for_init = function(...) {
      stop("incomplete cached init should not trigger backend recovery")
    },
    backend_get_sampler_state = function(...) {
      stop("cached sampler state should be used")
    },
    poll_of_polls = function(...) {
      list(args = list(...))
    },
    .package = "stanpop"
  )

  res <- refit_poll_of_polls(pop)

  expect_false("init" %in% names(res$args))
  expect_identical(
    res$args$inv_metric,
    list(c(1, 2), c(3, 4))
  )
  expect_identical(res$args$metric, "diag_e")
  expect_equal(res$args$step_size, c(0.12, 0.34))
})

test_that("get_num_upars uses cached warm-start num_upars on poll_of_polls objects", {
  pop <- make_mock_pop_for_refit_helpers("rstan")
  pop$warm_start_state <- list(num_upars = 42L)

  testthat::local_mocked_bindings(
    backend_get_num_upars = function(...) {
      stop("cached num_upars should be used")
    },
    .package = "stanpop"
  )

  expect_identical(get_num_upars(pop), 42L)
})

test_that("refit_poll_of_polls can disable init or inverse-metric reuse independently", {
  pop <- make_mock_pop_for_refit_helpers("rstan")
  new_polls_data <- polls_data(
    y = data.frame(x = c(0.41, 0.46, 0.49)),
    house = factor(c("A", "A", "B")),
    publish_date = as.Date(c("2020-01-02", "2020-01-09", "2020-01-16")),
    start_date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15")),
    end_date = as.Date(c("2020-01-02", "2020-01-09", "2020-01-16")),
    n = c(1000L, 1000L, 900L),
    poll_id = c("p1", "p2", "p3")
  )

  expected_case <- "init_mismatch"
  testthat::local_mocked_bindings(
    backend_get_last_draws_for_init = function(...) {
      list(list(x = c(0.11, 0.22)), list(x = c(0.33, 0.44)))
    },
    backend_get_init_skeleton = function(...) {
      list(list(x = numeric(2)), list(x = numeric(2)))
    },
    backend_get_sampler_state = function(...) {
      list(
        list(step_size = 0.12, inv_metric = c(1, 2), metric_type = "diag_e"),
        list(step_size = 0.34, inv_metric = c(3, 4), metric_type = "diag_e")
      )
    },
    backend_get_num_upars = function(...) 2L,
    refit_build_expected_parameter_dimensions = function(...) {
      if(identical(expected_case, "init_mismatch")) {
        return(list(
          num_upars = 2L,
          init_skeleton = list(list(x = numeric(3)))
        ))
      }
      list(
        num_upars = 3L,
        init_skeleton = list(list(x = numeric(2)))
      )
    },
    poll_of_polls = function(...) {
      list(args = list(...))
    },
    .package = "stanpop"
  )

  expect_error(
    refit_poll_of_polls(pop, polls_data = new_polls_data),
    "Warm-start init is incompatible"
  )

  init_disabled <- refit_poll_of_polls(
    pop,
    polls_data = new_polls_data,
    warm_start = list(init = NULL)
  )

  expect_false("init" %in% names(init_disabled$args))
  expect_identical(init_disabled$args$inv_metric, list(c(1, 2), c(3, 4)))
  expect_identical(init_disabled$args$metric, "diag_e")
  expect_equal(init_disabled$args$step_size, c(0.12, 0.34))

  expected_case <- "inv_metric_mismatch"

  expect_error(
    refit_poll_of_polls(pop, polls_data = new_polls_data),
    "expects 3 unconstrained parameter\\(s\\), but chain 1 encodes 2"
  )

  inv_metric_disabled <- refit_poll_of_polls(
    pop,
    polls_data = new_polls_data,
    warm_start = list(inv_metric = NULL)
  )

  expect_true("init" %in% names(inv_metric_disabled$args))
  expect_false("inv_metric" %in% names(inv_metric_disabled$args))
  expect_identical(inv_metric_disabled$args$metric, "diag_e")
  expect_equal(inv_metric_disabled$args$step_size, c(0.12, 0.34))
})

test_that("refit_poll_of_polls can refit an rstan-backed object when get_num_upars fails but sampler state is available", {
  pop <- make_mock_pop_for_refit_helpers("rstan")

  testthat::local_mocked_bindings(
    get_num_upars = function(...) {
      stop("the model object is not created or not valid")
    },
    .package = "rstan"
  )
  testthat::local_mocked_bindings(
    backend_get_sampler_state = function(...) {
      list(
        list(step_size = 0.12, inv_metric = c(1, 2), metric_type = "diag_e"),
        list(step_size = 0.34, inv_metric = c(3, 4), metric_type = "diag_e")
      )
    },
    poll_of_polls = function(...) {
      list(args = list(...))
    },
    .package = "stanpop"
  )

  res <- refit_poll_of_polls(
    pop,
    warm_start = list(init = NULL)
  )

  expect_false("init" %in% names(res$args))
  expect_identical(res$args$inv_metric, list(c(1, 2), c(3, 4)))
  expect_identical(res$args$metric, "diag_e")
  expect_equal(res$args$step_size, c(0.12, 0.34))
})

test_that("refit_poll_of_polls rejects incompatible chains overrides before sampling", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")
  warm_start_init <- list(list(x = c(0.11, 0.22)), list(x = c(0.33, 0.44)))

  expect_error(
    refit_poll_of_polls(
      pop,
      chains = 1,
      warm_start = list(
        init = warm_start_init,
        inv_metric = NULL,
        step_size = NULL
      )
    ),
    "warm-start argument 'init' contains 2 chain\\(s\\)"
  )
})

test_that("refit_poll_of_polls validates inv_metric against changed polls_data before sampling", {
  pop <- make_mock_pop_for_refit_helpers("rstan")
  new_polls_data <- polls_data(
    y = data.frame(x = c(0.41, 0.46, 0.49)),
    house = factor(c("A", "A", "B")),
    publish_date = as.Date(c("2020-01-02", "2020-01-09", "2020-01-16")),
    start_date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15")),
    end_date = as.Date(c("2020-01-02", "2020-01-09", "2020-01-16")),
    n = c(1000L, 1000L, 900L),
    poll_id = c("p1", "p2", "p3")
  )

  testthat::local_mocked_bindings(
    backend_get_sampler_state = function(...) {
      list(
        list(step_size = 0.12, inv_metric = c(1, 2), metric_type = "diag_e"),
        list(step_size = 0.34, inv_metric = c(3, 4), metric_type = "diag_e")
      )
    },
    backend_get_num_upars = function(...) 2L,
    refit_build_expected_parameter_dimensions = function(...) {
      list(
        num_upars = 3L,
        init_skeleton = list(list(x = numeric(3)))
      )
    },
    .package = "stanpop"
  )

  expect_error(
    refit_poll_of_polls(
      pop,
      polls_data = new_polls_data,
      warm_start = list(init = NULL)
    ),
    "changed polls_data altered parameter dimensions"
  )
  expect_error(
    refit_poll_of_polls(
      pop,
      polls_data = new_polls_data,
      warm_start = list(init = NULL)
    ),
    "expects 3 unconstrained parameter\\(s\\), but chain 1 encodes 2"
  )
})

test_that("refit_poll_of_polls gives a clear error when changed polls_data invalidates init reuse", {
  pop <- make_mock_pop_for_refit_helpers("rstan")
  new_polls_data <- polls_data(
    y = data.frame(x = c(0.41, 0.46, 0.49)),
    house = factor(c("A", "A", "B")),
    publish_date = as.Date(c("2020-01-02", "2020-01-09", "2020-01-16")),
    start_date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15")),
    end_date = as.Date(c("2020-01-02", "2020-01-09", "2020-01-16")),
    n = c(1000L, 1000L, 900L),
    poll_id = c("p1", "p2", "p3")
  )

  testthat::local_mocked_bindings(
    backend_get_last_draws_for_init = function(...) {
      list(list(x = c(0.11, 0.22)), list(x = c(0.33, 0.44)))
    },
    backend_get_init_skeleton = function(...) {
      list(list(x = numeric(2)), list(x = numeric(2)))
    },
    backend_get_sampler_state = function(...) {
      list(
        list(step_size = 0.12, inv_metric = c(1, 2), metric_type = "diag_e"),
        list(step_size = 0.34, inv_metric = c(3, 4), metric_type = "diag_e")
      )
    },
    backend_get_num_upars = function(...) 2L,
    refit_build_expected_parameter_dimensions = function(...) {
      list(
        num_upars = 3L,
        init_skeleton = list(list(x = numeric(3)))
      )
    },
    .package = "stanpop"
  )

  expect_error(
    refit_poll_of_polls(pop, polls_data = new_polls_data),
    "Warm-start init is incompatible with the refit model dimensions built from 'polls_data'"
  )
  expect_error(
    refit_poll_of_polls(pop, polls_data = new_polls_data),
    "changed parameter shape\\(s\\): x"
  )
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

test_that("refit_poll_of_polls errors when inherited model inputs are supplied through dots", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")

  expect_error(
    refit_poll_of_polls(pop, y = "other"),
    "Only backend sampler arguments belong in '\\.\\.\\.'"
  )
  expect_error(
    refit_poll_of_polls(
      pop,
      time_scale = "week"
    ),
    "Only backend sampler arguments belong in '\\.\\.\\.'"
  )
  expect_error(
    refit_poll_of_polls(
      pop,
      time_scale_overrides = data.frame(
        from = as.Date("2020-01-10"),
        to = as.Date("2020-01-15"),
        time_scale = "week"
      )
    ),
    "Only backend sampler arguments belong in '\\.\\.\\.'"
  )
  expect_error(
    refit_poll_of_polls(
      pop,
      known_state = tibble::tibble(date = as.Date("2020-01-02"), x = 0.45)
    ),
    "Only backend sampler arguments belong in '\\.\\.\\.'"
  )
  expect_error(
    refit_poll_of_polls(
      pop,
      model_time_range = time_range(c(as.Date("2020-02-01"), as.Date("2020-02-29")))
    ),
    "Only backend sampler arguments belong in '\\.\\.\\.'"
  )
  expect_error(
    refit_poll_of_polls(
      pop,
      latent_time_ranges = list(x = time_range(c(as.Date("2020-02-01"), as.Date("2020-02-29"))))
    ),
    "Only backend sampler arguments belong in '\\.\\.\\.'"
  )
  expect_error(
    refit_poll_of_polls(
      pop,
      hyper_parameters = list(use_softmax = 0L)
    ),
    "Only backend sampler arguments belong in '\\.\\.\\.'"
  )
  expect_error(
    refit_poll_of_polls(
      pop,
      slow_scales = as.Date("2020-01-20")
    ),
    "Only backend sampler arguments belong in '\\.\\.\\.'"
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

test_that("normalize_refit_warm_start accepts supported init_mode values", {
  normalize_refit_warm_start <- get_internal("normalize_refit_warm_start")

  expect_identical(
    normalize_refit_warm_start(list(init_mode = "last")),
    list(init_mode = "last")
  )
  expect_identical(
    normalize_refit_warm_start(list(init_mode = "random")),
    list(init_mode = "random")
  )
})

test_that("normalize_refit_warm_start validates init_mode values", {
  normalize_refit_warm_start <- get_internal("normalize_refit_warm_start")

  expect_error(
    normalize_refit_warm_start(list(init_mode = "foo")),
    "Must be element of set"
  )
})

test_that("refit_poll_of_polls accepts init_mode in warm_start", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")

  expect_silent(
    resolve_refit_arguments_for_test(
      pop,
      warm_start = list(
        init = NULL,
        init_mode = "random",
        inv_metric = NULL,
        step_size = NULL
      )
    )
  )
})

test_that("refit_poll_of_polls errors when automatic init reuse is incomplete", {
  pop <- make_mock_pop_for_refit_helpers("cmdstanr")

  testthat::local_mocked_bindings(
    backend_get_last_draws_for_init = function(...) {
      list(list(x = c(0.11, 0.22)))
    },
    backend_get_init_skeleton = function(...) {
      list(list(x = numeric(2), sigma = 0))
    },
    .package = "stanpop"
  )

  expect_error(
    resolve_refit_arguments_for_test(pop),
    "Automatic init reuse requires a complete last draw"
  )
})

test_that("refit_poll_of_polls with cmdstanr fully warm-starts model8k5 from rstan", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()

  if(!requireNamespace("rstan", quietly = TRUE)) {
    testthat::skip("Package 'rstan' is not available.")
  }

  backend_get_sampler_state <- get_internal("backend_get_sampler_state")
  backend_get_last_draws_for_init <- get_internal("backend_get_last_draws_for_init")
  backend_get_init_skeleton <- get_internal("backend_get_init_skeleton")

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

  original_state <- backend_get_sampler_state("rstan", pop$stan_fit)
  original_init <- backend_get_last_draws_for_init("rstan", pop$stan_fit)
  original_skeleton <- backend_get_init_skeleton("rstan", pop$stan_fit)
  original_skeleton <- lapply(original_skeleton, function(chain_skeleton) {
    chain_skeleton[vapply(chain_skeleton, length, integer(1)) > 0L]
  })

  expect_length(original_init, 1)
  expect_setequal(names(original_init[[1]]), names(original_skeleton[[1]]))
  expect_false(any(vapply(original_init[[1]], function(x) anyNA(x), logical(1))))
  expect_true("sigma_x" %in% names(original_init[[1]]))
  expect_true(all(original_init[[1]]$sigma_x > 0))

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          refit <- refit_poll_of_polls(
            pop,
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

  refit_state <- backend_get_sampler_state("cmdstanr", refit$stan_fit)

  expect_identical(refit$backend, "cmdstanr")
  expect_equal(
    lapply(refit$stan_arguments$init, unlist, use.names = TRUE),
    lapply(original_init, unlist, use.names = TRUE),
    tolerance = 1e-12
  )
  expect_identical(refit$stan_arguments$metric, original_state[[1]]$metric_type)
  expect_equal(refit$stan_arguments$inv_metric[[1]], original_state[[1]]$inv_metric, tolerance = 1e-12)
  expect_equal(as.numeric(refit$stan_arguments$step_size), original_state[[1]]$step_size, tolerance = 1e-12)

  expect_identical(refit_state[[1]]$metric_type, original_state[[1]]$metric_type)
  expect_equal(refit_state[[1]]$inv_metric, original_state[[1]]$inv_metric, tolerance = 1e-12)
  expect_equal(refit_state[[1]]$step_size, original_state[[1]]$step_size, tolerance = 1e-12)
})

test_that("refit_poll_of_polls with cmdstanr can add one poll to model8k5 and refit", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  backend_get_sampler_state <- get_internal("backend_get_sampler_state")
  backend_get_last_draws_for_init <- get_internal("backend_get_last_draws_for_init")

  case <- make_model8_mixed_smoke_case(npolls = 12)
  cfg <- list(
    sigma_kappa_hyper = 0.03,
    use_industry_bias = 1L,
    use_house_bias = 0L,
    use_design_effects = 0L,
    use_multivariate_version = 2L,
    use_softmax = 1L
  )

  polls_df <- as.data.frame(case$polls_data)
  added_poll <- polls_df[1, , drop = FALSE]
  added_poll$.poll_id <- "extra_poll"
  new_polls_data_df <- dplyr::bind_rows(polls_df, added_poll)
  new_polls_data <- polls_data(
    y = new_polls_data_df[, case$parties, drop = FALSE],
    house = factor(new_polls_data_df$.house, levels = levels(polls_df$.house)),
    publish_date = new_polls_data_df$.publish_date,
    start_date = new_polls_data_df$.start_date,
    end_date = new_polls_data_df$.end_date,
    n = as.integer(new_polls_data_df$.n),
    poll_id = new_polls_data_df$.poll_id
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
            iter_warmup = 10,
            iter_sampling = 5,
            chains = 1,
            parallel_chains = 1,
            refresh = 0,
            seed = 4711,
            cache_dir = NULL
          )
        )
      )
    )
  )

  original_state <- backend_get_sampler_state("cmdstanr", pop$stan_fit)
  original_init <- backend_get_last_draws_for_init("cmdstanr", pop$stan_fit)

  expect_silent(
    capture.output(
      suppressWarnings(
        suppressMessages(
          refit <- refit_poll_of_polls(
            pop,
            polls_data = new_polls_data,
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

  refit_state <- backend_get_sampler_state("cmdstanr", refit$stan_fit)

  expect_identical(length(refit$polls_data), length(pop$polls_data) + 1L)
  expect_identical(get_num_upars(refit), get_num_upars(pop))
  expect_equal(
    lapply(refit$stan_arguments$init, unlist, use.names = TRUE),
    lapply(original_init, unlist, use.names = TRUE),
    tolerance = 1e-12
  )
  expect_identical(refit$stan_arguments$metric, original_state[[1]]$metric_type)
  expect_equal(refit$stan_arguments$inv_metric[[1]], original_state[[1]]$inv_metric, tolerance = 1e-12)
  expect_equal(as.numeric(refit$stan_arguments$step_size), original_state[[1]]$step_size, tolerance = 1e-12)
  expect_identical(refit_state[[1]]$metric_type, original_state[[1]]$metric_type)
  expect_equal(refit_state[[1]]$inv_metric, original_state[[1]]$inv_metric, tolerance = 1e-12)
  expect_equal(refit_state[[1]]$step_size, original_state[[1]]$step_size, tolerance = 1e-12)
})

test_that("refit_poll_of_polls with cmdstanr uses and refreshes warm_start_state on model8k5 refits", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  res <- run_model8k5_refit_with_custom_cached_warm_start(
    iter_warmup = 0,
    iter_sampling = 3,
    adapt_engaged = FALSE
  )

  # The actual refit call should use the warm_start_state stored on x.
  expect_equal(
    lapply(res$refit$stan_arguments$init, unlist, use.names = TRUE),
    lapply(res$custom_init, unlist, use.names = TRUE),
    tolerance = 1e-12
  )
  expect_identical(res$refit$stan_arguments$metric, res$custom_sampler_state[[1]]$metric_type)
  expect_equal(res$refit$stan_arguments$inv_metric[[1]], res$custom_sampler_state[[1]]$inv_metric, tolerance = 1e-12)
  expect_equal(as.numeric(res$refit$stan_arguments$step_size), res$custom_sampler_state[[1]]$step_size, tolerance = 1e-12)

  # The returned object should refresh warm_start_state from the new fit rather
  # than keeping the cached values copied from x.
  expect_true(isTRUE(res$refit$warm_start_state$init_complete))
  expect_equal(
    lapply(res$refit$warm_start_state$init, unlist, use.names = TRUE),
    lapply(res$refit_init, unlist, use.names = TRUE),
    tolerance = 1e-12
  )
  expect_identical(res$refit$warm_start_state$sampler_state[[1]]$metric_type, res$refit_state[[1]]$metric_type)
  expect_equal(res$refit$warm_start_state$sampler_state[[1]]$inv_metric, res$refit_state[[1]]$inv_metric, tolerance = 1e-12)
  expect_equal(res$refit$warm_start_state$sampler_state[[1]]$step_size, res$refit_state[[1]]$step_size, tolerance = 1e-12)
  expect_identical(res$refit$warm_start_state$num_upars, res$refit_num_upars)
})

test_that("refit_poll_of_polls with cmdstanr refreshes warm_start_state from adapted model8k5 refits", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  res <- run_model8k5_refit_with_custom_cached_warm_start(
    iter_warmup = 5,
    iter_sampling = 5,
    adapt_engaged = TRUE
  )

  # With warmup enabled, the cached warm_start_state on the returned object
  # should match the adapted sampler state extracted from the new fit.
  expect_true(isTRUE(res$refit$warm_start_state$init_complete))
  expect_identical(
    res$refit$warm_start_state$sampler_state[[1]]$metric_type,
    res$refit_state[[1]]$metric_type
  )
  expect_equal(
    res$refit$warm_start_state$sampler_state[[1]]$inv_metric,
    res$refit_state[[1]]$inv_metric,
    tolerance = 1e-12
  )
  expect_equal(
    res$refit$warm_start_state$sampler_state[[1]]$step_size,
    res$refit_state[[1]]$step_size,
    tolerance = 1e-12
  )
})

test_that("refit_poll_of_polls with cmdstanr does not carry forward cached sampler state when adaptation runs", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  res <- run_model8k5_refit_with_custom_cached_warm_start(
    iter_warmup = 5,
    iter_sampling = 5,
    adapt_engaged = TRUE
  )

  # Because adaptation is enabled, the returned warm_start_state should be
  # refreshed from the new fit rather than reusing the incoming cached sampler
  # state unchanged. The inverse metric can legitimately stay the same, so we
  # compare the full per-chain sampler-state bundle here instead.
  expect_false(isTRUE(all.equal(
    res$refit$warm_start_state$sampler_state[[1]],
    res$custom_sampler_state[[1]],
    tolerance = 1e-12
  )))
})
