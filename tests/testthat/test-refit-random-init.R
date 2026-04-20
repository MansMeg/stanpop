context("refit-random-init")

make_model8k5_cmdstanr_refit_fixture <- function(original_chains = 2L,
                                                 original_iter_warmup = 10L,
                                                 original_iter_sampling = 10L) {
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
          iter_warmup = original_iter_warmup,
          iter_sampling = original_iter_sampling,
          chains = original_chains,
          parallel_chains = 1,
          refresh = 0,
          seed = 4711,
          cache_dir = NULL
        )
      )
    )
  )

  list(
    pop = pop,
    new_polls_data = new_polls_data
  )
}

run_model8k5_random_init_refit <- function(x, polls_data, chains, seed) {
  suppressMessages(
    suppressWarnings(
      capture.output(
        refit_poll_of_polls(
          x,
          polls_data = polls_data,
          chains = chains,
          parallel_chains = 1,
          iter_warmup = 0,
          iter_sampling = 3,
          adapt_engaged = FALSE,
          refresh = 0,
          seed = seed,
          cache_dir = NULL,
          warm_start = list(init_mode = "random")
        )
      )
    )
  )
}

expect_refit_random_warm_start_matches <- function(refit,
                                                   expected_random,
                                                   original_state) {
  expected_state <- original_state[expected_random$source_chain_ids]
  expected_metric <- unique(vapply(expected_state, function(x) x$metric_type, character(1)))

  expect_length(refit$stan_arguments$init, length(expected_random$init))
  expect_equal(
    lapply(refit$stan_arguments$init, unlist, use.names = TRUE),
    lapply(expected_random$init, unlist, use.names = TRUE),
    tolerance = 1e-12
  )
  expect_equal(
    refit$stan_arguments$inv_metric,
    lapply(expected_state, function(x) x$inv_metric),
    tolerance = 1e-12
  )
  expect_length(expected_metric, 1)
  expect_identical(refit$stan_arguments$metric, expected_metric[[1]])
  expect_equal(
    as.numeric(refit$stan_arguments$step_size),
    vapply(expected_state, function(x) x$step_size, numeric(1)),
    tolerance = 1e-12
  )
}

find_random_init_selection_for_seed_candidates <- function(pop,
                                                           chains,
                                                           seed_candidates,
                                                           predicate = function(...) TRUE) {
  backend_get_random_draws_for_init <- get_internal("backend_get_random_draws_for_init")

  for(seed in as.integer(seed_candidates)) {
    selection <- backend_get_random_draws_for_init(
      "cmdstanr",
      pop$stan_fit,
      chains = chains,
      seed = seed
    )
    if(isTRUE(predicate(selection, seed))) {
      return(list(seed = seed, selection = selection))
    }
  }

  stop("Could not find a suitable random init selection for the integration test.")
}

test_that("refit_poll_of_polls with cmdstanr can change chains and use init_mode random", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  backend_get_sampler_state <- get_internal("backend_get_sampler_state")
  backend_get_random_draws_for_init <- get_internal("backend_get_random_draws_for_init")

  fixture <- make_model8k5_cmdstanr_refit_fixture(
    original_chains = 2L,
    original_iter_warmup = 10L,
    original_iter_sampling = 10L
  )
  original_state <- backend_get_sampler_state("cmdstanr", fixture$pop$stan_fit)
  expected_random <- backend_get_random_draws_for_init(
    "cmdstanr",
    fixture$pop$stan_fit,
    chains = 3L,
    seed = 5712L
  )

  refit <- run_model8k5_random_init_refit(
    x = fixture$pop,
    polls_data = fixture$new_polls_data,
    chains = 3L,
    seed = 5712L
  )

  expect_identical(refit$backend, "cmdstanr")
  expect_identical(get_num_upars(refit), get_num_upars(fixture$pop))
  expect_refit_random_warm_start_matches(
    refit = refit,
    expected_random = expected_random,
    original_state = original_state
  )
})

test_that("refit_poll_of_polls with cmdstanr can use init_mode random when chains match", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  backend_get_last_draws_for_init <- get_internal("backend_get_last_draws_for_init")
  backend_get_sampler_state <- get_internal("backend_get_sampler_state")

  fixture <- make_model8k5_cmdstanr_refit_fixture(
    original_chains = 2L,
    original_iter_warmup = 10L,
    original_iter_sampling = 10L
  )
  original_state <- backend_get_sampler_state("cmdstanr", fixture$pop$stan_fit)
  last_draws <- backend_get_last_draws_for_init("cmdstanr", fixture$pop$stan_fit)
  selected <- find_random_init_selection_for_seed_candidates(
    pop = fixture$pop,
    chains = 2L,
    seed_candidates = 5801:5900,
    predicate = function(selection, seed) {
      !isTRUE(all.equal(
        lapply(selection$init, unlist, use.names = TRUE),
        lapply(last_draws, unlist, use.names = TRUE),
        tolerance = 1e-12
      ))
    }
  )

  refit <- run_model8k5_random_init_refit(
    x = fixture$pop,
    polls_data = fixture$new_polls_data,
    chains = 2L,
    seed = selected$seed
  )

  expect_refit_random_warm_start_matches(
    refit = refit,
    expected_random = selected$selection,
    original_state = original_state
  )
})

test_that("refit_poll_of_polls with cmdstanr uses seed-reproducible init_mode random selection", {
  skip_if_no_stan_tests()
  skip_if_no_cmdstanr_tests()
  skip_if_no_cmdstanr()

  backend_get_sampler_state <- get_internal("backend_get_sampler_state")

  fixture <- make_model8k5_cmdstanr_refit_fixture(
    original_chains = 2L,
    original_iter_warmup = 10L,
    original_iter_sampling = 10L
  )
  original_state <- backend_get_sampler_state("cmdstanr", fixture$pop$stan_fit)
  first_selected <- find_random_init_selection_for_seed_candidates(
    pop = fixture$pop,
    chains = 3L,
    seed_candidates = 6001:6100
  )
  second_selected <- find_random_init_selection_for_seed_candidates(
    pop = fixture$pop,
    chains = 3L,
    seed_candidates = seq.int(first_selected$seed + 1L, 6200L),
    predicate = function(selection, seed) {
      !identical(
        lapply(selection$init, unlist, use.names = TRUE),
        lapply(first_selected$selection$init, unlist, use.names = TRUE)
      )
    }
  )

  refit_a <- run_model8k5_random_init_refit(
    x = fixture$pop,
    polls_data = fixture$new_polls_data,
    chains = 3L,
    seed = first_selected$seed
  )
  refit_b <- run_model8k5_random_init_refit(
    x = fixture$pop,
    polls_data = fixture$new_polls_data,
    chains = 3L,
    seed = first_selected$seed
  )
  refit_c <- run_model8k5_random_init_refit(
    x = fixture$pop,
    polls_data = fixture$new_polls_data,
    chains = 3L,
    seed = second_selected$seed
  )

  expect_refit_random_warm_start_matches(
    refit = refit_a,
    expected_random = first_selected$selection,
    original_state = original_state
  )
  expect_refit_random_warm_start_matches(
    refit = refit_b,
    expected_random = first_selected$selection,
    original_state = original_state
  )
  expect_refit_random_warm_start_matches(
    refit = refit_c,
    expected_random = second_selected$selection,
    original_state = original_state
  )
  expect_equal(
    lapply(refit_a$stan_arguments$init, unlist, use.names = TRUE),
    lapply(refit_b$stan_arguments$init, unlist, use.names = TRUE),
    tolerance = 1e-12
  )
  expect_identical(refit_a$stan_arguments$metric, refit_b$stan_arguments$metric)
  expect_equal(refit_a$stan_arguments$inv_metric, refit_b$stan_arguments$inv_metric, tolerance = 1e-12)
  expect_equal(as.numeric(refit_a$stan_arguments$step_size), as.numeric(refit_b$stan_arguments$step_size), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(
    lapply(refit_a$stan_arguments$init, unlist, use.names = TRUE),
    lapply(refit_c$stan_arguments$init, unlist, use.names = TRUE),
    tolerance = 1e-12
  )))
})
