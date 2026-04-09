context("refit helpers")

make_mock_pop_for_refit_helpers <- function(){
  pd <- polls_data(
    y = data.frame(x = c(0.4, 0.45)),
    house = factor(c("A", "A")),
    publish_date = as.Date(c("2020-01-02", "2020-01-09")),
    start_date = as.Date(c("2020-01-01", "2020-01-08")),
    end_date = as.Date(c("2020-01-02", "2020-01-09")),
    n = c(1000L, 1000L),
    poll_id = c("p1", "p2")
  )

  structure(
    list(
      y = "x",
      model = "model8k5",
      backend = "cmdstanr",
      compile_arguments = list(stanc_options = list("O1")),
      polls_data = pd,
      time_scale = "day",
      time_scale_overrides = NULL,
      known_state = tibble::tibble(date = as.Date("2020-01-01"), x = 0.41),
      model_time_range = time_range(c(as.Date("2020-01-01"), as.Date("2020-01-31"))),
      latent_time_range = list(x = time_range(c(as.Date("2020-01-01"), as.Date("2020-01-31")))),
      stan_arguments = list(
        chains = 2,
        iter_warmup = 250,
        iter_sampling = 500,
        seed = 4711
      ),
      model_arguments = list(use_softmax = 1L),
      cache_dir = NULL,
      input_args = list(
        y = "x",
        model = "model8k5",
        backend = "cmdstanr",
        compile_args = list(stanc_options = list("O1")),
        time_scale = "day",
        time_scale_overrides = NULL,
        model_time_range = NULL,
        latent_time_ranges = NULL,
        hyper_parameters = list(use_softmax = 1L),
        slow_scales = as.Date("2020-01-15"),
        stan_arguments = list(
          chains = 2,
          iter_warmup = 250,
          iter_sampling = 500,
          seed = 4711
        ),
        cache_dir = NULL
      )
    ),
    class = c("pop_model8k5", "poll_of_polls")
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
