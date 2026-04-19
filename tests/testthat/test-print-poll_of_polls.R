context("poll_of_polls print compaction")

test_that("scalar init values stay unchanged in printed stan arguments", {
  compact_stan_arguments_for_print <- get_internal("compact_stan_arguments_for_print")

  args <- list(
    init = "random",
    chains = 4L,
    seed = 4711
  )

  expect_identical(compact_stan_arguments_for_print(args), args)
})

test_that("materialized per-chain init values are compacted for printing", {
  compact_stan_arguments_for_print <- get_internal("compact_stan_arguments_for_print")

  args <- list(
    chains = 2L,
    init = list(
      list(alpha = c(111.123, 222.456), beta = 333.789),
      list(alpha = c(444.987, 555.654), beta = 666.321)
    ),
    seed = 4711
  )

  compact <- compact_stan_arguments_for_print(args)
  yaml_lines <- capture.output(cat(yaml::as.yaml(compact)))

  expect_identical(compact$chains, 2L)
  expect_identical(compact$seed, 4711)
  expect_identical(compact$init, "materialized init values omitted")
  expect_identical(compact$init_type, "materialized_per_chain_list")
  expect_identical(compact$init_chains, 2L)
  expect_identical(compact$init_parameter_roots, c("alpha", "beta"))
  expect_false(any(grepl("111\\.123|222\\.456|333\\.789|444\\.987|555\\.654|666\\.321", yaml_lines)))
})

test_that("materialized named init lists are compacted for printing", {
  compact_stan_arguments_for_print <- get_internal("compact_stan_arguments_for_print")

  args <- list(
    init = list(alpha = c(111.123, 222.456), beta = 333.789),
    seed = 4711
  )

  compact <- compact_stan_arguments_for_print(args)

  expect_identical(compact$init, "materialized init values omitted")
  expect_identical(compact$init_type, "materialized_named_list")
  expect_identical(compact$init_parameter_roots, c("alpha", "beta"))
})


test_that("materialized named init lists are compacted for printing", {
  compact_stan_arguments_for_print <- get_internal("compact_stan_arguments_for_print")

  args <- list(
    init = list(alpha = c(111.123, 222.456),
                beta = 333.789,
                gamma = 122,
                delta = 19200,
                omega = 2231,
                sigma = 813,
                theta = 9319,
                psi = 7177,
                epsilon = 2717),
    seed = 4711
  )

  compact <- compact_stan_arguments_for_print(args)
  yaml_lines <- capture.output(cat(yaml::as.yaml(compact)))

  expect_identical(compact$init, "materialized init values omitted")
  expect_identical(compact$init_type, "materialized_named_list")
  expect_true(any(grepl("... ", yaml_lines)))
})
