# Run the line below to run different test suites locally
# See documentation for details.
# stanpop:::set_test_stan_basic_on_local(TRUE)
# stanpop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
}

test_that("Test model 8k2 full model", {

  # The input is Swedish Data 2002--2026-02
  # The model used here is the poll of polls model at BottenAda.se in feb 2026
  fp <- testthat::test_path("files/pop_args_full_swe_8k2.rds")
  pop_args <- readRDS(fp)


  skip_if_no_stan_tests()
  expect_silent(pop8k2_out <-
                  capture.output(
                    suppressWarnings(
                      suppressMessages(
                        pop8k2_1 <- poll_of_polls(y = pop_args$y,
                                                  model = pop_args$model,
                                                  model_time_range = pop_args$model_time_range,
                                                  polls_data = pop_args$polls_data,
                                                  time_scale = pop_args$time_scale,
                                                  known_state = pop_args$known_state,
                                                  slow_scales = pop_args$slow_scales,
                                                  hyper_parameters = pop_args$hyper_parameters,
                                                  warmup = 0,
                                                  iter = 1,
                                                  chains = 1,
                                                  cache_dir = NULL)
                      )
                    )
                  )
  )
  expect_silent(pn1 <- unique(parameter_names(pop8k2_1, TRUE)))
})

