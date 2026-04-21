context("plot")

test_that("plot works for saved poll_of_polls fixture", {
  skip_if_no_cmdstanr()

  pop <- load_pop(testthat::test_path("files", "test_pop_8m5_cmdstanr.rds"))

  expect_s3_class(pop, "poll_of_polls")
  expect_silent(
    plt <- suppressWarnings(
      plot(pop, pop$y[1], include_latent_state = TRUE)
    )
  )
  expect_s3_class(plt, "ggplot")
  expect_silent(suppressWarnings(ggplot2::ggplot_build(plt)))
  skip("TODO: Test that there is a warning if not the whole latent state is plotted that also propose how to change time_range to show the whole LS")
})
