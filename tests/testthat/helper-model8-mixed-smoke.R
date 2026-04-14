make_model8_mixed_smoke_case <- function(npolls = 40) {
  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(
    date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1)
  )
  known_state <- cbind(known_state, txdf[true_idx, ])

  polls_data <- simulate_polls(
    x = txdf,
    pd = pd_test,
    npolls = npolls,
    time_scale = time_scale,
    start_date = "2010-01-01"
  )

  list(
    polls_data = polls_data,
    known_state = known_state,
    parties = parties,
    time_scale = time_scale,
    time_scale_overrides = tibble::tibble(
      from = as.Date("2010-05-05"),
      to = as.Date("2010-05-10"),
      time_scale = "day"
    )
  )
}
