#' Reweight and resample a polls_data object
#'
#' @description
#' The functions reweights and resamples a data_polls object.
#' This is used to simulated polls with different [time_weights].
#'
#' @param x a [polls_data] object.
#' @param true_ls a vector with the true latent state
#' @param time_scale The [time_scale] used for the latent state.
#' @param weight The way the collection period is weighted. See code for details.
#'
reweight_and_resample <- function(x, true_ls, time_scale, weight = "none"){
  checkmate::assert_class(x, "polls_data")
  tl <- time_line(x$time_range, time_scale)
  checkmate::assert_numeric(true_ls, len = nrow(tl$time_line))
  checkmate::assert_choice(weight, choices = c("none", "keep", "skew", "vskew"))

  if(weight == "none") return(x)

  is_long_poll_ids <- poll_ids(x)[as.integer(end_dates(x) - start_dates(x) + 1L) > 14]
  ptw <- polls_time_weights(x)
  if(weight == "skew") {
    polls_time_weights(x) <- polls_time_reweight(ptw, poll_ids = is_long_poll_ids, stats::dnbinom, size = 3, mu = 7)
  }
  if(weight == "vskew") {
    polls_time_weights(x) <- polls_time_reweight(ptw, poll_ids = is_long_poll_ids, stats::dnbinom, size = 1, mu = 4)
  }
  ydat <- sample_polls_y(x, as.data.frame(x = true_ls), tl, week_start = 1L)
  y(x) <- ydat
  x
}
