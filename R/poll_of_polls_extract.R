#' Extract mean x change from a poll_of_polls object
#'
#' @details
#' Extracts the difference in the latent state between time points,
#' either as the [diff]erence x[t]-x[t-1] or the [ratio] x[t]/x[t-1].
#' Here t=1 is set to NA.
#'
#' @param pop a poll of polls object
#' @param type type of
#'
#' @export
extract_pop_empirical_posterior_mean_x_change <- function(pop, type){
  checkmate::assert_class(pop, "poll_of_polls")
  checkmate::assert_choice(type, choices = c("diff", "ratio"))

  x <- extract(pop, pars = "x_pred")$x_pred
  dims <- dim(x)
  n <- 1
  if(type == "diff"){
    mat <- x[,(1+n):(dims[2]),] - x[,1:(dims[2]-n),]
  } else if(type == "ratio"){
    mat <- x[,(1+n):(dims[2]),] / x[,1:(dims[2]-n),]
  }

  df <- pop$time_line$time_line
  for(i in seq_along(pop$y)){
    vec <- c(NA, colMeans(mat[,,i]))
    df[[pop$y[i]]] <- vec
  }
  df
}


#' Extract election periods from a poll_of_polls object
#'
#' @details
#' Extracts a tibble with the election period as a boolean per time point
#'
#' @param pop a poll of polls object
#' @param election_period_length the number of time points that make up the lection period
#'
#' @export
extract_pop_election_period <- function(pop, election_period_length = 6){
  checkmate::assert_class(pop, "poll_of_polls")
  checkmate::assert_int(election_period_length)

  el <- pop$time_line$daily[pop$time_line$daily$date %in% pop$known_state$date,]
  tps <- el$time_line_t
  tpsl <- list()
  for(i in 1:(election_period_length-1)){
    tpsl[[i]] <- tps - i
  }
  tps <- c(tps, unlist(tpsl))
  tps <- tps[order(tps)]

  df <- pop$time_line$time_line
  df$election_period <- "normal"
  df$election_period[tps] <- "election"
  df$election_period <- as.factor(df$election_period)
  df
}
