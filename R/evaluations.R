#' Return the latest evaluation window for a poll_of_polls object
#'
#' @description
#' Return the model end date together with the latest known-state date that can
#' be used for evaluation.
#'
#' @details
#' This helper exists to extract definition of the "last evaluation window"
#' instead of recomputing it ad hoc. In practice, reports often need to know:
#'
#' 1. when the fitted latent series ends,
#' 2. what the most recent usable known-state date is, and
#' 3. how many days separate those two dates.
#'
#' @param pop a [poll_of_polls] object
#'
#' @return A one-row tibble with `model_time_to`, `evaluation_date`, and
#'   `gap_days`. When there is no known state on or before the model end date,
#'   `evaluation_date` and `gap_days` are returned as missing values.
#' @export
get_last_evaluation_info <- function(pop) {
  model_time_to <- evaluation_model_time_to(pop)
  known_dates <- evaluation_known_state_dates(pop)
  evaluation_date <- as.Date(NA)
  gap_days <- NA_integer_

  if(length(known_dates) > 0L) {
    evaluation_date <- max(known_dates)
    gap_days <- as.integer(model_time_to - evaluation_date)
  }

  tibble::tibble(
    model_time_to = model_time_to,
    evaluation_date = evaluation_date,
    gap_days = gap_days
  )
}

#' Return known-state evaluation periods for a poll_of_polls object
#'
#' @description
#' Return one general evaluation period per usable known-state date.
#'
#' @details
#' This function extract stable period definition. A usable evaluation date is
#' a non-missing known-state date on or before the model end date.
#' Returned dates are sorted and deduplicated before periods are constructed.
#'
#' The period semantics are intentionally conservative:
#'
#' 1. `period_to` is the current `evaluation_date`,
#' 2. `period_from` is the previous `evaluation_date`, and
#' 3. the first period gets `NA` for `previous_evaluation_date` and
#'    `period_from`.
#'
#'
#' @param pop a [poll_of_polls] object
#'
#' @return A tibble with one row per usable known-state date and the columns
#'   `period_index`, `evaluation_date`, `previous_evaluation_date`,
#'   `period_from`, `period_to`, and `is_last`.
#' @export
get_known_state_periods <- function(pop) {
  checkmate::assert_class(pop, "poll_of_polls")
  known_dates <- evaluation_known_state_dates(pop)

  if(length(known_dates) < 1L) {
    return(tibble::tibble(
      period_index = integer(),
      evaluation_date = as.Date(character()),
      previous_evaluation_date = as.Date(character()),
      period_from = as.Date(character()),
      period_to = as.Date(character()),
      is_last = logical()
    ))
  }

  previous_evaluation_date <- c(as.Date(NA), known_dates[-length(known_dates)])

  tibble::tibble(
    period_index = seq_along(known_dates),
    evaluation_date = known_dates,
    previous_evaluation_date = previous_evaluation_date,
    period_from = previous_evaluation_date,
    period_to = known_dates,
    is_last = seq_along(known_dates) == length(known_dates)
  )
}

#' Return known-state lookback windows for a poll_of_polls object
#'
#' @description
#' Return one lookback window per usable known-state evaluation date.
#'
#' @details
#' This helper builds directly on [get_known_state_periods()] so lookback
#' windows use the same stable set of usable evaluation dates. The `window`
#' argument must be a scalar lubridate [Period], for example
#' `lubridate::period(months = 6)` or `lubridate::days(180)`.
#'
#' Lookback window semantics are:
#'
#' 1. `window_to` is the current `evaluation_date`,
#' 2. `window_from` is `evaluation_date` shifted backward by `window`, and
#' 3. the returned rows retain the matching period metadata.
#'
#' Calendar-aware month arithmetic is computed with `lubridate::\%m-\%` so
#' month-based lookback windows behave consistently around month ends.
#'
#' @param pop a [poll_of_polls] object
#' @param window a scalar lubridate [Period] defining how far back each
#'   lookback window should start.
#'
#' @return A tibble with one row per usable known-state date and the columns
#'   `window_index`, `evaluation_date`, `window_from`, `window_to`,
#'   `period_index`, `previous_evaluation_date`, `period_from`, `period_to`,
#'   and `is_last`.
#' @export
get_known_state_lookback_windows <- function(pop, window = lubridate::period(months = 6)) {
  checkmate::assert_class(pop, "poll_of_polls")
  assert_known_state_lookback_window(window)

  periods <- get_known_state_periods(pop)

  if(nrow(periods) < 1L) {
    return(tibble::tibble(
      window_index = integer(),
      evaluation_date = as.Date(character()),
      window_from = as.Date(character()),
      window_to = as.Date(character()),
      period_index = integer(),
      previous_evaluation_date = as.Date(character()),
      period_from = as.Date(character()),
      period_to = as.Date(character()),
      is_last = logical()
    ))
  }

  tibble::tibble(
    window_index = periods$period_index,
    evaluation_date = periods$evaluation_date,
    window_from = lubridate::`%m-%`(periods$evaluation_date, window),
    window_to = periods$evaluation_date,
    period_index = periods$period_index,
    previous_evaluation_date = periods$previous_evaluation_date,
    period_from = periods$period_from,
    period_to = periods$period_to,
    is_last = periods$is_last
  )
}

#' @keywords internal
evaluation_model_time_to <- function(pop) {
  checkmate::assert_class(pop, "poll_of_polls")
  unname(time_range(pop$time_line)["to"])
}

#' @keywords internal
evaluation_known_state_dates <- function(pop) {
  checkmate::assert_class(pop, "poll_of_polls")
  model_time_to <- evaluation_model_time_to(pop)

  if(is.null(pop$known_state) || nrow(pop$known_state) < 1L) {
    return(as.Date(character()))
  }

  checkmate::assert_names(names(pop$known_state), must.include = "date")

  known_dates <- pop$known_state$date
  known_dates <- known_dates[!is.na(known_dates) & known_dates <= model_time_to]
  sort(unique(known_dates))
}

#' Compute the elpd, percentiles and rmse for a true [known_state]
#'
#' @details
#' Incorrect draws has been removed
#'
#' @param x a [poll_of_polls] object
#' @param known_state a [known_state] object
#' @export
elpd_known_state <- function(x, known_state){
  checkmate::assert_class(x, "poll_of_polls")
  assert_known_state(known_state, null.ok = FALSE)
  if(!is.null(known_state)){
    checkmate::assert_names(x = names(known_state), must.include = c("date", x$y))
  }
  UseMethod("elpd_known_state")
}


#' @rdname elpd_known_state
#' @export
elpd_known_state.pop_model8f1 <- function(x, known_state){

  results <- matrix(0.0, nrow = length(known_state$date), ncol = length(x$y) + 1L,
                    dimnames = list(as.character(known_state$date), c(x$y, "ndraws")))

  known_state$t <- get_time_points(x, known_state$date)

  ls <- latent_state(x)
  id <- incorrect_draws(x)
  ls <- ls[!id,,]

  sigma_x <- extract(x, "sigma_x")$sigma_x[!id,]
  colnames(sigma_x) <- x$y
  sigma_xc <- extract(x, "sigma_xc")$sigma_xc
  if(!is.null(sigma_xc)) {
    colnames(sigma_xc) <- x$y
    sigma_xc <- sigma_xc[!id,]
  }

  for(i in seq_along(known_state$date)){
    for (j in seq_along(x$y)){
      if(x$stan_data$stan_data$use_latent_state_version == 0L){
        sigma <- sigma_x[,x$y[j]]
      } else if(x$stan_data$stan_data$use_latent_state_version == 3L){
        x_t_minus_1 <- ls$latent_state[,known_state$t[i] - 1, x$y[j]]

        sigma <- sqrt(x_t_minus_1 * (1 - x_t_minus_1)) * sigma_x[,x$y[j]] + sigma_xc[,x$y[j]]
      } else {
        stop("model not implemented for 'use_latent_state_version' = ",
             x$stan_data$stan_data$use_latent_state_version)
      }
      results[i, j] <- logMeanExp(stats::dnorm(known_state[[x$y[j]]][i], mean = ls$latent_state[,known_state$t[i],x$y[j]], sd = sigma, log = TRUE))
    }
    results[i, "ndraws"] <- sum(!id)
  }
  results
}

#' @rdname elpd_known_state
#' @export
elpd_known_state.pop_model8f2 <- elpd_known_state.pop_model8f1


#' @rdname elpd_known_state
#' @export
rmse_known_state <- function(x, known_state){
  checkmate::assert_class(x, "poll_of_polls")
  assert_known_state(known_state, null.ok = FALSE)
  if(!is.null(known_state)){
    checkmate::assert_names(x = names(known_state), must.include = c("date", x$y))
  }

  results <- matrix(0.0, nrow = length(known_state$date), ncol = length(x$y) + 1L,
                    dimnames = list(as.character(known_state$date), c(x$y, "ndraws")))
  known_state$t <- get_time_points(x, known_state$date)
  ls <- latent_state(x)
  id <- incorrect_draws(x)
  ls <- ls[!id,,]

  for(i in seq_along(known_state$date)){
    for (j in seq_along(x$y)){
      results[i, x$y[j]] <- sqrt(mean((known_state[[x$y[j]]][i] - ls$latent_state[,known_state$t[i], x$y[j]])^2))
    }
    results[i, "ndraws"] <- sum(!id)
  }
  results
}

#' @rdname elpd_known_state
#' @export
percentile_known_state <- function(x, known_state){
  checkmate::assert_class(x, "poll_of_polls")
  assert_known_state(known_state, null.ok = FALSE)
  if(!is.null(known_state)){
    checkmate::assert_names(x = names(known_state), must.include = c("date", x$y))
  }

  results <- matrix(0.0, nrow = length(known_state$date), ncol = length(x$y) + 1L,
                    dimnames = list(as.character(known_state$date), c(x$y, "ndraws")))

  known_state$t <- get_time_points(x, known_state$date)
  ls <- latent_state(x)
  id <- incorrect_draws(x)
  ls <- ls[!id,,]

  for(i in seq_along(known_state$date)){
    for (j in seq_along(x$y)){
      results[i, j] <- mean(known_state[[x$y[j]]][i] > ls$latent_state[,known_state$t[i], x$y[j]])
    }
    results[i, "ndraws"] <- sum(!id)
  }

  results
}

#' What samples are correct?
#'
#' @param x a [poll_of_polls] object
#' @export
incorrect_draws <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  ls <- latent_state(x)
  incorrect_x <- apply(ls$latent_state < 0 | ls$latent_state > 1, 1, any)
  incorrect_x
}
