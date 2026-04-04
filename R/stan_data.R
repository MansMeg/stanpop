#' Generate Stan Data from [polls_data] objects
#'
#' @param x a [polls_data] object
#' @param time_scale to use.
#' @param time_scale_overrides a [data.frame] with columns [from], [to], and [time_scale]
#'   that override the default [time_scale] for inclusive date ranges in the latent state.
#'   The [from] and [to] bounds are inclusive and override ranges must not overlap.
#' @param y_name a character vector indicating y variables in polls object.
#' @param model model to get data for.
#' @param known_state known time points in the latent state
#' @param model_time_range the time range to model (for example for extrapolation)
#' @param latent_time_ranges the time range of the latent state
#' @param hyper_parameters a list with hyper parameters supplied to the model.
#' @param slow_scales a vector of [Date]s that indicate breaks (right-inclusive) for a slower moving time scale.
#'        Example: If only 2010-01-15 is used, all dates up to and including 2010-01-15, will have s=1,
#'                 Dates after 2010-01-15 will have s=2.
#' @details
#' The returned object keeps the existing [stan_data] and [time_line] fields
#' unchanged for current models. A parallel future path is attached in
#' [stan_data_with_overrides] and [time_line_with_overrides], built using
#' [time_line_with_overrides()].
#' @export
stan_polls_data <- function(x,
                            y_name,
                            model,
                            time_scale = "week",
                            time_scale_overrides = NULL,
                            known_state = NULL,
                            model_time_range = NULL,
                            latent_time_ranges = NULL,
                            hyper_parameters = NULL,
                            slow_scales = NULL){
  checkmate::assert_class(x, "polls_data")
  checkmate::assert_subset(y_name, choices = names(y(x)))
  checkmate::assert_choice(time_scale, supported_time_scales())
  checkmate::assert_choice(model, choices = supported_pop_models())
  assert_known_state(known_state)
  assert_latent_time_ranges(latent_time_ranges)
  assert_slow_scales(slow_scales, null.ok = TRUE)
  if(is.null(model_time_range)) {
    mtr <- time_range(x)
  } else {
    mtr <- time_range(model_time_range)
  }
  assert_time_scale_overrides(
    x = time_scale_overrides,
    dates = tibble::tibble(date = seq(from = mtr["from"], to = mtr["to"], by = 1))
  )
  spd <- structure(list(), class = c(model,"stan_polls_data"))
  if(model %in% c("model2","model3","model4","model5","model6","model6b", "model6c","model7","model9")){
    spd <- stan_polls_data_model(spd = spd,
                                 psd = x,
                                 y_name = y_name,
                                 time_scale = time_scale,
                                 known_state = known_state,
                                 latent_time_ranges = latent_time_ranges,
                                 model_time_range = model_time_range)
  } else if(model %in% c("model8a")) {
    spd <- stan_polls_data_model8a(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters)
  } else if(model %in% c("model8a3", "model8a4")) {
    spd <- stan_polls_data_model8a3(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters)
  } else if(model %in% c("model8a1")) {
    spd <- stan_polls_data_model8a1(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters)
  } else if(model %in% c("model8b", "model8b1")) {
    spd <- stan_polls_data_model8b(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales)
  } else if(model %in% c("model8c", "model8c2")) {
    spd <- stan_polls_data_model8c(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales)
  } else if(substr(model,1,8) %in% c("model10d")) {
    spd <- stan_polls_data_model10d(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales,'model8d3')
  } else if(substr(model,1,8) %in% c("model10e")) {
    spd <- stan_polls_data_model10e(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales,'model8d3')
  } else if(substr(model,1,7) %in% c("model8d")) {
    spd <- stan_polls_data_model8d(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales, model)
  } else if(substr(model,1,8) %in% c("model11a")) {
    spd <- stan_polls_data_model11a(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales,'model8d3')
  } else if(substr(model,1,8) %in% c("model11b")) {
    spd <- stan_polls_data_model11b(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales,'model8e')
  } else if(substr(model,1,7) %in% c("model8e")) {
    spd <- stan_polls_data_model8e(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales, model)
  } else if(substr(model,1,7) %in% c("model8f")) {
    spd <- stan_polls_data_model8f(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales, model)
  } else if(substr(model,1,7) %in% c("model8g","model8h")) {
    spd <- stan_polls_data_model8g(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales, model)
  } else if(grepl(model, pattern = "^model8i[0-9]+$")) {
    spd <- stan_polls_data_model8i(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales, model)
  } else if(grepl(model, pattern = "^model8j[0-9]+$")) {
    spd <- stan_polls_data_model8i(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales, model)
  } else if(grepl(model, pattern = "^model8k[0-9]+$")) {
    spd <- stan_polls_data_model8k(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales, model)
  } else if(grepl(model, pattern = "^model8l[0-9]+$")) {
    spd <- stan_polls_data_model8l(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales, model)
  } else if(grepl(model, pattern = "^model8m[0-9]+$")) {
    spd <- stan_polls_data_model8m(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters, slow_scales, model)
  } else {
    stop("'", model, "' not implemented in stan_polls_data().")
  }

  spd <- attach_stan_data_with_overrides(
    spd = spd,
    x = x,
    y_name = y_name,
    model = model,
    time_scale = time_scale,
    time_scale_overrides = time_scale_overrides,
    known_state = known_state,
    model_time_range = mtr,
    latent_time_ranges = latent_time_ranges,
    hyper_parameters = hyper_parameters,
    slow_scales = slow_scales
  )

  if(use_override_aware_stan_data_by_default(model)){
    spd$stan_data <- spd$stan_data_with_overrides
    spd$time_line <- spd$time_line_with_overrides
  }

  assert_stan_polls_data(x = spd)
  assert_stan_data_model(x = spd)
  spd
}

use_override_aware_stan_data_by_default <- function(model) {
  checkmate::assert_string(model)
  model_supports_time_scale_overrides(model)
}

model_supports_time_scale_overrides <- function(model) {
  checkmate::assert_string(model)
  grepl(pattern = "^model8[km]5$", x = model)
}


#' @rdname stan_polls_data
#' @export
stan_polls_data_model2 <- function(x, y_name, time_scale = "week", model_time_range = NULL){
  checkmate::assert_class(x, "polls_data")
  checkmate::assert_choice(y_name, choices = names(y(x)))
  checkmate::assert_choice(time_scale, supported_time_scales())
  assert_time_range(model_time_range, null.ok = TRUE)

  poll_ids <- tibble::tibble(.poll_id = poll_ids(x), i = 1:length(poll_ids(x)))
  tl <- get_time_line(x, model_time_range, time_scale)
  assert_poll_data_in_time_line(x, tl)

  tws <- polls_time_weights(x)
  tws <- summarize_polls_time_weights(ptw = tws, tl)
  tws <- dplyr::left_join(tws, tl$time_line, by = "date")
  tws <- dplyr::left_join(tws, poll_ids, by = ".poll_id")

  yvar <- y(x)[, y_name, drop = TRUE]
  sigma_y <- sqrt(yvar * (1 - yvar) / n(x))

  sd <- list(T = get_total_time_points_from_time_line(tl),
             N = length(x),
             L = nrow(tws),
             y = yvar,
             sigma_y = sigma_y,
             tw = tws$weight,
             tw_t = tws$t,
             tw_i = tws$i)

  psd <- list(stan_data = sd,
              time_line = tl,
              poll_ids =  poll_ids)
  class(psd) <- c("model2", "stan_polls_data")
  assert_stan_polls_data(psd)
  assert_stan_data_model(psd)
  psd
}

#' @rdname stan_polls_data
#' @export
stan_polls_data_model3 <- function(x, y_name, time_scale = "week", model_time_range = NULL){
  spd <- stan_polls_data_model2(x, y_name, time_scale, model_time_range)
  spd$stan_data$time_scale_length <- time_scale_length(time_scale)
  class(spd) <- c("model3", "stan_polls_data")
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

time_scale_length <- function(time_scale){
  if(time_scale == "day") tsl <- 1
  if(time_scale == "week") tsl <- 7
  if(time_scale == "month") tsl <- 30
  return(tsl)
}

#' @rdname stan_polls_data
#' @export
stan_polls_data_model5 <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL){
  spd <- stan_polls_data_model3(x, y_name, time_scale, model_time_range)

  sdks <- stan_data_known_state(y_name, stan_data_time_line = spd$time_line, known_state)
  spd$stan_data$T_known <- sdks$T_known
  spd$stan_data$x_known <- as.array(sdks$x_known[,1])
  spd$stan_data$x_known_t <- sdks$x_known_t
  spd$stan_data$x_unknown_t <- sdks$x_unknown_t

  class(spd) <- c("model5", "stan_polls_data")
  assert_stan_polls_data(x = spd)
  assert_stan_data_model(x = spd)
  spd
}

#' @rdname stan_polls_data
#' @export
stan_polls_data_model6 <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL){

  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_time_range(model_time_range, null.ok = TRUE)

  poll_ids <- tibble::tibble(.poll_id = poll_ids(x), i = 1:length(poll_ids(x)))
  tl <- get_time_line(x, model_time_range, time_scale)

  assert_poll_data_in_time_line(x, tl)

  tws <- polls_time_weights(x)
  tws <- summarize_polls_time_weights(ptw = tws, tl)
  tws <- dplyr::left_join(tws, tl$time_line, by = "date")
  tws <- dplyr::left_join(tws, poll_ids, by = ".poll_id")

  ymat <- as.matrix(y(x)[, y_name, drop = FALSE])
  sigma_y <- ymat * (1 - ymat)
  for(i in 1:nrow(sigma_y)){
    sigma_y[i, ] <- sqrt(sigma_y[i, ] / n(x)[i])
  }

  tsl <- time_scale_length(time_scale)

  sdks <- stan_data_known_state(y_name, stan_data_time_line = tl, known_state)

  sd <- list(T = get_total_time_points_from_time_line(tl),
             N = length(x),
             L = nrow(tws),
             P = ncol(ymat),
             y = ymat,
             sigma_y = sigma_y,
             tw = tws$weight,
             tw_t = tws$t,
             tw_i = tws$i,
             time_scale_length = tsl,
             T_known = sdks$T_known,
             x_known = sdks$x_known,
             x_known_t = sdks$x_known_t,
             x_unknown_t = sdks$x_unknown_t)

  psd <- list(stan_data = sd,
              time_line = tl,
              poll_ids =  poll_ids)
  class(psd) <- c("model6", "stan_polls_data")
  assert_stan_polls_data(psd)
  assert_stan_data_model(psd)
  psd
}



stan_data_known_state <- function(y_name, stan_data_time_line, known_state){
  sdks <- list()
  tl <- stan_data_time_line
  T <- get_total_time_points_from_time_line(tl)
  if(is.null(known_state)){
    sdks$T_known <- 0
    sdks$x_known <- matrix(0, nrow = 0, ncol = length(y_name))
    sdks$x_known_t <- integer(0)
    sdks$x_unknown_t <- 1:T
  } else {
    sdks$T_known <- nrow(known_state)
    sdks$x_known <- as.matrix(known_state[, y_name, drop = FALSE])
    sdks$x_known_t <- as.array(get_time_points_from_time_line(dates = known_state[, "date", drop = TRUE], tl = tl))
    sdks$x_unknown_t <- as.array((1:T)[-sdks$x_known_t])
  }
  sdks
}


#' Build a Parallel Stan Data Path with Time Scale Overrides
#'
#' @description
#' Build a parallel future [stan_data] path using [time_line_with_overrides()].
#' The current [stan_data] and [time_line] fields are left unchanged; the future
#' path is attached in [stan_data_with_overrides] and [time_line_with_overrides].
#'
#' @param spd a [stan_polls_data] object.
#' @param x a [polls_data] object.
#' @param y_name a character vector indicating y variables in polls object.
#' @param model the Stan model name.
#' @param time_scale the base time scale.
#' @param time_scale_overrides optional inclusive time scale override ranges.
#' @param known_state known time points in the latent state.
#' @param model_time_range the time range to model.
#' @param latent_time_ranges the time range of the latent state.
#' @param hyper_parameters optional model hyper parameters.
#' @param slow_scales optional slow-scale break dates.
#'
#' @return The input [stan_polls_data] object with [stan_data_with_overrides]
#'   and [time_line_with_overrides] added.
#'
#' @keywords internal
attach_stan_data_with_overrides <- function(spd,
                                            x,
                                            y_name,
                                            model = class(spd)[1],
                                            time_scale,
                                            time_scale_overrides = NULL,
                                            known_state = NULL,
                                            model_time_range = NULL,
                                            latent_time_ranges = NULL,
                                            hyper_parameters = NULL,
                                            slow_scales = NULL){
  assert_stan_polls_data(spd)
  assert_polls_data(x)
  checkmate::assert_subset(y_name, choices = names(y(x)))
  checkmate::assert_string(model)
  assert_time_scale(time_scale)
  assert_known_state(known_state)
  assert_time_range(model_time_range)
  assert_latent_time_ranges(latent_time_ranges)

  if(is.null(latent_time_ranges)){
    latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)
  }

  legacy_stan_data_names <- names(spd$stan_data)
  poll_ids <- tibble::tibble(.poll_id = poll_ids(x), i = 1:length(poll_ids(x)))
  tl <- time_line_with_overrides(
    model_time_range = model_time_range,
    time_scale = time_scale,
    time_scale_overrides = time_scale_overrides
  )
  assert_poll_data_in_time_line(x, tl)
  known_state_in_time_line <- known_state
  if(!is.null(known_state)){
    known_state_in_time_line <- known_state[dates_in_time_line(known_state$date, tl), , drop = FALSE]
  }

  tws <- polls_time_weights(x)
  tws <- summarize_polls_time_weights(ptw = tws, tl)
  tws <- dplyr::left_join(tws, tl$time_line[, c("date", "t")], by = "date")
  tws <- dplyr::left_join(tws, poll_ids, by = ".poll_id")

  ymat <- as.matrix(y(x)[, y_name, drop = FALSE])
  sigma_y <- ymat * (1 - ymat)
  for(i in 1:nrow(sigma_y)){
    sigma_y[i, ] <- sqrt(sigma_y[i, ] / n(x)[i])
  }

  sdks <- stan_data_known_state(y_name, stan_data_time_line = tl, known_state_in_time_line)
  sd <- list(T = get_total_time_points_from_time_line(tl),
             N = length(x),
             L = nrow(tws),
             P = ncol(ymat),
             y = ymat,
             sigma_y = sigma_y,
             tw = tws$weight,
             tw_t = tws$t,
             tw_i = tws$i,
             time_scale_length = time_scale_length(time_scale),
             T_known = sdks$T_known,
             x_known = sdks$x_known,
             x_known_t = sdks$x_known_t,
             x_unknown_t = sdks$x_unknown_t)
  sd <- stan_data_add_missing(sd)

  from_dates <- do.call(c, lapply(latent_time_ranges[y_name], function(x) x["from"]))
  to_dates <- do.call(c, lapply(latent_time_ranges[y_name], function(x) x["to"]))
  sd$t_start <- as.array(get_time_points_from_time_line(dates = from_dates, tl = tl) + 1L)
  sd$t_end <- as.array(get_time_points_from_time_line(dates = to_dates, tl = tl))
  sd$delta_days_t <- as.array(ifelse(is.na(tl$time_line$delta_days), 0L, tl$time_line$delta_days))
  sd$step_scale_t <- as.array(ifelse(is.na(tl$time_line$step_scale), 0.0, tl$time_line$step_scale))

  has_time_scale_overrides <- !is.null(time_scale_overrides) && nrow(time_scale_overrides) > 0
  is_model8k <- grepl(pattern = "^model8k[0-9]+$", x = model)
  is_model8m <- grepl(pattern = "^model8m[0-9]+$", x = model)

  if((is_model8k || is_model8m) && !is.null(known_state)){
    sd <- stan_data_add_model8km_common_fields(
      stan_data = sd,
      x = x,
      time_line = tl,
      known_state = known_state,
      known_state_in_time_line = known_state_in_time_line,
      time_scale = time_scale,
      slow_scales = slow_scales,
      use_date_diff_g_t = has_time_scale_overrides,
      use_date_diff_g_i = has_time_scale_overrides
    )

    if(is_model8k){
      sd <- stan_data_finalize_model8k(
        stan_data = sd,
        hyper_parameters = hyper_parameters,
        time_line = tl,
        y_name = y_name,
        model = model
      )
    }

    if(is_model8m){
      sd <- stan_data_finalize_model8m(
        stan_data = sd,
        hyper_parameters = hyper_parameters,
        time_line = tl,
        y_name = y_name,
        model = model
      )
    }
  } else if(any(c("s_i", "s_t") %in% legacy_stan_data_names)){
    tls <- time_line_add_slow_scale(tl, slow_scales)
    if("s_i" %in% legacy_stan_data_names){
      sd$s_i <- get_time_points_from_time_line(collection_midpoint_dates(x), tls, "time_line_s")
    }
    if("s_t" %in% legacy_stan_data_names){
      sd$s_t <- stan_data_s_t(tls)
    }
    if("g_t" %in% legacy_stan_data_names && !is.null(known_state)){
      sd$g_t <- as.array(stan_data_g_t_date_diff(known_state = known_state, time_line = tl))
    }
    if("g_i" %in% legacy_stan_data_names && !is.null(known_state)){
      sd$g_i <- suppressWarnings(stan_data_g_i_date_diff(x = x, known_state = known_state, type = "collection_midpoint"))
    }
    if("next_known_state_t_index" %in% legacy_stan_data_names && !is.null(known_state)){
      sd$next_known_state_t_index <- as.integer(get_time_line_next_known_state_index(time_line = tl, known_state = known_state))
    }
  }

  spd$stan_data_with_overrides <- sd
  spd$time_line_with_overrides <- tl
  spd
}

#' @rdname stan_polls_data
#' @export
stan_polls_data_model6b <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  spd <- stan_polls_data_model6(x, y_name, time_scale, known_state, model_time_range)
  spd$stan_data$y_missing <- 1 * is.na(spd$stan_data$y)
  spd$stan_data$y[spd$stan_data$y_missing == 1] <- 0
  spd$stan_data$sigma_y[spd$stan_data$y_missing == 1] <- 0

  from_dates <- do.call(c, lapply(latent_time_ranges[y_name], function(x) x["from"]))
  to_dates <- do.call(c, lapply(latent_time_ranges[y_name], function(x) x["to"]))
  spd$stan_data$t_start <- as.array(get_time_points_from_time_line(dates = from_dates, spd$time_line) + 1L)
  spd$stan_data$t_end <- as.array(get_time_points_from_time_line(dates = to_dates, spd$time_line))

  class(spd) <- c("model6b", "stan_polls_data")

  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

#' @rdname stan_polls_data
#' @export
stan_polls_data_model7 <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL){
  spd <- stan_polls_data_model6b(x,
                                 y_name,
                                 time_scale,
                                 known_state,
                                 model_time_range,
                                 latent_time_ranges)
  class(spd) <- c("model7", "stan_polls_data")
  spd
}

#' @rdname stan_polls_data
#' @export
stan_polls_data_model8a <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  tl <- get_time_line(x, model_time_range, time_scale)
  ks <- known_state[dates_in_time_line(known_state$date, tl),]
  spd <- stan_polls_data_model6b(x, y_name, time_scale, ks, model_time_range, latent_time_ranges)

  spd$stan_data$next_known_state_index <- get_polls_next_known_state_index(x = x, known_state = ks, type = "collection_midpoint")
  spd$stan_data$g <- get_polls_time_points_since_last_known_state(x, known_state = known_state, tl = tl, type = "collection_midpoint")

  if(is.null(hyper_parameters$sigma_kappa_hyper)){
    spd$stan_data$sigma_kappa_hyper <- 0.002 / spd$stan_data$time_scale_length
  } else {
    spd$stan_data$sigma_kappa_hyper <- hyper_parameters$sigma_kappa_hyper
  }

  class(spd) <- c("model8a", "stan_polls_data")
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

stan_polls_data_model8a3 <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL){
  spd <- stan_polls_data_model8a(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges, hyper_parameters)

  industry_bias_time_scale <- "year"
  spd$stan_data$g <- (spd$stan_data$g * time_scale_as_days(time_scale)) / time_scale_as_days(industry_bias_time_scale)

  if(is.null(hyper_parameters$sigma_kappa_hyper)){
    spd$stan_data$sigma_kappa_hyper <- 0.005 # industry bias per year prior
  } else {
    spd$stan_data$sigma_kappa_hyper <- hyper_parameters$sigma_kappa_hyper
  }

  class(spd) <- c("model8a3", "stan_polls_data")
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}


#' @rdname stan_polls_data
#' @export
stan_polls_data_model8a1 <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  tl <- get_time_line(x, model_time_range, time_scale)
  ks <- known_state[dates_in_time_line(known_state$date, tl),]
  spd <- stan_polls_data_model6b(x, y_name, time_scale, ks, model_time_range, latent_time_ranges)

  spd$stan_data$next_known_state_index <- get_polls_next_known_state_index(x = x, known_state = ks, type = "collection_midpoint")
  g <- get_polls_time_points_since_last_known_state(x, known_state = known_state, tl = tl, type = "collection_midpoint")
  g_total <- suppressWarnings(get_polls_time_points_between_known_states(x, known_state = known_state, tl = tl, type = "collection_midpoint"))
  g_mean <- g_total / 2  # Starts from 0
  spd$stan_data$g_intercept <- g_mean
  spd$stan_data$g_centered <- g - g_mean

  if(is.null(hyper_parameters$sigma_kappa_hyper)){
    spd$stan_data$sigma_kappa_hyper <- 0.002 / spd$stan_data$time_scale_length
  } else {
    spd$stan_data$sigma_kappa_hyper <- hyper_parameters$sigma_kappa_hyper
  }

  class(spd) <- c("model8a1", "stan_polls_data")
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}




#' Get the next known state index
#'
#' @details
#' The function returns the index for the next known state.
#' Polls without a next unknown state, gets index [nrow(known_state) + 1].
#'
#' @param x a [polls_data] object
#' @param known_state a [known_state] object
#' @param type how is the next state index defined.
#' Default is based on [collection_period] dates.
#' @param dates Dates to get next index for
#' @param time_line time line to get next index for
#'
get_polls_next_known_state_index <- function(x, known_state, type = "collection_midpoint"){
  assert_polls_data(x)
  assert_known_state(known_state)
  checkmate::assert_choice(type, choices = "collection_midpoint")

  if(type == "collection_midpoint"){
    dates <- collection_midpoint_dates(x)
  } else {
    stop("Incorrect type!")
  }
  get_dates_next_known_state_index(dates, known_state)
}

#' @rdname get_polls_next_known_state_index
get_time_line_next_known_state_index <- function(time_line, known_state){
  assert_time_line(time_line)
  assert_known_state(known_state)
  get_dates_next_known_state_index(time_line$time_line$date, known_state)
}

#' @rdname get_polls_next_known_state_index
get_dates_next_known_state_index <- function(dates, known_state){
  checkmate::assert_date(dates)
  assert_known_state(known_state)

  bool_matrix <- matrix(FALSE, nrow = length(dates), ncol = nrow(known_state))
  for (j in 1:nrow(known_state)){
    bool_matrix[,j] <- (dates <= known_state$date[j])
  }

  nrow(known_state) - rowSums(bool_matrix) + 1
}


#' Get the number of time points since the last known state
#'
#' @details
#' The function returns the number of time points since the last known state.
#'
#' @param x a [polls_data] object
#' @param known_state a [known_state] object
#' @param type how is the next state index defined.
#' Default is based on [collection_period] dates.
#' @param dates Dates to get the number of time points since the last known state
#' @param tl a [time_line] object to get the number of time points since last known state
get_polls_time_points_since_last_known_state <- function(x, known_state, tl, type = "collection_midpoint"){
  assert_polls_data(x)
  assert_known_state(known_state)
  assert_time_line(tl)
  checkmate::assert_choice(type, choices = "collection_midpoint")

  if(type == "collection_midpoint"){
    dates <- collection_midpoint_dates(x)
  } else {
    stop("Incorrect type!")
  }

  get_dates_time_points_since_last_known_state(dates, known_state = known_state, tl = tl)
}

#' @rdname get_polls_time_points_since_last_known_state
get_time_line_time_points_since_last_known_state <- function(tl, known_state){
  assert_time_line(tl)
  assert_known_state(known_state)
  get_dates_time_points_since_last_known_state(tl$time_line$date, known_state, tl)
}

#' @rdname get_polls_time_points_since_last_known_state
get_dates_time_points_since_last_known_state <- function(dates, known_state, tl){
  get_dates_time_points_type_known_state(dates, known_state, tl, "since_last")
}


#' Get the number of time points between known state
#'
#' @details
#' The function returns the number of time points between
#' the known states.
#'
#' @param x a [polls_data] object
#' @param known_state a [known_state] object
#' @param type how is the next state index defined.
#' Default is based on [collection_period] dates.
#' @param dates Dates to get the number of time points between known states
#' @param tl a [time_line] object to get the number of time points between known state
get_polls_time_points_between_known_states <- function(x, known_state, tl, type = "collection_midpoint"){
  assert_polls_data(x)
  assert_known_state(known_state)
  assert_time_line(tl)
  checkmate::assert_choice(type, choices = "collection_midpoint")

  if(type == "collection_midpoint"){
    dates <- collection_midpoint_dates(x)
  } else {
    stop("Incorrect type!")
  }

  get_dates_time_points_between_known_states(dates, known_state = known_state, tl = tl)
}

#' @rdname get_polls_time_points_between_known_states
get_time_line_time_points_between_known_states <- function(tl, known_state){
  assert_time_line(tl)
  assert_known_state(known_state)
  get_dates_time_points_between_known_states(tl$time_line$date, known_state, tl)
}

#' @rdname get_polls_time_points_between_known_states
get_dates_time_points_between_known_states <- function(dates, known_state, tl){
  get_dates_time_points_type_known_state(dates, known_state, tl, "between")
}

# The core function that is used
get_dates_time_points_type_known_state <- function(dates, known_state, tl, type){
  checkmate::assert_date(dates)
  assert_known_state(known_state)
  assert_time_line(tl)
  checkmate::assert_choice(type, c("since_last", "between"))

  etr <- time_range(c(min(c(tl$daily$time_line_date, known_state$date)),
                      max(c(tl$daily$time_line_date, known_state$date))))
  etl <- time_line_expand(tl, time_range = etr)
  pksidx <- get_dates_next_known_state_index(dates, known_state)
  kstp <- get_time_points_from_time_line(known_state$date, etl)
  ptp <- get_time_points_from_time_line(dates, etl)
  if(any(pksidx <= 1)){
    pksidx <- pksidx + 1
    kstp <- c(0L, kstp)
    warning("'known_state' is missing before some dates.\n Assumes the previous 'known_state' is at time point zero.", call. = FALSE)
  }

  if(type == "since_last"){
    tpslks <- ptp - kstp[pksidx - 1]
    return(tpslks)
  }
  if(type == "between"){
    kstp <- c(kstp, max(etl$time_line[, "t", drop = TRUE]) + 1)
    len <- kstp[-1] - kstp[-length(kstp)] - 1L

    tplen <- len[pksidx - 1]
    return(tplen)
  }
}



assert_stan_polls_data <- function(x){
  checkmate::assert_class(x, "stan_polls_data")
  checkmate::assert_names(names(x),  must.include  = c("stan_data", "time_line", "poll_ids"))
  checkmate::assert_list(x$stan_data)
  checkmate::assert_class(x$time_line, "time_line")
  checkmate::assert_data_frame(x$poll_ids)
  checkmate::assert_names(names(x$poll_ids), identical.to = c(".poll_id", "i"))
}

#' Check Stan Data for specific model
#'
#' @param x a [poll_of_polls] object
#'
#' @export
assert_stan_data_model <- function(x){
  UseMethod("assert_stan_data_model")
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model2 <- function(x){
  checkmate::assert_integerish(x$stan_data$tw_i, lower = 1, upper = length(x$stan_data$y))
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model3 <- function(x){
  assert_stan_data_model.model2(x)
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model5 <- function(x){
  assert_stan_data_model.model2(x)

  # Test for data on true latent states
  checkmate::assert_integerish(x$stan_data$T_known, lower = 0, upper = x$stan_data$T)
  checkmate::assert_numeric(x$stan_data$x_known, len = x$stan_data$T_known)
  checkmate::assert_integerish(x$stan_data$x_known_t, len = x$stan_data$T_known, upper = x$stan_data$T)
  checkmate::assert_integerish(x$stan_data$x_unknown_t, len = x$stan_data$T - x$stan_data$T_known, upper = x$stan_data$T)
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model6 <- function(x){
  checkmate::assert_integerish(x$stan_data$tw_i, lower = 1, upper = x$stan_data$N)

  # Test for data on true latent states
  checkmate::assert_integerish(x$stan_data$T_known, lower = 0, upper = x$stan_data$T)
  checkmate::assert_matrix(x$stan_data$x_known, nrows = x$stan_data$T_known, ncols = x$stan_data$P)
  checkmate::assert_integerish(x$stan_data$x_known_t, len = x$stan_data$T_known, upper = x$stan_data$T)
  checkmate::assert_integerish(x$stan_data$x_unknown_t, len = x$stan_data$T - x$stan_data$T_known, upper = x$stan_data$T)

}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model6b <- function(x){
  assert_stan_data_model.model6(x)
  checkmate::assert_integerish(x$stan_data$t_start, lower = 1, upper = x$stan_data$T, len = x$stan_data$P)
  checkmate::assert_integerish(x$stan_data$t_end, lower = 1, upper = x$stan_data$T, len = x$stan_data$P)
  checkmate::assert_matrix(x$stan_data$y_missing, mode = "integerish", any.missing = FALSE, nrows = nrow(x$stan_data$y), ncols = ncol(x$stan_data$y))
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8a <- function(x){
  assert_stan_data_model.model6b(x)
  checkmate::assert_integerish(x$stan_data$g, lower = 0, upper = x$stan_data$T, len = x$stan_data$N)
  checkmate::assert_integerish(x$stan_data$next_known_state_index, lower = 0, upper = x$stan_data$T_known + 1, len = x$stan_data$N)
  checkmate::assert_number(x$stan_data$sigma_kappa_hyper, lower = 0)
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8a1 <- function(x){
  assert_stan_data_model.model6b(x)
  checkmate::assert_numeric(x$stan_data$g_intercept, lower = 0, upper = x$stan_data$T, len = x$stan_data$N)
  checkmate::assert_numeric(x$stan_data$g_centered, lower = -x$stan_data$T, upper = x$stan_data$T, len = x$stan_data$N)
  checkmate::assert_integerish(x$stan_data$next_known_state_index, lower = 0, upper = x$stan_data$T_known + 1, len = x$stan_data$N)
  checkmate::assert_number(x$stan_data$sigma_kappa_hyper, lower = 0)
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8a3 <- function(x){
  assert_stan_data_model.model6b(x)
  checkmate::assert_numeric(x$stan_data$g, lower = 0, len = x$stan_data$N)
  checkmate::assert_integerish(x$stan_data$next_known_state_index, lower = 0, upper = x$stan_data$T_known + 1, len = x$stan_data$N)
  checkmate::assert_number(x$stan_data$sigma_kappa_hyper, lower = 0)
}


assert_y_name <- function(x, pd){
  assert_polls_data(pd)
  checkmate::assert_subset(x, choices = names(y(pd)))
}




#' @rdname stan_polls_data
#' @export
stan_polls_data_model8b <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)
  assert_slow_scales(slow_scales, null.ok = TRUE)
  assert_all_houses_has_observations_in_polls_data(x)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)
  tl <- get_time_line(x, model_time_range, time_scale)
  spd <- stan_polls_data_model6b(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges)

  # Compute S and H
  tls <- time_line_add_slow_scale(tl, slow_scales)

  spd$stan_data$H <- stan_data_H(x)
  spd$stan_data$h_i <- stan_data_h_i(x)

  spd$stan_data$S <- stan_data_S(tls)
  spd$stan_data$s_i <- get_time_points_from_time_line(collection_midpoint_dates(x), tls, "time_line_s")

  mc <- model_config("model8k2", hyper_parameters)
  spd$stan_data <- c(spd$stan_data, mc)

  class(spd) <- c("model8b", "stan_polls_data")
  assert_all_periods_has_observations_in_polls_data(spd)
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

stan_data_H <- function(x){
  assert_polls_data(x)
  length(unique(x$poll_info$.house))
}

stan_data_h_i <- function(x){
  assert_polls_data(x)
  as.integer(x$poll_info$.house)
}

stan_data_S <- function(tls){
  # length(tls$slow_scales) + 1
  assert_time_line(tls)
  S <- 1L
  if(!is.null(tls$slow_scales)){
    S <- length(tls$slow_scales) + 1L
  }
  S
}

stan_data_s_t <- function(tls){
  dts <- tls$time_line$date
  tr <- time_range(tls)
  dts[dts < tr["from"]] <- tr["from"]
  dts[dts > tr["to"]] <- tr["to"]
  get_time_points_from_time_line(dts, tls, "time_line_s")
}

stan_data_g <- function(x, known_state, time_line, time_scale, type = "collection_midpoint"){
  assert_known_state(known_state)
  assert_time_line(time_line)
  assert_time_scale(time_scale)

  g <- get_polls_time_points_since_last_known_state(x, known_state = known_state, tl = time_line, type = type)
  stan_data_normalize_g_by(g, time_scale, by = "year")
}

stan_data_g_t <- function(known_state, time_line, time_scale){
  assert_known_state(known_state)
  assert_time_line(time_line)
  assert_time_scale(time_scale)

  dates <- time_line$time_line$date
  g <- get_dates_time_points_since_last_known_state(dates, known_state = known_state, tl = time_line)
  stan_data_normalize_g_by(g, time_scale, by = "year")
}

stan_data_g_t_date_diff <- function(known_state, time_line){
  assert_known_state(known_state)
  assert_time_line(time_line)

  ks <- known_state[order(known_state$date), , drop = FALSE]
  dates <- time_line$time_line$date
  prev_known_state_index <- findInterval(dates, ks$date)
  prev_known_state_dates <- ks$date[pmax(prev_known_state_index, 1L)]

  if(any(prev_known_state_index < 1L)){
    warning("'known_state' is missing before some dates.\n Assumes the previous 'known_state' is at the first latent time point.", call. = FALSE)
    prev_known_state_dates[prev_known_state_index < 1L] <- dates[prev_known_state_index < 1L]
  }

  as.numeric(dates - prev_known_state_dates) / time_scale_as_days("year")
}

stan_data_g_i_date_diff <- function(x, known_state, type = "collection_midpoint"){
  assert_polls_data(x)
  assert_known_state(known_state)
  checkmate::assert_choice(type, choices = "collection_midpoint")

  if(type == "collection_midpoint"){
    dates <- collection_midpoint_dates(x)
  } else {
    stop("Incorrect type!")
  }

  ks <- known_state[order(known_state$date), , drop = FALSE]
  prev_known_state_index <- findInterval(dates, ks$date)
  prev_known_state_dates <- ks$date[pmax(prev_known_state_index, 1L)]

  if(any(prev_known_state_index < 1L)){
    warning("'known_state' is missing before some dates.\n Assumes the previous 'known_state' is at the collection midpoint date.", call. = FALSE)
    prev_known_state_dates[prev_known_state_index < 1L] <- dates[prev_known_state_index < 1L]
  }

  as.numeric(dates - prev_known_state_dates) / time_scale_as_days("year")
}

stan_data_normalize_g_by <- function(g, time_scale, by = "year"){
  checkmate::assert_choice(by, "year")
  assert_time_scale(time_scale)
  (g * time_scale_as_days(time_scale)) / time_scale_as_days(by)
}

#' @rdname stan_polls_data
#' @export
stan_polls_data_model8c <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)
  assert_slow_scales(slow_scales, null.ok = TRUE)
  assert_all_houses_has_observations_in_polls_data(x)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)
  tl <- get_time_line(x, model_time_range, time_scale)
  spd <- stan_polls_data_model6b(x, y_name, time_scale, known_state, model_time_range, latent_time_ranges)

  # Compute S and H
  tls <- time_line_add_slow_scale(tl, slow_scales)

  spd$stan_data$H <- stan_data_H(x)
  spd$stan_data$h_i <- stan_data_h_i(x)

  spd$stan_data$S <- stan_data_S(tls)
  spd$stan_data$s_i <- get_time_points_from_time_line(collection_midpoint_dates(x), tls, "time_line_s")

  mc <- model_config("model8k2", hyper_parameters)
  spd$stan_data <- c(spd$stan_data, mc)

  class(spd) <- c("model8c", "stan_polls_data")
  assert_all_periods_has_observations_in_polls_data(spd)
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}



assert_all_houses_has_observations_in_polls_data <- function(x){
  empty_house <- table(x$poll_info$.house) == 0
  if(any(empty_house)){
    stop(paste0("Observations are missing for house(s): ", paste0(names(which(empty_house)), collapse = ", ")),call. = FALSE)
  }
}

assert_all_periods_has_observations_in_polls_data <- function(x){
  checkmate::assert_class(x, classes = "stan_polls_data")
  s_factor <- factor(x$stan_data$s_i, levels = 1:x$stan_data$S)
  empty_period <- table(s_factor) == 0
  if(any(empty_period)){
    warning(paste0("Observations are missing for slow scale period(s): ", paste0(paste0("s = ", names(which(empty_period))), collapse = ", ")),call. = FALSE)
  }
}

assert_stan_data_model.model8b <- function(x){
  assert_stan_data_model.model6b(x)
  assert_stan_data_H(x)
  assert_stan_data_h_i(x)
  assert_stan_data_S(x)
  assert_stan_data_s_i(x)
  assert_model_arguments(x)
}

assert_stan_data_H <- function(x){
  checkmate::assert_int(x$stan_data$H, lower = 1, upper = x$stan_data$N)
}

assert_stan_data_h_i <- function(x){
  checkmate::assert_integerish(x$stan_data$h_i, lower = 1, upper = x$stan_data$H, len = x$stan_data$N)
}

assert_stan_data_S <- function(x){
  checkmate::assert_int(x$stan_data$S, lower = 1, upper = x$stan_data$T)
}

assert_stan_data_s_i <- function(x){
  checkmate::assert_integerish(x$stan_data$s_i, lower = 1, upper = x$stan_data$S, len = x$stan_data$N)
}


#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8c <- function(x){
  assert_stan_data_model.model6b(x)
  assert_stan_data_H(x)
  assert_stan_data_h_i(x)
  assert_stan_data_S(x)
  assert_stan_data_s_i(x)
  assert_model_arguments(x)
}


assert_slow_scales <- function(x, null.ok = FALSE){
  checkmate::assert_date(x, min.len = 1, null.ok = null.ok, unique = TRUE)
}


#' Create a time line with slower scales
#'
#' @param tl a [time_line] object
#' @inheritParams stan_polls_data
#'
#' @keywords internal
time_line_add_slow_scale <- function(tl, slow_scales){
  assert_time_line(tl)
  assert_slow_scales(slow_scales, null.ok = TRUE)
  if(is.null(slow_scales)) {
    tl$slow_scales <- max(tl$daily[, "date", drop=TRUE])
  } else {
    tl$slow_scales <- slow_scales[order(slow_scales)]
  }
  tl$daily[, "time_line_s"] <- 0L
  max_date <- time_range(tl)[2]
  for(i in seq_along(tl$daily$date)){
    tl$daily[i, "time_line_s"] <- min(which(tl$daily$date[i] <= c(tl$slow_scales, max_date)))
  }
  tlds <- tl$daily[, c("date", "time_line_s")]
  names(tlds)[2] <- "s"
  tl$time_line <- dplyr::left_join(tl$time_line, tlds, by = "date")
  names(tl$time_line)[length(names(tl$time_line))] <- "s"
  if(is.null(slow_scales)) tl$slow_scales <- NULL
  assert_time_line(tl)
  tl
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8b <- function(x){
  assert_stan_data_model.model6b(x)
  checkmate::assert_int(x$stan_data$H, lower = 1, upper = x$stan_data$N)
  checkmate::assert_integerish(x$stan_data$h_i, lower = 1, upper = x$stan_data$H, len = x$stan_data$N)
  checkmate::assert_int(x$stan_data$S, lower = 1, upper = x$stan_data$T)
  checkmate::assert_integerish(x$stan_data$s_i, lower = 1, upper = x$stan_data$S, len = x$stan_data$N)
}


#' @rdname stan_polls_data
#' @export
stan_polls_data_model8d <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL, model){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)
  assert_all_houses_has_observations_in_polls_data(x)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  tl <- get_time_line(x, model_time_range, time_scale)
  ks <- known_state[dates_in_time_line(known_state$date, tl),]
  spd <- stan_polls_data_model6b(x, y_name, time_scale, ks, model_time_range, latent_time_ranges)

  # Compute industry bias data
  spd$stan_data$next_known_state_index <- get_polls_next_known_state_index(x = x, known_state = ks, type = "collection_midpoint")
  spd$stan_data$g <- stan_data_g(x = x, known_state = known_state, time_line = tl, time_scale = time_scale, type = "collection_midpoint")

  # Compute S and H
  tls <- time_line_add_slow_scale(tl, slow_scales)
  spd$stan_data$H <- stan_data_H(x)
  spd$stan_data$h_i <- stan_data_h_i(x)
  spd$stan_data$S <- stan_data_S(tls)
  spd$stan_data$s_i <- get_time_points_from_time_line(collection_midpoint_dates(x), tls, "time_line_s")

  # Model configs
  mc <- model_config("model8k2", hyper_parameters, spd$stan_data)
  spd$stan_data <- c(spd$stan_data, mc)

  class(spd) <- c("model8d", "stan_polls_data")
  assert_all_periods_has_observations_in_polls_data(spd)
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8d <- function(x){
  assert_stan_data_model.model6b(x)
  assert_stan_data_g(x)
  assert_model_arguments(x$stan_data)
}

assert_stan_data_g <- function(x){
  checkmate::assert_numeric(x$stan_data$g, lower = 0, len = x$stan_data$N)
  checkmate::assert_integerish(x$stan_data$next_known_state_index, lower = 0, upper = x$stan_data$T_known + 1, len = x$stan_data$N)
}

stan_polls_data_model8e <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL, model){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)
  assert_all_houses_has_observations_in_polls_data(x)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  tl <- get_time_line(x, model_time_range, time_scale)
  ks <- known_state[dates_in_time_line(known_state$date, tl),]
  spd <- stan_polls_data_model6b(x, y_name, time_scale, ks, model_time_range, latent_time_ranges)

  # Compute industry bias data
  spd$stan_data$next_known_state_index <- get_polls_next_known_state_index(x = x, known_state = ks, type = "collection_midpoint")
  spd$stan_data$g <- stan_data_g(x = x, known_state = known_state, time_line = tl, time_scale = time_scale, type = "collection_midpoint")

  # Compute S and H
  tls <- time_line_add_slow_scale(tl, slow_scales)
  spd$stan_data$H <- stan_data_H(x)
  spd$stan_data$h_i <- stan_data_h_i(x)
  spd$stan_data$S <- stan_data_S(tls)
  spd$stan_data$s_i <- get_time_points_from_time_line(collection_midpoint_dates(x), tls, "time_line_s")

  # Model configs
  mc <- model_config(model, hyper_parameters, spd$stan_data)
  spd$stan_data <- c(spd$stan_data, mc)

  # Set known values to array of size 1
  spd$stan_data$alpha_kappa_known <- array(spd$stan_data$alpha_kappa_known, dim=1)
  spd$stan_data$alpha_beta_mu_known <- array(spd$stan_data$alpha_beta_mu_known, dim=1)
  spd$stan_data$alpha_beta_sigma_known <- array(spd$stan_data$alpha_beta_sigma_known, dim=1)

  class(spd) <- c("model8e", "stan_polls_data")
  assert_all_periods_has_observations_in_polls_data(spd)
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8e <- function(x){
  assert_stan_data_model.model6b(x)
  assert_stan_data_g(x)
  assert_model_arguments(x$stan_data)
}


stan_polls_data_model8f <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL, model){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)
  assert_all_houses_has_observations_in_polls_data(x)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  tl <- get_time_line(x, model_time_range, time_scale)
  ks <- known_state[dates_in_time_line(known_state$date, tl),]
  spd <- stan_polls_data_model6b(x, y_name, time_scale, ks, model_time_range, latent_time_ranges)

  # Compute industry bias data
  spd$stan_data$next_known_state_poll_index <- get_polls_next_known_state_index(x = x, known_state = ks, type = "collection_midpoint")
  spd$stan_data$next_known_state_t_index <- get_time_line_next_known_state_index(time_line = tl, known_state = ks)
  spd$stan_data$g_t <- stan_data_g_t(known_state = known_state, time_line = tl, time_scale = time_scale)
  spd$stan_data$g_i <- suppressWarnings(stan_data_g(x = x, known_state = known_state, time_line = tl, time_scale = time_scale, type = "collection_midpoint"))

  # Compute S and H
  tls <- time_line_add_slow_scale(tl, slow_scales)
  spd$stan_data$H <- stan_data_H(x)
  spd$stan_data$h_i <- stan_data_h_i(x)
  spd$stan_data$S <- stan_data_S(tls)
  spd$stan_data$s_i <- get_time_points_from_time_line(collection_midpoint_dates(x), tls, "time_line_s")

  # Model configs
  mc <- model_config(model, hyper_parameters, spd$stan_data)
  spd$stan_data <- c(spd$stan_data, mc)

  # Set known values to array of size 1
  spd$stan_data$alpha_kappa_known <- array(spd$stan_data$alpha_kappa_known, dim=1)
  spd$stan_data$alpha_beta_mu_known <- array(spd$stan_data$alpha_beta_mu_known, dim=1)
  spd$stan_data$alpha_beta_sigma_known <- array(spd$stan_data$alpha_beta_sigma_known, dim=1)

  class(spd) <- c("model8f", "stan_polls_data")
  assert_all_periods_has_observations_in_polls_data(spd)
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8f <- function(x){
  assert_stan_data_model.model6b(x)
  assert_stan_data_H(x)
  assert_stan_data_h_i(x)
  assert_stan_data_S(x)
  assert_stan_data_s_i(x)
  assert_stan_data_g_i_and_g_t(x)
  assert_model_arguments(x)
}

assert_stan_data_g_i_and_g_t <- function(x){
  checkmate::assert_numeric(x$stan_data$g_t, lower = 0, len = x$stan_data$T)
  checkmate::assert_numeric(x$stan_data$g_i, lower = 0, len = x$stan_data$N)
  # For dates just after the known state dates but at the same time point
  # g can be 0, even if this is not part of the g_t. Although, this
  # wil most likely not happen in any real data scenarios.
  has_mixed_step_scales <- FALSE
  if(!is.null(x$stan_data$step_scale_t)){
    step_scale_t <- x$stan_data$step_scale_t
    if(length(step_scale_t) > 1){
      has_mixed_step_scales <- any(abs(step_scale_t[-1] - 1) > 1e-12)
    }
  }
  if(!has_mixed_step_scales){
    checkmate::assert_subset(x$stan_data$g_i, c(x$stan_data$g_t, 0))
  }
  checkmate::assert_integerish(x$stan_data$next_known_state_poll_index, lower = 0, upper = x$stan_data$T_known + 1, len = x$stan_data$N)
  checkmate::assert_integerish(x$stan_data$next_known_state_t_index, lower = 0, upper = x$stan_data$T_known + 1, len = x$stan_data$T)
}

stan_polls_data_model10d <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL,model){
  spd <- stan_polls_data_model8d(x,
                                 y_name,
                                 time_scale = "week",
                                 known_state,
                                 model_time_range,
                                 latent_time_ranges,
                                 hyper_parameters,
                                 slow_scales,
                                 model)
  class(spd) <- c("model10d","stan_polls_data")
  return(spd)
}


stan_polls_data_model10e <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL, model){
  spd <- stan_polls_data_model8d(x,
                                 y_name,
                                 time_scale = "week",
                                 known_state,
                                 model_time_range,
                                 latent_time_ranges,
                                 hyper_parameters,
                                 slow_scales,
                                 model)
  class(spd) <- c("model10e","stan_polls_data")
  return(spd)
}
stan_polls_data_model11a <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL, model){
  spd <- stan_polls_data_model8d(x,
                                 y_name,
                                 time_scale = "week",
                                 known_state,
                                 model_time_range,
                                 latent_time_ranges,
                                 hyper_parameters,
                                 slow_scales,
                                 model)
  class(spd) <- c("model11a","stan_polls_data")
  return(spd)
}


stan_polls_data_model11b <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL, model){
  spd <- stan_polls_data_model8e(x,
                                 y_name,
                                 time_scale = "week",
                                 known_state,
                                 model_time_range,
                                 latent_time_ranges,
                                 hyper_parameters,
                                 slow_scales,
                                 model)
  class(spd) <- c("model11a","stan_polls_data")
  return(spd)
}



stan_polls_data_model8g <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL, model){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)
  assert_all_houses_has_observations_in_polls_data(x)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  tl <- get_time_line(x, model_time_range, time_scale)
  ks <- known_state[dates_in_time_line(known_state$date, tl),]
  spd <- stan_polls_data_model6b(x, y_name, time_scale, ks, model_time_range, latent_time_ranges)

  # Compute industry bias data
  spd$stan_data$next_known_state_poll_index <- get_polls_next_known_state_index(x = x, known_state = ks, type = "collection_midpoint")
  spd$stan_data$next_known_state_t_index <- get_time_line_next_known_state_index(time_line = tl, known_state = ks)
  spd$stan_data$g_t <- stan_data_g_t(known_state = known_state, time_line = tl, time_scale = time_scale)
  spd$stan_data$g_i <- suppressWarnings(stan_data_g(x = x, known_state = known_state, time_line = tl, time_scale = time_scale, type = "collection_midpoint"))

  # Compute S and H
  tls <- time_line_add_slow_scale(tl, slow_scales)
  spd$stan_data$H <- stan_data_H(x)
  spd$stan_data$h_i <- stan_data_h_i(x)
  spd$stan_data$S <- stan_data_S(tls)
  spd$stan_data$s_i <- get_time_points_from_time_line(collection_midpoint_dates(x), tls, "time_line_s")
  spd$stan_data$s_t <- stan_data_s_t(tls)

  # Model configs
  mc <- model_config(model, hyper_parameters, spd$stan_data)
  spd$stan_data <- c(spd$stan_data, mc)

  # Set known values to array of size 1
  spd$stan_data$alpha_kappa_known <- array(spd$stan_data$alpha_kappa_known, dim=1)
  spd$stan_data$alpha_beta_mu_known <- array(spd$stan_data$alpha_beta_mu_known, dim=1)
  spd$stan_data$alpha_beta_sigma_known <- array(spd$stan_data$alpha_beta_sigma_known, dim=1)

  class(spd) <- c("model8g", "stan_polls_data")
  assert_all_periods_has_observations_in_polls_data(spd)
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8g <- function(x){
  assert_stan_data_model.model6b(x)
  assert_stan_data_H(x)
  assert_stan_data_h_i(x)
  assert_stan_data_S(x)
  assert_stan_data_s_i(x)
  assert_stan_data_s_t(x)
  assert_stan_data_g_i_and_g_t(x)
  assert_model_arguments(x)
}

assert_stan_data_s_t <- function(x){
  checkmate::assert_integerish(x$stan_data$s_t, lower = 1, upper = x$stan_data$S, len = x$stan_data$T)
}





stan_polls_data_model8i <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL, model){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)
  assert_all_houses_has_observations_in_polls_data(x)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  tl <- get_time_line(x, model_time_range, time_scale)
  ks <- known_state[dates_in_time_line(known_state$date, tl),]
  spd <- stan_polls_data_model6b(x, y_name, time_scale, ks, model_time_range, latent_time_ranges)

  # Compute industry bias data
  spd$stan_data$next_known_state_poll_index <- get_polls_next_known_state_index(x = x, known_state = ks, type = "collection_midpoint")
  spd$stan_data$next_known_state_t_index <- get_time_line_next_known_state_index(time_line = tl, known_state = ks)
  spd$stan_data$g_t <- stan_data_g_t(known_state = known_state, time_line = tl, time_scale = time_scale)
  spd$stan_data$g_i <- suppressWarnings(stan_data_g(x = x, known_state = known_state, time_line = tl, time_scale = time_scale, type = "collection_midpoint"))

  # Compute Pp
  spd$stan_data$Pp <- as.integer(spd$stan_data$P * (spd$stan_data$P - 1) / 2)

  # Compute S and H
  tls <- time_line_add_slow_scale(tl, slow_scales)
  spd$stan_data$H <- stan_data_H(x)
  spd$stan_data$h_i <- stan_data_h_i(x)
  spd$stan_data$S <- stan_data_S(tls)
  spd$stan_data$s_i <- get_time_points_from_time_line(collection_midpoint_dates(x), tls, "time_line_s")
  spd$stan_data$s_t <- stan_data_s_t(tls)

  # Model configs
  mc <- model_config(model, hyper_parameters, spd$stan_data)
  spd$stan_data <- c(spd$stan_data, mc)

  # Set known values to array of size 1
  spd$stan_data$alpha_kappa_known <- array(spd$stan_data$alpha_kappa_known, dim=1)
  spd$stan_data$alpha_beta_mu_known <- array(spd$stan_data$alpha_beta_mu_known, dim=1)
  spd$stan_data$alpha_beta_sigma_known <- array(spd$stan_data$alpha_beta_sigma_known, dim=1)

  class(spd) <- c("model8i", "stan_polls_data")
  assert_all_periods_has_observations_in_polls_data(spd)
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8i <- function(x){
  assert_stan_data_model.model6b(x)
  Pp <- as.integer(x$stan_data$P * (x$stan_data$P - 1) / 2)
  checkmate::assert_int(x$stan_data$Pp, lower = Pp, upper = Pp)
  assert_stan_data_H(x)
  assert_stan_data_h_i(x)
  assert_stan_data_S(x)
  assert_stan_data_s_i(x)
  assert_stan_data_s_t(x)
  assert_stan_data_g_i_and_g_t(x)
  assert_model_arguments(x)
}


stan_data_add_model8km_common_fields <- function(stan_data,
                                                 x,
                                                 time_line,
                                                 known_state,
                                                 known_state_in_time_line,
                                                 time_scale,
                                                 slow_scales = NULL,
                                                 use_date_diff_g_t = FALSE,
                                                 use_date_diff_g_i = FALSE){
  checkmate::assert_list(stan_data)
  assert_polls_data(x)
  assert_time_line(time_line)
  assert_known_state(known_state, null.ok = FALSE)
  assert_known_state(known_state_in_time_line, null.ok = FALSE)
  assert_time_scale(time_scale)
  assert_slow_scales(slow_scales, null.ok = TRUE)
  checkmate::assert_flag(use_date_diff_g_t)
  checkmate::assert_flag(use_date_diff_g_i)

  stan_data$next_known_state_poll_index <- get_polls_next_known_state_index(
    x = x,
    known_state = known_state_in_time_line,
    type = "collection_midpoint"
  )
  stan_data$next_known_state_t_index <- as.integer(get_time_line_next_known_state_index(
    time_line = time_line,
    known_state = known_state_in_time_line
  ))
  if(use_date_diff_g_t){
    stan_data$g_t <- as.array(stan_data_g_t_date_diff(
      known_state = known_state,
      time_line = time_line
    ))
  } else {
    stan_data$g_t <- stan_data_g_t(
      known_state = known_state,
      time_line = time_line,
      time_scale = time_scale
    )
  }
  if(use_date_diff_g_i){
    stan_data$g_i <- suppressWarnings(stan_data_g_i_date_diff(
      x = x,
      known_state = known_state,
      type = "collection_midpoint"
    ))
  } else {
    stan_data$g_i <- suppressWarnings(
      stan_data_g(
        x = x,
        known_state = known_state,
        time_line = time_line,
        time_scale = time_scale,
        type = "collection_midpoint"
      )
    )
  }

  stan_data$Pp <- as.integer(stan_data$P * (stan_data$P - 1) / 2)

  tls <- time_line_add_slow_scale(time_line, slow_scales)
  stan_data$H <- stan_data_H(x)
  stan_data$h_i <- stan_data_h_i(x)
  stan_data$S <- stan_data_S(tls)
  stan_data$s_i <- get_time_points_from_time_line(collection_midpoint_dates(x), tls, "time_line_s")
  stan_data$s_t <- stan_data_s_t(tls)

  stan_data
}


stan_data_finalize_model8k <- function(stan_data,
                                       hyper_parameters,
                                       time_line,
                                       y_name,
                                       model){
  checkmate::assert_list(stan_data)
  assert_time_line(time_line)
  checkmate::assert_character(y_name)
  checkmate::assert_string(model)

  hyper_parameters <- parse_obs_x(hyper_parameters, time_line, y_name)

  mc <- model_config(model, hyper_parameters, stan_data)
  stan_data <- c(stan_data, mc)

  stan_data$alpha_kappa_known <- array(stan_data$alpha_kappa_known, dim = 1)
  stan_data$alpha_beta_mu_known <- array(stan_data$alpha_beta_mu_known, dim = 1)
  stan_data$alpha_beta_sigma_known <- array(stan_data$alpha_beta_sigma_known, dim = 1)

  stan_data
}

stan_data_finalize_model8m <- function(stan_data,
                                       hyper_parameters,
                                       time_line,
                                       y_name,
                                       model){
  checkmate::assert_list(stan_data)
  assert_time_line(time_line)
  checkmate::assert_character(y_name)
  checkmate::assert_string(model)

  hyper_parameters <- parse_obs_x(hyper_parameters, time_line, y_name)
  hyper_parameters <- parse_election_period(hyper_parameters, time_line)
  if(is.null(hyper_parameters$EP)) hyper_parameters$EP <- as.integer(max(hyper_parameters$election_period))

  mc <- model_config(model, hyper_parameters, stan_data)
  stan_data <- c(stan_data, mc)

  stan_data$alpha_kappa_known <- array(stan_data$alpha_kappa_known, dim = 1)
  stan_data$alpha_beta_mu_known <- array(stan_data$alpha_beta_mu_known, dim = 1)
  stan_data$alpha_beta_sigma_known <- array(stan_data$alpha_beta_sigma_known, dim = 1)

  stan_data
}



stan_polls_data_model8k <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL, model){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)
  assert_all_houses_has_observations_in_polls_data(x)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  tl <- get_time_line(x, model_time_range, time_scale)
  ks <- known_state[dates_in_time_line(known_state$date, tl),]
  spd <- stan_polls_data_model6b(x, y_name, time_scale, ks, model_time_range, latent_time_ranges)

  spd$stan_data <- stan_data_add_model8km_common_fields(
    stan_data = spd$stan_data,
    x = x,
    time_line = tl,
    known_state = known_state,
    known_state_in_time_line = ks,
    time_scale = time_scale,
    slow_scales = slow_scales
  )

  spd$stan_data <- stan_data_finalize_model8k(
    stan_data = spd$stan_data,
    hyper_parameters = hyper_parameters,
    time_line = tl,
    y_name = y_name,
    model = model
  )

  class(spd) <- c("model8k", "stan_polls_data")
  assert_all_periods_has_observations_in_polls_data(spd)
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8k <- function(x){
  assert_stan_data_model.model8i(x)
  assert_model_arguments(x)
}

#' Parse a data frame of observed x (t distributed)
#'
#' @param hyper_parameters a list that contains the slot obs_x with the dataframe
#' @param time_line the time_line to use to parse dates to time points
#' @param y_name the different values used to parse obs_x$y to integers
parse_obs_x <- function(hyper_parameters, time_line, y_name){
  if(is.null(hyper_parameters)) return(NULL)
  checkmate::assert_list(hyper_parameters)
  assert_time_line(time_line)
  checkmate::assert_character(y_name)

  if(is.null(hyper_parameters$obs_x)) {
    hyper_parameters$use_obs_of_x <- 0L
    hyper_parameters$R <- 0L
    return(hyper_parameters)
  }
  assert_obs_x(hyper_parameters$obs_x, y_name)
  hyper_parameters$use_obs_of_x <- 1L
  hyper_parameters$R <- nrow(hyper_parameters$obs_x)
  hyper_parameters$obs_of_x_t <- get_time_points_from_time_line(hyper_parameters$obs_x$date, time_line)
  hyper_parameters$obs_of_x_p <- unname(vapply(hyper_parameters$obs_x$y, FUN = function(x) which(y_name == x), FUN.VALUE = integer(1)))
  hyper_parameters$obs_of_x_mu <- hyper_parameters$obs_x$mu
  hyper_parameters$obs_of_x_sigma <- hyper_parameters$obs_x$sigma
  hyper_parameters$obs_of_x_nu <- hyper_parameters$obs_x$nu

  hyper_parameters$obs_x <- NULL

  hyper_parameters
}

assert_obs_x <- function(obs_x, y_name){
  checkmate::assert_data_frame(obs_x)
  checkmate::assert_names(colnames(obs_x), identical.to = c("date", "y", "mu", "sigma", "nu"))
  checkmate::assert_date(obs_x$date)
  checkmate::assert_names(as.character(obs_x$y), subset.of = y_name)
  checkmate::assert_numeric(obs_x$mu)
  checkmate::assert_numeric(obs_x$sigma, lower = 0)
  checkmate::assert_numeric(obs_x$nu, lower = 2)
}



stan_polls_data_model8l <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL, model){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)
  assert_all_houses_has_observations_in_polls_data(x)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  tl <- get_time_line(x, model_time_range, time_scale)
  ks <- known_state[dates_in_time_line(known_state$date, tl),]
  spd <- stan_polls_data_model6b(x, y_name, time_scale, ks, model_time_range, latent_time_ranges)

  spd$stan_data <- stan_data_add_model8km_common_fields(
    stan_data = spd$stan_data,
    x = x,
    time_line = tl,
    known_state = known_state,
    known_state_in_time_line = ks,
    time_scale = time_scale,
    slow_scales = slow_scales
  )


  # Compute observations of x
  hyper_parameters <- parse_obs_x(hyper_parameters, tl, y_name)
  # Compute election period indicator
  hyper_parameters <- parse_election_period(hyper_parameters, tl)

  # Model configs
  mc <- model_config(model, hyper_parameters, spd$stan_data)
  spd$stan_data <- c(spd$stan_data, mc)

  # Set known values to array of size 1
  spd$stan_data$alpha_kappa_known <- array(spd$stan_data$alpha_kappa_known, dim=1)
  spd$stan_data$alpha_beta_mu_known <- array(spd$stan_data$alpha_beta_mu_known, dim=1)
  spd$stan_data$alpha_beta_sigma_known <- array(spd$stan_data$alpha_beta_sigma_known, dim=1)

  class(spd) <- c("model8l", "stan_polls_data")
  assert_all_periods_has_observations_in_polls_data(spd)
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8l <- function(x){
  assert_stan_data_model.model8i(x)
  checkmate::assert_integer(x$stan_data$election_period, lower = 0L, len = x$T)
  assert_model_arguments(x)
}


parse_election_period <- function(x, tl){
  mt <- max(tl$time_line$t)
  if(is.null(x$use_sigma_ep) || x$use_sigma_ep == 0L){
    if(!is.null(x$election_period)) stop("'election_period' is set, but not used.", call. = FALSE)
    x$use_sigma_ep <- 0L
    x$election_period <- rep(0L, mt)
    return(x)
  }
  if(x$use_sigma_ep > 0){
    if(is.null(x$election_period)) {
      stop("'election_period' is missing", call. = FALSE)
    }
    checkmate::assert_list(x$election_period, min.len = 1)
    election_time_points <- rep(0L, mt)
    for(i in seq_along(x$election_period)){
      x$election_period[[i]] <- as.Date(x$election_period[[i]])
      checkmate::assert_date(x$election_period[[i]], any.missing = FALSE, len = 2)
      checkmate::assert_true(x$election_period[[i]][1] <= x$election_period[[i]][2])
      tps <- get_time_points(tl, x$election_period[[i]])
      election_time_points[tps[1]:tps[2]] <- i
    }
    x$election_period <- election_time_points
    return(x)
  }
}


stan_polls_data_model8m <- function(x, y_name, time_scale = "week", known_state = NULL, model_time_range = NULL, latent_time_ranges = NULL, hyper_parameters = NULL, slow_scales = NULL, model){
  assert_polls_data(x)
  assert_y_name(y_name, x)
  assert_time_scale(time_scale)
  assert_known_state(known_state, null.ok = FALSE)
  assert_time_range(model_time_range, null.ok = TRUE)
  assert_latent_time_ranges(latent_time_ranges)
  assert_all_houses_has_observations_in_polls_data(x)

  if(is.null(model_time_range)) model_time_range <- time_range(x)
  if(is.null(latent_time_ranges)) latent_time_ranges <- setup_latent_time_ranges(x = latent_time_ranges, y = y_name, model_time_range)

  tl <- get_time_line(x, model_time_range, time_scale)
  ks <- known_state[dates_in_time_line(known_state$date, tl),]
  spd <- stan_polls_data_model6b(x, y_name, time_scale, ks, model_time_range, latent_time_ranges)

  spd$stan_data <- stan_data_add_model8km_common_fields(
    stan_data = spd$stan_data,
    x = x,
    time_line = tl,
    known_state = known_state,
    known_state_in_time_line = ks,
    time_scale = time_scale,
    slow_scales = slow_scales
  )

  spd$stan_data <- stan_data_finalize_model8m(
    stan_data = spd$stan_data,
    hyper_parameters = hyper_parameters,
    time_line = tl,
    y_name = y_name,
    model = model
  )

  class(spd) <- c("model8m", "stan_polls_data")
  assert_all_periods_has_observations_in_polls_data(spd)
  assert_stan_polls_data(spd)
  assert_stan_data_model(spd)
  spd
}

#' @rdname assert_stan_data_model
#' @export
assert_stan_data_model.model8m <- function(x){
  assert_stan_data_model.model8i(x)
  checkmate::assert_int(x$stan_data$EP, lower = 0L)
  checkmate::assert_true(x$stan_data$EP == max(x$stan_data$election_period))
  checkmate::assert_integer(x$stan_data$election_period, lower = 0L, upper = x$stan_data$EP,  len = x$T)
  checkmate::assert_list(x$stan_data$ep_inv_x, len = x$stan_data$EP)
  for(i in seq_along(x$stan_data$ep_inv_x)){
    checkmate::assert_numeric(x$stan_data$ep_inv_x[[i]], lower = 0, len = x$stan_data$P)
  }
  assert_model_arguments(x)
}
