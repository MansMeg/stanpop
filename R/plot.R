#' Plot a [polls_data] object
#'
#' @param x a [polls_data] object
#' @param y a variable name in the [polls_data] object.
#' @param publish_date visualize publication dates.
#' @param collection_period visualize collection period.
#' @param ... further arguments to [geom_collection_period]
#' @importFrom graphics plot
#' @export
plot.polls_data <- function(x, y = NULL, publish_date = TRUE, collection_period = TRUE, ...){
  if(is.null(y)) y <- names(y(x))[2]
  checkmate::assert_subset(y, choices = names(y(x))[-1])
  tr <- time_range(x)
  colnames(y(x)) <- make.names(colnames(y(x)))
  y <- make.names(y)
  plt <-
    ggplot2::ggplot()
  if(publish_date){
    plt <- plt + geom_publish_date(x, y, shape = 4, size = 0.5)
  }
  if(collection_period){
    plt <- plt + geom_collection_period(x, y, ...)
  }
  plt <-
    plt +
    ggplot2::xlim(tr["from"], tr["to"]) +
    ggplot2::xlab("")
  plt
}

#' @rdname plot.polls_data
#' @export
geom_publish_date <- function(x, y, ...){
  ggplot2::geom_point(data = as.data.frame(x),
                      ggplot2::aes_string(x = ".publish_date", y = y), ...)
}

#' @rdname plot.polls_data
#' @export
geom_collection_period <- function(x, y, ...){
  ggplot2::geom_segment(data = as.data.frame(x),
                        ggplot2::aes_string(x = ".start_date", xend = ".end_date", y = y, yend = y))
}

#' Add a [latent_state] geom to a ggplot
#'
#' @param x a [latent_state] object
#' @param median should the median be plotted?
#' @param intervals what intervals should be visualized
#' @param latent_state_colour the color of the latent state
#' @param ... further arguments to [geom_ribbon] and [geom_line].
#'
#' @export
geom_latent_state <- function(x, median = TRUE, intervals = c(0.90, 0.75, 0.5), latent_state_colour = "darkgrey", ...){
  checkmate::assert_class(x, "latent_state")
  checkmate::assert_flag(median)
  checkmate::assert_numeric(intervals, lower = 0, upper = 1)


  intervals <- intervals[order(intervals)]
  qs_low <- (1 - intervals)/2
  qs_high <- (1 - (1 - intervals)/2)
  if(median) qs_high <- c(0.5, qs_high)
  ps <- c(rev(qs_low), qs_high)
  psn <- make.names(ps)
  lsp <- latent_state_percentiles(x, percentiles = ps)

  interval_alpha <- 1/(length(intervals) + 1)

  P <- length(psn)
  geom <- list()
  for (i in seq_along(intervals)){
    geom[[i]] <- ggplot2::geom_ribbon(data = lsp, ggplot2::aes_string(x = "date", ymin = psn[i], ymax = psn[P - i + 1]), alpha = interval_alpha, fill = latent_state_colour, ...)
  }
  if(median){
    geom[[length(geom) + 1]] <- ggplot2::geom_line(data = lsp, ggplot2::aes_string(x = "date", y = "X0.5"), colour = latent_state_colour, ...)
  }
  return(geom)
}

#' Visualize a known state in a poll_of_polls ggplot
#' @param x a known_states data.frame
#' @param y the specific variable to visualize
#' @param ... further arguments to geom_line and geom_point
#' @export
geom_known_state <- function(x, y, ...){
  UseMethod("geom_known_state")
}

#' @export
geom_known_state.data.frame <- function(x, y, ...){
  geom_known_state_layers(known_state_overlay_data(x, y), ...)
}

#' @export
geom_known_state.poll_of_polls <- function(x, y, ...){
  geom_known_state_layers(known_state_overlay_data(x, y), ...)
}

#' Normalize known-state overlay data
#'
#' @param x A [poll_of_polls] object or explicit known-state data frame.
#' @param y Name of the value column to plot.
#'
#' @return A tibble with sorted `date` and `value` columns ready to be passed
#'   to [geom_known_state_layers()].
#' @keywords internal
known_state_overlay_data <- function(x, y) {
  checkmate::assert_string(y)

  if(inherits(x, "poll_of_polls")) {
    if(is.null(x$known_state)) {
      stop("poll_of_polls object does not contain known_state data.", call. = FALSE)
    }
    x <- x$known_state
  }

  checkmate::assert_data_frame(x)

  missing_columns <- setdiff(c("date", y), names(x))
  if(length(missing_columns) > 0L) {
    stop(
      "Known-state overlay data is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  checkmate::assert_date(x$date, any.missing = TRUE)

  overlay_data <- tibble::tibble(
    date = x$date,
    value = x[[y]]
  )
  overlay_data <- overlay_data[!is.na(overlay_data$date), , drop = FALSE]
  overlay_data <- overlay_data[order(overlay_data$date), , drop = FALSE]

  if(anyDuplicated(overlay_data$date) > 0L) {
    stop("Known-state overlay data must not contain duplicate dates.", call. = FALSE)
  }

  overlay_data
}

#' Build known-state overlay layers
#'
#' @param data Normalized overlay data returned by
#'   [known_state_overlay_data()].
#' @param ... Further arguments passed on to [ggplot2::geom_vline()] and
#'   [ggplot2::geom_point()].
#'
#' @return A list containing the vline and point layers used by
#'   [geom_known_state()].
#' @keywords internal
geom_known_state_layers <- function(data, ...) {
  list(
    ggplot2::geom_vline(
      data = data,
      ggplot2::aes(xintercept = date),
      lty = "dashed",
      ...
    ),
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(x = date, y = value),
      ...
    )
  )
}

#' Visualize a stan_data object
#' @seealso stan_polls_data
#' @param x a [stan_polls_data] object
#' @param ... further arguments supplied to [geom_polls_time_weights()].
#' @export
geom_stan_polls_data <- function(x, ...){
  checkmate::assert_class(x, "stan_polls_data")
  tws <- polls_time_weights(x)
  tws <- dplyr::left_join(tws, x$poll_ids, by = ".poll_id")
  y <- x$stan_data$y[tws$i]
  geom_polls_time_weights(tws, y, ...)
}


#' @rdname polls_time_weights
#' @param y a vector to plot for the [polls_time_weights] object.
#' @param ... further arguments to [geom_point]
#' @export
geom_polls_time_weights <- function(x, y, ...){
  checkmate::assert_class(x, "polls_time_weights")
  checkmate::assert_numeric(y, len = nrow(x))
  df <- dplyr::bind_cols(x, tibble::tibble(y = y))
  ggplot2::geom_point(data = df, ggplot2::aes(x=date, y = y), ...)
}


#' Plot a [poll_of_polls] object
#'
#' @param x a [poll_of_polls] object
#' @param y column to visualize.
#' @param from plot from [date]
#' @param to plot to [date]
#' @inheritParams plot.polls_data
#'
#' @param ... further arguments to [geom_latent_state]
#' @param shift_latent_days Shift the latent series right, this number of days.
#' @param house only plot the following polling houses
#' @param include_latent_state include latent state in plot
#'
#' @export
plot_poll_of_polls <- function(x, y = NULL, from = NULL, to = NULL, publish_date = TRUE, collection_period = FALSE, shift_latent_days = 0, house = NULL, include_latent_state = TRUE, ...){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_flag(publish_date)
  checkmate::assert_flag(collection_period)
  checkmate::assert_flag(include_latent_state)

  if(is.null(y)) y <- x$y[1]
  checkmate::assert_choice(y, x$y, null.ok = TRUE)

  ls <- latent_state(x, time_line = x$time_line)
  if(!is.null(y)) ls <- ls[, ,y]
  ls <- subset_latent_state_dates(x = ls, from, to)
  ls$time_line$time_line$date <- ls$time_line$time_line$date + lubridate::days(shift_latent_days)

  pd <- x$polls_data
  if(!is.null(house)){
    checkmate::assert_subset(house, levels(x$polls_data$poll_info$.house))
    pd <- subset(x$polls_data, subset = houses(x$polls_data) %in% house)
  }
  pd <- subset_dates(x = pd, from, to)

  ft <- parse_from_to(from, to,
                      default = time_range(ls$time_line),
                      limits = time_range(ls$time_line))

  x$known_state <- x$known_state[x$known_state$date >= ft["from"] & x$known_state$date <= ft["to"], c("date", y)]

  plt <- plot.polls_data(x = pd, y = y, publish_date = publish_date, collection_period = FALSE)
  if(collection_period & nrow(y(pd)) > 0){
    tws <- polls_time_weights(pd)
    tws <- summarize_polls_time_weights(tws, x$time_line)
    poll_ids <- tibble::tibble(.poll_id = poll_ids(pd), i = seq_along(poll_ids(pd)))
    tws <- dplyr::left_join(tws, poll_ids, by = ".poll_id")
    ypd <- y(pd)[[y]]
    plt <- plt + geom_polls_time_weights(tws, ypd[tws$i], size = 0.3, alpha = 0.5)
  }
  if(include_latent_state){
    plt <- plt + geom_latent_state(x = ls, ...)
  }
  plt
}

#' @rdname plot_poll_of_polls
#' @export
plot.poll_of_polls <- function(x, ...){
  plot_poll_of_polls(x, ...)
}

#' @rdname plot_poll_of_polls
#' @param ... further arguments to [geom_line]
#' @export
geom_pop_line <- function(x, y, ...){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_numeric(x = y, len = length(x$time_line$time_line$date))
  ggplot2::geom_line(data = data.frame(date = x$time_line$time_line$date,
                                       y = y),
                     ggplot2::aes(x=date, y = y), ...)
}





#' Plot posterior distributions of parameters
#'
#' @param x a [poll_of_polls] object
#' @param params parameters to plot (see \code{parameter_names()} too see all parameters in model)
#' @param params_plot_name names to use for parameters in plot
#' @param pars optional parameter names to include in the traceplot.
#' @param inc_warmup should warmup draws be included when constructing the
#'   traceplot input?
#' @param ... further arguments sent to bayesplot::mcmc_areas, bayesplot::mcmc_hex, etc.
#' @param title The title of the plot.
#'
#' @export
plot_parameters_areas <- function(x, params, params_plot_name = NULL, title = "Posterior distributions", ...){
  checkmate::assert_string(title)
  plot_title <- ggplot2::ggtitle(title)
  plt <- plot_parameters_bayesplot(x, bayesplot::mcmc_areas, params, params_plot_name, ...)
  plt <- plt + plot_title
  plt
}

#' @rdname plot_parameters_areas
#' @export
plot_parameters_mcmc_hex <- function(x, params, params_plot_name = NULL, title = "Posterior distributions", ...){
  checkmate::assert_string(title)
  plot_title <- ggplot2::ggtitle(title)
  plt <- plot_parameters_bayesplot(x, bayesplot::mcmc_hex, params, params_plot_name, ...)
  plt <- plt + plot_title
  plt
}

#' @rdname plot_parameters_areas
#' @export
plot_parameters_mcmc_scatter <- function(x, params, params_plot_name = NULL, title = "Posterior distributions", ...){
  checkmate::assert_string(title)
  plot_title <- ggplot2::ggtitle(title)
  plt <- plot_parameters_bayesplot(x, bayesplot::mcmc_scatter, params, params_plot_name, ...)
  plt <- plt + plot_title
  plt
}

#' @rdname plot_parameters_areas
#' @export
plot_parameters_mcmc_intervals <- function(x, params, params_plot_name = NULL, title = "Posterior distributions", ...){
  checkmate::assert_string(title)
  plot_title <- ggplot2::ggtitle(title)
  plt <- plot_parameters_bayesplot(x, bayesplot::mcmc_intervals, params, params_plot_name, ...)
  plt <- plt + plot_title
  plt
}

#' Plot posterior distributions of parameters
#'
#' @param x a [poll_of_polls] object
#' @param bayeplot_FUN a plotting function from the basyeplot package
#' @param params parameters to plot (see \code{parameter_names()} too see all parameters in model)
#' @param params_plot_name names to use for parameters in plot
#' @param ... further arguments sent to bayesplot::mcmc_area
#'
plot_parameters_bayesplot <- function(x, bayeplot_FUN, params, params_plot_name = NULL, ...){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_character(params, any.missing = FALSE, unique = TRUE)
  checkmate::assert_subset(params, parameter_names(x))
  checkmate::assert_character(params_plot_name, any.missing = FALSE, unique = TRUE, len = length(params), null.ok = TRUE)

  if(is.null(params_plot_name)) params_plot_name <- params

  post_array <- pop_draws_array(x, variables = params)
  post_matrix <- as.matrix(posterior::as_draws_matrix(post_array))
  colnames(post_matrix) <- params_plot_name
  plt <- suppressWarnings(
    bayeplot_FUN(post_matrix, ...))
  plt
}


#' @rdname plot_parameters_areas
#' @importFrom graphics pairs
#' @export
pairs.poll_of_polls <- function(x, ...){
  pairs(x$stan_fit, ...)
}

#' @rdname plot_parameters_areas
#' @export
traceplot <- function(x, ...){
  UseMethod("traceplot")
}

#' @rdname plot_parameters_areas
#' @export
traceplot.poll_of_polls <- function(x, pars = NULL, inc_warmup = FALSE, ...){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_character(pars, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
  checkmate::assert_flag(inc_warmup)
  if(!is.null(pars)) {
    checkmate::assert_subset(pars, parameter_names(x))
  }

  bayesplot::mcmc_trace(
    pop_draws_array(x, variables = pars, inc_warmup = inc_warmup),
    ...
  )
}
