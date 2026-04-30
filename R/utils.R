month_abbr_en <- function() c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#' Return a data.frame with dates from a list of pop objects
#'
#' @param x a list of pop objects
#'
model_dates_data_frame <- function(x){
  checkmate::assert_list(x)
  for(i in seq_along(x)){
    checkmate::assert_class(x[[i]], "poll_of_polls")
    checkmate::assert_subset(x[[i]]$y, x[[1]]$y)
  }
  trk <-  function(x){
    ds <- x$known_state$date
    if(is.null(ds)){
      c(from = as.Date(NA), to = as.Date(NA))
    } else {
      c(from = ds[which.min(ds)], to = ds[which.max(ds)])
    }
  }

  dat <- data.frame(model_no = 1:length(x))
  dat$model_start_date <- unname(do.call("c", lapply(x, function(x) time_range(x$time_line)["from"])))
  dat$model_end_date <- unname(do.call("c", lapply(x, function(x) time_range(x$time_line)["to"])))
  dat$polls_start_date <- unname(do.call("c", lapply(x, function(x) time_range(x$polls_data)["from"])))
  dat$polls_end_date <- unname(do.call("c", lapply(x, function(x) time_range(x$polls_data)["to"])))
  dat$known_state_start_date <- unname(do.call("c", lapply(x, function(x) trk(x)["from"])))
  dat$known_state_end_date <- unname(do.call("c", lapply(x, function(x) trk(x)["to"])))
  dat
}

#' Remove file extension
#'
#' @param x a string with a file name
#'
#' @export
remove_file_extension <- function(x){
  checkmate::assert_string(x)
  splt <- strsplit(x, "\\.")[[1]]
  substr(x, 1, nchar(x) - nchar(splt[length(splt)]) - 1)
}


#' Extract parameters from the stan object
#'
#' @param object an object to extract parameter draws from
#' @param ... arguments supplied to rstan::extract.
#' @export
extract <- function(object, ...){
  UseMethod("extract")
}

#' @rdname extract
#' @export
extract.poll_of_polls <- function(object, ...){
  backend_extract(object$backend, object$stan_fit, ...)
}

#' Return posterior draws as a draws_array
#'
#' @keywords internal
pop_draws_array <- function(object, variables = NULL, inc_warmup = FALSE) {
  checkmate::assert_class(object, "poll_of_polls")

  backend_draws_array(
    backend = object$backend,
    fit = object$stan_fit,
    variables = variables,
    inc_warmup = inc_warmup
  )
}

#' Extract the date when Stan was run
#'
#' @param object a [poll_of_polls] object
#'
#' @export
get_stan_date <- function(object){
  checkmate::assert_class(object, "poll_of_polls")

  if("stan_date" %in% names(object) && inherits(object$stan_date, "POSIXt")) {
    return(object$stan_date)
  }

  if(identical(object$backend, "rstan")) {
    return(lubridate::parse_date_time(
      substr(object$stan_fit@date, 5, nchar(object$stan_fit@date)),
      orders = "%b %d %H:%M:%S %Y",
      tz = Sys.timezone()
    ))
  }

  if(identical(object$backend, "cmdstanr")) {
    output_files <- try(object$stan_fit$output_files(), silent = TRUE)
    if(!inherits(output_files, "try-error") &&
       length(output_files) > 0L &&
       all(file.exists(output_files))) {
      mtimes <- file.info(output_files)$mtime
      mtimes <- mtimes[!is.na(mtimes)]
      if(length(mtimes) > 0L) {
        return(max(mtimes))
      }
    }
  }

  stop("Stan run date is not available on this poll_of_polls object.", call. = FALSE)
}

#' Extract the data when Stan was run
#'
#' @param x a numeric vector of loged values
#'
#' @export
logMeanExp <- function(x) {
  logS <- log(length(x))
  matrixStats::logSumExp(x) - logS
}



#' Return parties existing in a given model range
#'
#' @param y a character vector of party names
#' @param ltr the [time_range] of the latent series
#' @param mtr the [time_range] of the model
#'
#' @export
existing_parties <- function(y, ltr, mtr){
  assert_latent_time_range_list(ltr)
  in_mtr <- !logical(length(y))
  for(i in seq_along(y)){
    if(is.null(ltr[[y[i]]])) next
    tests <- c(mtr["from"] > ltr[[y[i]]]$to, mtr["to"] < ltr[[y[i]]]$from)
    if(any(tests)) {
      in_mtr[i] <- FALSE
      message("Category '", y[i], "' is exluded. The latent time range is not included in the model time range (", paste0(mtr, collapse = "--"), ").")
    }
  }
  y[in_mtr]
}
