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
  rstan::extract(object$stan_fit, ...)
}

#' Extract the data when Stan was run
#'
#' @param object a [poll_of_polls] object
#'
#' @export
get_stan_date <- function(object){
  checkmate::assert_class(object, "poll_of_polls")
  lubridate::parse_date_time(substr(object$stan_fit@date,5,nchar(object$stan_fit@date)), orders = "%b %d %H:%M:%S %Y", tz = Sys.timezone())
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

