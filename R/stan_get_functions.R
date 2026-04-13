#' Stan Function for pop objects
#'
#' @param object an object
#' @param ... arguments supplied to the rstan function
#'
#' @export
log_prob <- function(object, ...){
  UseMethod("log_prob")
}
#' @rdname log_prob
#' @export
log_prob.poll_of_polls <- function(object, ...){
  backend_log_prob(object$backend, object$stan_fit, ...)
}
#' @rdname log_prob
#' @export
log_prob.default <- function(object, ...){
  rstan::log_prob(object, ...)
}

#' @rdname log_prob
#' @export
get_num_upars <- function(object, ...){
  UseMethod("get_num_upars")
}
#' @rdname log_prob
#' @export
get_num_upars.poll_of_polls <- function(object, ...){
  if("warm_start_state" %in% names(object) &&
     is.list(object$warm_start_state) &&
     "num_upars" %in% names(object$warm_start_state) &&
     !is.null(object$warm_start_state$num_upars)) {
    return(object$warm_start_state$num_upars)
  }
  nu <- try(backend_get_num_upars(object$backend, object$stan_fit, ...), silent = TRUE)
  if(inherits(nu, "try-error")){
    nu <- length(get_adaptation_info(object)[[1]]$diag_inv_mass_matrix)
  }
  nu
}

#' @rdname log_prob
#' @export
get_num_upars.default <- function(object, ...){
  rstan::get_num_upars(object, ...)
}

#' @rdname log_prob
#' @export
get_num_pars <- function(object){
  suppressWarnings(length(parameter_names(object)))
}

#' Get Sampler Diagnostics
#'
#' @param object a [poll_of_polls] object to extract stan samples from
#' @param ... arguments further supplied to [rstan::get_sampler_params()]
#' @seealso rstan::get_sampler_params
#'
#' @export
get_sampler_params <- function(object, ...){
  UseMethod("get_sampler_params")
}


#' @rdname get_sampler_params
#' @export
get_sampler_params.poll_of_polls <- function(object, ...){
  backend_get_sampler_params(object$backend, object$stan_fit, ...)
}


#' Get Stan Code from a pop object
#'
#' @param object a [poll_of_polls] object to extract stan code from
#' @param ... arguments further supplied to [rstan::get_stancode()]
#' @seealso rstan::get_stancode
#'
#' @export
get_stancode <- function(object, ...){
  UseMethod("get_stancode")
}


#' @rdname get_stancode
#' @export
get_stancode.poll_of_polls <- function(object, ...){
  checkmate::assert_class(object, "poll_of_polls")

  if("stan_code" %in% names(object) && !is.null(object$stan_code)) {
    return(object$stan_code)
  }

  if(identical(object$backend, "rstan")) {
    return(rstan::get_stancode(object$stan_fit, ...))
  }
  if(identical(object$backend, "cmdstanr")) {
    runset <- try(object$stan_fit$runset, silent = TRUE)
    if(!inherits(runset, "try-error") && !is.null(runset)) {
      stan_code <- try(runset$stan_code(), silent = TRUE)
      if(!inherits(stan_code, "try-error") && length(stan_code) > 0L) {
        return(paste(stan_code, collapse = "\n"))
      }
    }

    metadata <- try(object$stan_fit$metadata(), silent = TRUE)
    if(!inherits(metadata, "try-error") &&
       !is.null(metadata$stan_file) &&
       checkmate::test_file_exists(metadata$stan_file, extension = "stan")) {
      return(paste(readLines(metadata$stan_file, warn = FALSE), collapse = "\n"))
    }
  }

  stop("Stan code is not available on this poll_of_polls object.", call. = FALSE)
}
