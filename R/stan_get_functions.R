#' Stan Function for pop objects
#'
#' @param object an object
#' @param ... arguments supplied to the rstan function
#'
#' @export
log_prob <- function(object, ...){
  NextMethod("log_prob")
}
#' @rdname log_prob
#' @export
log_prob.poll_of_polls <- function(object, ...){
  rstan::log_prob(object$stan_fit, ...)
}
#' @rdname log_prob
#' @export
log_prob.default <- function(object, ...){
  rstan::log_prob(object, ...)
}

#' @rdname log_prob
#' @export
get_num_upars <- function(object, ...){
  NextMethod("get_num_upars")
}
#' @rdname log_prob
#' @export
get_num_upars.poll_of_polls <- function(object, ...){
  nu <- try(rstan::get_num_upars(object$stan_fit, ...), silent = TRUE)
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
  rstan::get_sampler_params(object$stan_fit, ...)
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
  rstan::get_stancode(object$stan_fit, ...)
}
