#' Re-estimate a poll_off_polls stan model with new stan arguments
#'
#' @param x a poll_of_polls object
#' @param ... argument to supply to rstan::stan
#'
#' @export
rerun_poll_of_polls <- function(x, ...){
  checkmate::assert_class(x, "poll_of_polls")

  new_args <- list(...)
  rstan_arguments <- x$stan_arguments
  for(i in seq_along(new_args)){
    rstan_arguments[[names(new_args)[i]]] <- new_args[[i]]
  }

  if(!is.null(rstan_arguments$data)) warning("The 'data' argument has been overwritten")
  rstan_arguments$data <- x$stan_data$stan_data
  if(is.null(rstan_arguments$model_code)) rstan_arguments$model_code = x$stan_fit@stanmodel@model_code
  if(is.null(rstan_arguments$pars)) rstan_arguments$pars <- adapop:::stan_parameters_to_store(x$model)


  # Run Stan
  stan_fit <- do.call(rstan::stan, rstan_arguments)
  pop$stan_fit <- stan_fit
  return(pop)
}

#' @rdname rerun_poll_of_polls
#' @export
reestimate_poll_of_polls <- rerun_poll_of_polls
