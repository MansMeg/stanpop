#' Supported Stan backends
#'
#' @keywords internal
supported_pop_backends <- function() {
  c("rstan", "cmdstanr")
}

#' Assert supported backend value
#'
#' @keywords internal
assert_pop_backend <- function(backend){
  checkmate::assert_choice(backend, choices = supported_pop_backends())
}

#' Run a Stan fit using the selected backend
#'
#' @keywords internal
backend_sample <- function(backend, stan_arguments){
  checkmate::assert_list(stan_arguments)
  assert_pop_backend(backend)
  if(backend == "rstan"){
    return(backend_sample_rstan(stan_arguments))
  }
  if(backend == "cmdstanr"){
    return(backend_sample_cmdstanr(stan_arguments))
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' @keywords internal
backend_sample_rstan <- function(stan_arguments){
  do.call(rstan::stan, stan_arguments)
}

#' @keywords internal
backend_sample_cmdstanr <- function(stan_arguments){
  stop(
    "Backend 'cmdstanr' is not yet implemented in 'poll_of_polls()'. ",
    "Use backend = 'rstan' for now.",
    call. = FALSE
  )
}
