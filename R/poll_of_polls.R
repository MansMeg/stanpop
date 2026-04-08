#' Fit a poll of polls object
#'
#' @param y character vector indicating poll variable to use
#' @param model poll of polls model to use (or a path to a stan model)
#' @param polls_data a [polls_data] object
#' @param time_scale the time scale to use, [day], [week], or [month].
#' @param time_scale_overrides a [data.frame] with columns [from], [to], and [time_scale]
#'   that override the default [time_scale] for inclusive date ranges in the latent state.
#'   The [from] and [to] bounds are inclusive and override ranges must not overlap.
#' @param model_time_range a [time_range] object that describe the period used for the latent state. Default is the [time_range] of the [polls_data] object.
#' @param known_state a [data.frame] with variables [date] and [x] indicating the known states for certain dates.
#' @param latent_time_ranges time ranges of the latent state of individual [y]s
#' @param hyper_parameters hyperparameters to supply direct to the model
#' @param slow_scales a vector of [Date]s that indicate breaks (right-inclusive) for a slower moving time scale.
#' @param backend Stan backend to use. Supported values are [rstan] and
#'   [cmdstanr].
#' @param compile_args additional arguments passed to [cmdstanr::cmdstan_model()]
#'   when `backend = "cmdstanr"`. Ignored for `backend = "rstan"`.
#' @param ... further arguments passed directly to the backend sampler.
#'   These go to [rstan::stan()] when `backend = "rstan"` and to
#'   `CmdStanModel$sample()` when `backend = "cmdstanr"`.
#' @param cache_dir directory to cache model. Default is cache in tempdir(). [NULL], no cache.
#'
#' @details
#' The [input_args] slot contain all input arguments except polls data and known state that are stored in the original object instead.
#' The [stan_arguments] slot contains the backend sampling arguments supplied through `...`,
#' except for the `data` argument which is stored in the [stan_data] slot.
#' The [compile_arguments] slot contains the CmdStanR compilation arguments.
#'
#'
#' @export
poll_of_polls <- function(y,
                          model,
                          polls_data,
                          time_scale,
                          time_scale_overrides = NULL,
                          known_state = NULL,
                          model_time_range = NULL,
                          latent_time_ranges = NULL,
                          hyper_parameters = NULL,
                          slow_scales = NULL,
                          backend = "rstan",
                          compile_args = NULL,
                          ...,
                          cache_dir = file.path(tempdir(), "pop_cache")){
  checkmate::assert_subset(x = y, choices = names(y(polls_data)))
  if(checkmate::test_file_exists(model, extension = "stan")){
    smfp <- model
    model <- remove_file_extension(basename(model))
  } else {
    smfp <- get_pop_stan_model_file_path(model)
  }
  checkmate::assert_choice(model, choices = supported_pop_models())
  assert_pop_backend(backend)
  checkmate::assert_list(compile_args, null.ok = TRUE)
  assert_polls_data(polls_data, min.rows = 1, min.cols = 1)
  assert_known_state(known_state)
  if(!is.null(known_state)){
    checkmate::assert_names(x = names(known_state), must.include = c("date", y))
  }
  checkmate::assert_choice(time_scale, choices = supported_time_scales())
  if(is.null(model_time_range)) {
    mtr <- time_range(polls_data)
  } else {
    mtr <- time_range(model_time_range)
  }
  assert_poll_data_in_time_line_using_time_range(polls_data, time_scale, mtr)
  assert_time_scale_overrides(
    x = time_scale_overrides,
    dates = tibble::tibble(date = seq(from = mtr["from"], to = mtr["to"], by = 1))
  )
  if(!is.null(time_scale_overrides) && nrow(time_scale_overrides) > 0 &&
     !model_supports_time_scale_overrides(model)) {
    stop(
      "'time_scale_overrides' requires a model that uses 'step_scale_t'. ",
      "Model '", model, "' does not support time scale overrides in fitting.",
      call. = FALSE
    )
  }
  assert_latent_time_range_list(latent_time_ranges)
  ltr <- setup_latent_time_ranges(x = latent_time_ranges, y, mtr)
  assert_poll_data_and_latent_time_range_list_agree(polls_data, ltr)
  checkmate::assert_list(hyper_parameters, null.ok = TRUE)

  sd <- stan_polls_data(x = polls_data,
                        time_scale = time_scale,
                        time_scale_overrides = time_scale_overrides,
                        y_name = y,
                        model = model,
                        known_state = known_state,
                        model_time_range = mtr,
                        latent_time_ranges = ltr,
                        hyper_parameters = hyper_parameters,
                        slow_scales = slow_scales)


  if(!is.null(cache_dir)) {
    if(!checkmate::test_directory(cache_dir)) dir.create(cache_dir)
    checkmate::assert_directory(cache_dir)
  }

  # SHA is setup both of all
  fun_args <- names(formals(poll_of_polls))[-which(names(formals(poll_of_polls)) %in% c("...", "cache_dir"))]
  sha_fun_args <- list(y = y,
                       model = readLines(smfp),
                       polls_data = polls_data,
                       backend = backend,
                       compile_args = compile_args,
                       time_scale = time_scale,
                       time_scale_overrides = time_scale_overrides,
                       known_state = known_state,
                       model_time_range = mtr,
                       latent_time_ranges = ltr,
                       hyper_parameters = hyper_parameters,
                       slow_scales = slow_scales)
  checkmate::assert_set_equal(fun_args, names(sha_fun_args))
  sha_stan_args <- list(data = sd$stan_data, ...)
  sha <- digest::digest(c(sha_fun_args, sha_stan_args), algo = "sha1")

  # Read from cache
  if(!is.null(cache_dir)){
    cache_fp <- cache_file_path(sha, cache_dir)
    if(file.exists(cache_fp)){
      pop <- readRDS(file = cache_fp)
      message("Cached results used.")
      return(pop)
    } else {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Setup Stan backend arguments
  backend_arguments <- list(...)
  stan_arguments <- list(...)
  if(!is.null(backend_arguments$data)) warning("The 'data' argument has been overwritten")
  backend_arguments$data <- sd$stan_data
  if(backend == "cmdstanr"){
    cmdstanr_rstan_only_args <- c("file", "model_name", "control", "iter", "warmup",
                                  "cores", "algorithm", "init_r")
    found_rstan_only_args <- intersect(names(backend_arguments), cmdstanr_rstan_only_args)
    if(length(found_rstan_only_args) > 0){
      stop(
        "With backend = 'cmdstanr', supply CmdStanR sample arguments directly in '...'. ",
        "Unsupported RStan-style arguments: ",
        paste0(found_rstan_only_args, collapse = ", "),
        ". Use e.g. 'iter_warmup', 'iter_sampling', 'parallel_chains', and 'compile_args'.",
        call. = FALSE
      )
    }
  }
  if(backend == "rstan" && !is.null(compile_args) && length(compile_args) > 0){
    warning("'compile_args' is ignored when backend = 'rstan'.", call. = FALSE)
  }
  # The parameters to store should be supplied as an argument to stan instead.
  # if(is.null(backend_arguments$pars)) backend_arguments$pars <- stan_parameters_to_store(model)
  # TODO: rm stan_parameters_to_store() and just use the pars argument supplied by the user

  # Run Stan
  stan_fit <- backend_sample(
    backend = backend,
    sample_arguments = backend_arguments,
    stan_file = smfp,
    model_name = model,
    compile_arguments = compile_args
  )

  pop <-  list(y = y,
               model = model,
               backend = backend,
               compile_arguments = compile_args,
               polls_data = polls_data,
               time_scale = time_scale,
               time_scale_overrides = time_scale_overrides,
               known_state = known_state,
               model_time_range = mtr,
               latent_time_range = ltr,
               stan_arguments = stan_arguments,
               sha = sha,
               input_args = list(y = y,
                                 model = model,
                                 backend = backend,
                                 compile_args = compile_args,
                                 time_scale = time_scale,
                                 time_scale_overrides = time_scale_overrides,
                                 model_time_range = model_time_range,
                                 latent_time_ranges = latent_time_ranges,
                                 hyper_parameters = hyper_parameters,
                                 slow_scales = slow_scales,
                                 stan_arguments = stan_arguments,
                                 cache_dir = cache_dir
                                 ),
               git_sha = get_git_sha(),
               cache_dir = cache_dir,
               time_line = sd$time_line,
               stan_fit = stan_fit,
               diagnostics = compute_diagnostics(stan_fit, backend = backend),
               model_arguments = hyper_parameters,
               stan_data = sd)
  class(pop) <- c(paste0("pop_", model), "poll_of_polls")

  assert_pop(pop)
  # Save to cache
  if(!is.null(cache_dir)) saveRDS(pop, file = cache_fp)
  pop
}

assert_pop <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
}

assert_known_state <- function(x, null.ok = TRUE){
  if(is.null(x)) {
    if(null.ok){
      return(invisible(TRUE))
    } else {
      stop("known_state is NULL", call. = FALSE)
    }
  }
  checkmate::assert_data_frame(x, min.rows = 1)
  checkmate::assert_names(colnames(x), must.include = c("date"))
}

cache_file_path <- function(sha, cache_dir){
  file.path(cache_dir, paste0(sha, ".rds"))
}


#' Parameters we want to store, by model
#'
#' @details
#' The function returns the parameter names of the parameters that are needed
#' for analysis or in computing predictive distributions.
#' Hence these parameters will be the only ones that will be stored
#' when running Stan.
#'
#' @param model a model name to get the parameters to store from.
#'
#' @keywords internal
stan_parameters_to_store <- function(model){
  checkmate::assert_choice(model, choices = supported_pop_models())
  if(grepl(model, pattern = "^model8[jk][0-9]+$")){
    return(c("x_pred", "sigma_x", "lp__", "eta",
             "kappa_pred", "sigma_kappa", "sigma_xc",
             "beta_mu", "sigma_beta_mu",
             "beta_sigma", "sigma_beta_sigma",
             "alpha_kappa", "alpha_beta_mu", "alpha_beta_sigma",
             "nu_kappa", "v_kappa",
             "psi", "sigma_psi",
             "alpha_V","theta_x","ar_V", "V",
             # additional parameters used ro make the results full for analysis
             "x_unknown", "eta_z_unknown", "kappa_raw",
             "alpha_kappa_unknown", "alpha_beta_mu_unknown",
             "alpha_beta_sigma_unknown", "nu_kappa_raw", "V_noise", "L_Omega_x"))
  } else if(grepl(model, pattern = "^model8[lm][0-9]+$")){
    return(c("x_pred", "sigma_x", "lp__", "eta",
             "kappa_pred", "sigma_kappa", "sigma_xc",
             "beta_mu", "sigma_beta_mu",
             "beta_sigma", "sigma_beta_sigma",
             "alpha_kappa", "alpha_beta_mu", "alpha_beta_sigma",
             "nu_kappa", "v_kappa",
             "psi", "sigma_psi",
             "alpha_V","theta_x","ar_V", "V", "sigma_ep"))
  } else {
    stop("'", model, "' not implemented in stan_parameters_to_store().")
  }
}

#' Parameters that are not state parameters, by model
#'
#' @details
#' The function returns the parameter names of the parameters
#' that are not state parameters and hence can be visualized.
#'
#' @param model a model name to get non-state parameters from.
#'
#' @keywords internal
stan_non_state_parameters <- function(model){
  checkmate::assert_choice(model, choices = supported_pop_models())
  if(grepl(model, pattern = "^model8[jk][0-9]+$")){
    stop("'", model, "' not implemented in stan_non_state_parameters().")
    # return(c("sigma_x"))
  } else if(grepl(model, pattern = "^model8[lm][0-9]+$")){
    stop("'", model, "' not implemented in stan_non_state_parameters().")
  } else {
    stop("'", model, "' not implemented in stan_non_state_parameters().")
  }
}

supported_pop_models <- function() {
  # When updating a model, the following parts needs to also be fixed:
  # a. Update stan_polls_data() with how the input data looks like
  # b. Update stan_parameters_to_store() with info on what parameters should be stored and used
  # c. Update stan_non_state_parameters() with info on what parameters should be stored and used
  # d. Update latent_state.stanfit() with info on how the latent state is extracted
  # e. Update compute_prediction_error()
  c(paste0("model8k", 1:9),
    paste0("model8m", 1:9))
}

get_pop_stan_model_file_path <-function(model){
  checkmate::assert_choice(model, supported_pop_models())
  fp <- file.path(system.file(package = "stanpop"), "stan_models", paste0(model, ".stan"))
  checkmate::assert_file_exists(fp)
  fp
}

#' @export
print.poll_of_polls <- function(x, ...){
  ms <- utils::capture.output(print(utils::object.size(x), units = "auto", standard = "SI"))
  cat("==== Poll of Polls Model (",ms,") ==== \n", sep = "")
  tr <- time_range(x$time_line)
  cat("Model is fit during the period ", as.character(tr["from"]), "--", as.character(tr["to"]), "\n", sep = "")
  cat("Stan model: ", x$model, ".stan\n", sep = "")
  cat("Backend: ", x$backend, "\n", sep = "")
  cat("Number of parameters:", get_num_pars(x), "\n")
  cat("Number of unconstrained parameters:", get_num_upars(x), "\n")
  cat("Parties:", paste0(x$y, collapse = ", "), "\n")
  cat("Time scale:", x$time_scale, "\n")


  cat("\n== Data == \n")
  polls_data_summary(x$polls_data)
  cat("\n")
  known_state_summary(x$known_state)

  cat("\n== Stan arguments == \n")
  cat(yaml::as.yaml(x$stan_arguments))

  cat("\n== Model arguments == \n")
  x$model_arguments$election_period <- format_election_period_to_string(x, period_marker = "--")
  x$model_arguments$obs_x <- format_obs_x_to_string(x)
  cat(yaml::as.yaml(x$model_arguments))

  cat("\n== Model diagnostics == \n")
  out <- utils::capture.output(md <- try(get_model_diagnostics(x), silent = TRUE))
  if(inherits(md, what = "try-error")){
    cat(out, "\n")
  } else {
    # Round digits to zero (and make ints)
    zero <- c("no_divergent_transistions", "no_max_treedepth", "no_low_bfmi_chains", "no_Rhat_above_1_1", "no_Rhat_is_NA", "mean_chain_warmup_time", "mean_chain_sampling_time")
    md[zero] <- lapply(lapply(md[zero], round), as.integer)
    # Round digits to one (and make ints)
    one <- c("mean_no_leapfrog_steps")
    md[one] <- lapply(lapply(md[one], round, digits = 1), as.integer)
    cat(yaml::as.yaml(md))
  }

  if(!is.null(x$git_sha)){
    cat("\n== Git == \n")
    cat("git sha:", x$git_sha, "\n")
  }

  if(!is.null(x$cache_dir)){
    cat("\n== Cache == \n")
    cat("sha:", x$sha, "\n")
    cat("cache directory:", x$cache_dir, "\n")
  }

}

#' Format election period lists to string for printout
#'
#' @param x a poll of polls object
#' @param period_marker character used to combine election period dates to an election period
#' @param collapse should the election periods be collapsed to a character of length one (NULL if not),
#' then the string used to collapse the character vector elements.
#'
format_election_period_to_string <- function(x, period_marker = "--", collapse = NULL){
  checkmate::assert_class(x = x, classes = "poll_of_polls")
  checkmate::assert_string(period_marker)
  checkmate::assert_string(collapse, null.ok = TRUE)

  if(is.null(x$model_arguments[["election_period"]])) return(NULL)
  res <- unlist(lapply(x$model_arguments[["election_period"]], FUN = function(y) paste(y, collapse = period_marker)))
  if(!is.null(collapse)){
    res <- paste(res, collapse = collapse)
  }
  res
}

#' Format obs_x lists to string for printout
#'
#' @param x a poll of polls object
#' @param collapse should the obs_x characters be collapsed to a character of length one (NULL if not),
#' then the string used to collapse the character vector elements.
#'
format_obs_x_to_string <- function(x, collapse = NULL){
  if(is.null(x$model_arguments[["obs_x"]])) return(NULL)
  tbl <- x$model_arguments[["obs_x"]]
  tbl$y <- as.character(tbl$y)
  tbl$date <- as.character(tbl$date)
  lst <- split(tbl, 1:nrow(tbl))
  for(i in seq_along(lst)){
    res <- unlist(lst[[i]])
    res <- paste(paste0(names(res), "=", res), collapse = ", ")
    lst[[i]] <- unname(res)
  }
  res <- unname(unlist(lst))
  if(!is.null(collapse)){
    res <- paste(res, collapse = collapse)
  }
  res
}

format_ep_inv_x_to_string <- function(x, collapse = NULL){
  if(is.null(x$model_arguments[["ep_inv_x"]])) return(NULL)
  lst <- x$model_arguments[["ep_inv_x"]]
  for(i in seq_along(lst)){
    lst[[i]] <- paste0(lst[[i]],collapse =  ", ")
  }
  res <- unname(unlist(lst))
  if(!is.null(collapse)){
    res <- paste(res, collapse = collapse)
  }
  res
}


assert_poll_data_and_latent_time_range_list_agree <- function(x, ltr){
  checkmate::assert_class(x, "polls_data")
  assert_latent_time_ranges(ltr)
  ys <- y(x)
  sd <- start_date(x)
  ed <- end_date(x)
  yn <- names(ltr)
  for(i in seq_along(ltr)){
    pre_polls <- ltr[[yn[i]]]["from"] > sd
    if(any(pre_polls)){
      value <- ys[[yn[i]]][pre_polls]
      is_incorrect <- !(is.na(value) | value == 0)
      if(any(is_incorrect)){
        pre_polls[pre_polls] <- is_incorrect
        stop("Polls has values for category '", yn[i],
             "' that should be NA before '",
             ltr[[yn[i]]]["from"], "':\n",
             paste0(poll_ids(x)[pre_polls], collapse = ", "), call. = FALSE)
      }
    }
    post_polls <- ltr[[yn[i]]]["to"] < ed
    if(any(post_polls)){
      value <- ys[[yn[i]]][post_polls]
      is_incorrect <- !(is.na(value) | value == 0)
      if(any(is_incorrect)){
        post_polls[post_polls] <- is_incorrect
        stop("Polls has values for category '", yn[i],
             "' that should be NA after '",
             ltr[[yn[i]]]["to"], "':\n",
             paste0(poll_ids(x)[post_polls], collapse = ", "), call. = FALSE)
      }
    }
  }
}


#' Extract the parameter names and parameter counts from a poll_of_polls object
#'
#' @param x a [poll_of_polls] object
#' @param rm_idx remove parameter indecies (e.g. [1], [1,1], [1,1,1]) from the parameter names. Default is FALSE.
#' @export
parameter_names <- function(x, rm_idx = FALSE){
  res <- try(backend_parameter_names(x$backend, x$stan_fit), silent = TRUE)
  if(rm_idx) res <- parameters_names_remove_indecies(res)

  if(inherits(res, "try-error")){
    warning("Stan model does not contain samples.")
    return(NULL)
  }
  res
}

#' @rdname parameter_names
#' @export
extract_parameter_names <- parameter_names

#' @rdname parameter_names
#' @export
parameter_block_names <- function(x){
  parameters_names_remove_indecies(parameter_names(x))
}

parameters_names_remove_indecies <- function(nms){
  checkmate::assert_character(nms)
  nms <- sub("\\[[0-9]+\\]", "", nms)
  nms <- sub("\\[[0-9]+,[0-9]+\\]", "", nms)
  nms <- sub("\\[[0-9]+,[0-9]+,[0-9]+\\]", "", nms)
  nms
}

#' @rdname parameter_names
#' @export
parameters_counts <- function(x){
  table(parameter_block_names(x))
}



#' @rdname parameter_names
#' @export
get_parameter_names <- parameter_names


#' Get the model arguments used in a [poll_of_polls] object
#'
#' @param x a [poll_of_polls] object
#' @param all Should all model arguments be returned, or just the one that were set?
#'
#' @return a list with model arguments
#'
#' @export
get_model_model_arguments <- function(x, all = TRUE){
  checkmate::assert_class(x, classes = "poll_of_polls")
  if(all){
    ma <- x$stan_data$stan_data[model_arguments(x$model)]
    ma <- ma[!is.na(names(ma))]
  } else {
    ma <- x$model_arguments
  }
  ma
}


#' Get the model diagnostics used in a [poll_of_polls] object
#'
#' @param x a [poll_of_polls] object
#'
#' @details
#' no_divergent_transitions: The number of draws with divergent transitions
#' no_max_treedepth: The number of draws where the max treedepth was reached
#' no_low_bfmi_chains: The number of chains with low E-BFMI
#' no_Rhat_above_1_1: The number of parameters with an Rhat above 1.1
#' no_Rhat_is_NA: The number of parameters without an Rhat value
#' mean_no_leapfrog_steps: The mean number of leapfrog steps taken per iteration/draw
#' mean_chain_step_size: The mean stepsize over the chains
#' mean_chain_inv_mass_matrix_min_max: The mean max and min of the invers mass matrix over the chains
#' mean_chain_warmup_time: the mean warmup time in seconds
#' mean_chain_sampling_time: the mean warmup time in seconds
#'
#' @return a list with diagnostic values
#'
#' @export
get_model_diagnostics <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  res <- list()
  if(x$backend == "rstan"){
    res$no_divergent_transistions <- sum(rstan::get_divergent_iterations(x$stan_fit))
    res$no_max_treedepth <- sum(rstan::get_max_treedepth_iterations(x$stan_fit))
    res$no_low_bfmi_chains <- length(rstan::get_low_bfmi_chains(x$stan_fit))
    res$mean_no_leapfrog_steps <- mean(rstan::get_num_leapfrog_per_iteration(x$stan_fit))
    tm <- rstan::get_elapsed_time(x$stan_fit)
    res$mean_chain_warmup_time <- mean(tm[,"warmup"])
    res$mean_chain_sampling_time <- mean(tm[,"sample"])
  } else if(x$backend == "cmdstanr"){
    ds <- x$stan_fit$diagnostic_summary(quiet = TRUE)
    res$no_divergent_transistions <- sum(ds$num_divergent)
    res$no_max_treedepth <- sum(ds$num_max_treedepth)
    res$no_low_bfmi_chains <- sum(ds$ebfmi < 0.3, na.rm = TRUE)

    sp <- get_sampler_params(x, inc_warmup = FALSE)
    res$mean_no_leapfrog_steps <- mean(unlist(lapply(sp, function(chain) chain[, "n_leapfrog__"])))

    tm <- x$stan_fit$time()
    res$mean_chain_warmup_time <- mean(tm$chains$warmup)
    res$mean_chain_sampling_time <- mean(tm$chains$sampling)
  } else {
    stop("Unknown backend '", x$backend, "' in get_model_diagnostics().", call. = FALSE)
  }
  if(!is.null(x$diagnostics)){
    res$no_Rhat_above_1_1 <- sum(x$diagnostics$Rhat[!is.na(x$diagnostics$Rhat)] > 1.1)
    res$no_Rhat_is_NA <- sum(is.na(x$diagnostics$Rhat))
  }

  ai <- get_adaptation_info(x)
  res$mean_chain_step_size <- mean(unlist(lapply(ai, function(x) x$step_size)))
  res$mean_chain_inv_mass_matrix_min <-
    mean(unlist(lapply(ai, function(x) min(x$diag_inv_mass_matrix))))
  res$mean_chain_inv_mass_matrix_max <-
    mean(unlist(lapply(ai, function(x) max(x$diag_inv_mass_matrix))))
  res
}

#' Get the number of draws/samples in a [poll_of_polls] object
#'
#' @param x  a [poll_of_polls] object
#' @export
get_ndraws <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  backend_get_ndraws(x$backend, x$stan_fit)
}

#' @rdname get_ndraws
#' @export
ndraws <- get_ndraws

#' Get the levels/houses from a [poll_of_polls] object
#'
#' @description
#' The house levels are extracted in the same order
#' as h_i in the stan data object.
#'
#' @param x  a [poll_of_polls] object
#' @export
house_levels <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  levels(x$polls_data$poll_info$.house)
}

#' @rdname house_levels
#' @export
party_levels <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  return(x$y)
}


get_git_sha <- function(){
  gsha <- try(git2r::revparse_single(git2r::repository(),"HEAD")$sha, silent = TRUE)
  if(inherits(gsha, "try-error")) gsha <- NULL
  gsha
}


compute_diagnostics <- function(x, backend = "rstan"){
  backend_compute_diagnostics(backend = backend, fit = x)
}



#' Extract all pop arguments for poll_of_polls() from a pop object
#'
#' @description
#' This function takes a pop object and extracts the relevant arguments for the poll_of_polls() function.
#' It ensures that the pop object is valid and then retrieves the necessary information to be used as input for poll_of_polls().
#'
#' @param x A pop object
#'
#' @export
extract_poll_of_polls_input_arguments <- function(x){
  assert_pop(x)
  # Extract the arguments for poll_of_polls() from the pop object
  args <- list()

  # Extract relevant information from the pop object
  args <- x$input_args
  args$polls_data <- x$polls_data
  args$known_state <- x$known_state

  class(args) <- c("poll_of_polls_input_arguments", "list")

  return(args)
}
