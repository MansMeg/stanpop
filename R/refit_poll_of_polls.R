#' Refit a poll_of_polls object
#'
#' @description
#' `refit_poll_of_polls()` reconstructs a call to [poll_of_polls()] from an
#' existing [poll_of_polls] object. The public API mirrors the practical refit
#' use case: optionally replace `polls_data`, optionally choose refit-specific
#' settings such as `backend`, `compile_args`, `cache_dir`, and `warm_start`,
#' and use `...` only for backend sampler arguments.
#'
#' Backend sampler arguments inherit from `x` and can be overridden or removed
#' via `...`. Named `NULL` values remove stored sampler arguments.
#'
#' Warm starts are filled in automatically unless you override them in
#' `warm_start`. Omitted `warm_start` elements are taken from the last draw and
#' sampler state of `x`. When `polls_data` changes, reused warm-start values
#' are validated against the refit model's parameter dimensions before
#' sampling begins.
#'
#' Structural model inputs other than `polls_data` are always inherited from
#' `x`; use [poll_of_polls()] directly to change `y`, `model`, `time_scale`,
#' `time_scale_overrides`, `known_state`, `model_time_range`,
#' `latent_time_ranges`, `hyper_parameters`, or `slow_scales`.
#'
#' @param x Existing [poll_of_polls] object to refit.
#' @param polls_data a [polls_data] object. Defaults to `x$polls_data`.
#' @param backend Stan backend to use for the refit. Only `backend = "cmdstanr"`
#'   is currently supported because RStan does not accept a supplied mass
#'   matrix.
#'   The argument remains explicit so a future version can extend backend
#'   support without changing the public API.
#' @param compile_args additional arguments passed to
#'   [cmdstanr::cmdstan_model()] when `backend = "cmdstanr"`. Defaults to
#'   `x$compile_arguments`.
#' @param cache_dir directory to cache model. Defaults to `x$cache_dir`.
#' @param warm_start A named list controlling warm-start initialization.
#'   Supported elements are `init`, `init_mode`, `inv_metric`,
#'   `metric_type`, and `step_size`.
#'   Omitted elements use the automatic defaults extracted from `x`.
#'   Set an element to `NULL` to disable that default warm-start component.
#'   When supplied, `init_mode` must be one of `"last"` or `"random"`.
#'   The default is `"last"`, which reuses the final draw from each stored
#'   chain and therefore requires the refit to use the same number of chains.
#'   `"random"` samples one post-warmup posterior draw per refit chain from
#'   `x$stan_fit` and reuses the `inv_metric` and `step_size` from the sampled
#'   source chain.
#'   For example, `warm_start = list(inv_metric = NULL)` reuses the last draw
#'   but does not reuse the inverse metric.
#' @param ... Named backend sampler arguments supplied to
#'   `CmdStanModel$sample()`. Warm-start controls such as `init`, `init_mode`,
#'   `inv_metric`, `metric_type`, and `step_size` must be supplied through
#'   `warm_start`, not `...`.
#'
#' @return A refitted [poll_of_polls] object.
#' @export
refit_poll_of_polls <- function(x,
                                polls_data = x$polls_data,
                                backend = "cmdstanr",
                                compile_args = x$compile_arguments,
                                cache_dir = x$cache_dir,
                                warm_start = list(),
                                ...) {
  dots <- list(...)

  resolved <- resolve_refit_poll_of_polls_arguments(
    x = x,
    polls_data = polls_data,
    backend = backend,
    compile_args = compile_args,
    cache_dir = cache_dir,
    dots = dots,
    warm_start = warm_start
  )
  resolved$sample_args <- refit_materialize_warm_start_arguments(
    x = x,
    backend = resolved$constructor_args$backend,
    sample_args = resolved$sample_args,
    warm_start = resolved$warm_start
  )
  assert_warm_start_is_compatible(
    x = x,
    constructor_args = resolved$constructor_args,
    sample_args = resolved$sample_args
  )

  do.call(
    poll_of_polls,
    c(resolved$constructor_args, resolved$sample_args)
  )
}

#' @rdname refit_poll_of_polls
#' @export
reestimate_poll_of_polls <- refit_poll_of_polls


#' Resolve refit arguments before rebuilding the model
#'
#' @description
#' Build the pieces needed to rebuild a [poll_of_polls()] call from an
#' existing object under the narrow refit API. This helper validates the
#' requested backend, checks that `...` contains only backend sampler
#' arguments, normalizes the `warm_start` specification, and returns the
#' inherited constructor arguments after the explicit refit inputs
#' (`polls_data`, `backend`, `compile_args`, and `cache_dir`) have been
#' applied. Automatic warm-start defaults are not inserted here; they are added
#' later by [refit_materialize_warm_start_arguments()].
#'
#' @keywords internal
resolve_refit_poll_of_polls_arguments <- function(x,
                                                  polls_data,
                                                  backend,
                                                  compile_args,
                                                  cache_dir,
                                                  dots,
                                                  warm_start) {
  assert_pop(x)
  assert_refit_poll_of_polls_backend(backend)
  checkmate::assert_list(dots, null.ok = TRUE)
  warm_start <- normalize_refit_warm_start(warm_start)
  if(length(dots) > 0L && (is.null(names(dots)) || any(names(dots) == ""))) {
    stop("All overrides in '...' must be named.", call. = FALSE)
  }

  # `...` is reserved for backend sampler arguments only.
  constructor_names_in_dots <- intersect(
    names(dots),
    c(refit_inherited_constructor_argument_names(), "polls_data", "backend", "compile_args", "cache_dir")
  )
  if(length(constructor_names_in_dots) > 0L) {
    stop(
      "Only backend sampler arguments belong in '...'. ",
      "Use the explicit refit arguments 'polls_data', 'backend', 'compile_args', ",
      "'cache_dir', and 'warm_start'. ",
      "Use poll_of_polls() directly to change inherited model inputs such as ",
      paste0(sprintf("'%s'", refit_inherited_constructor_argument_names()), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  unsupported_wrapper_names <- intersect(
    names(dots),
    c("reuse_last_draw", "reuse_inv_metric", "reuse_step_size")
  )
  if(length(unsupported_wrapper_names) > 0L) {
    stop(
      "Warm-start defaults are automatic. Override them via 'warm_start'.",
      call. = FALSE
    )
  }
  warm_start_names_in_dots <- intersect(names(dots), refit_warm_start_argument_names())
  if(length(warm_start_names_in_dots) > 0L) {
    stop(
      "Warm-start overrides belong in 'warm_start', not in '...'. ",
      "Move ", paste0(warm_start_names_in_dots, collapse = ", "),
      " into the 'warm_start' list.",
      call. = FALSE
    )
  }

  # Rebuild the poll_of_polls() constructor args from x, then apply the explicit refit arguments.
  constructor_args <- unclass(extract_poll_of_polls_refit_arguments(x))
  constructor_args["polls_data"] <- list(polls_data)
  constructor_args["backend"] <- list(backend)
  constructor_args["compile_args"] <- list(compile_args)
  constructor_args["cache_dir"] <- list(cache_dir)

  # Rebuild the inherited sampler args, drop old warm-start values, and apply refit overrides.
  sample_args <- unclass(extract_poll_of_polls_sample_arguments(x))
  sample_args <- refit_remove_inherited_warm_start_arguments(sample_args)
  sample_args <- refit_translate_sample_arguments(
    sample_args = sample_args,
    from_backend = x$backend,
    to_backend = backend
  )
  sample_args <- refit_merge_sample_arguments(
    sample_args = sample_args,
    sample_overrides = dots
  )

  list(
    constructor_args = constructor_args,
    sample_args = sample_args,
    warm_start = warm_start
  )
}

#' @keywords internal
assert_refit_poll_of_polls_backend <- function(backend) {
  assert_pop_backend(backend)
  if(!identical(backend, "cmdstanr")) {
    stop(
      "refit_poll_of_polls() currently only supports backend = 'cmdstanr'.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @keywords internal
refit_inherited_constructor_argument_names <- function() {
  c(
    "y",
    "model",
    "time_scale",
    "time_scale_overrides",
    "known_state",
    "model_time_range",
    "latent_time_ranges",
    "hyper_parameters",
    "slow_scales"
  )
}

#' @keywords internal
refit_warm_start_argument_names <- function() {
  c("init", "init_mode", "inv_metric", "metric_type", "step_size")
}

#' Supported automatic init-reuse modes for refits
#'
#' @description
#' Return the allowed values for `warm_start$init_mode` in
#' [refit_poll_of_polls()]. These modes control how automatic init reuse is
#' materialized when the caller does not supply an explicit `warm_start$init`.
#'
#' @return Character vector of allowed init modes.
#'
#' @keywords internal
refit_init_mode_choices <- function() {
  c("last", "random")
}

#' Resolve the automatic init-reuse mode for a refit
#'
#' @description
#' Return the effective automatic init mode implied by a validated
#' `warm_start` list. When `init_mode` is omitted, the helper falls back to the
#' backward-compatible default `"last"`.
#'
#' @param warm_start A validated warm-start list.
#'
#' @return Character scalar, either `"last"` or `"random"`.
#'
#' @keywords internal
refit_default_init_mode <- function(warm_start) {
  checkmate::assert_list(warm_start, null.ok = FALSE)
  if(is.null(warm_start$init_mode)) {
    return("last")
  }
  warm_start$init_mode
}

#' Extract a reproducible seed for random init reuse
#'
#' @description
#' Pull the refit sampling seed from `sample_args` when it is available so
#' automatic `init_mode = "random"` selection can be reproducible.
#'
#' @param sample_args Named sampler argument list for the refit call.
#'
#' @return Integer scalar seed, or `NULL` when the refit does not specify one.
#'
#' @keywords internal
refit_random_init_seed <- function(sample_args) {
  checkmate::assert_list(sample_args, names = "named")
  if(is.null(sample_args$seed)) {
    return(NULL)
  }
  checkmate::assert_integerish(sample_args$seed, len = 1L, lower = 1L, any.missing = FALSE)
  as.integer(sample_args$seed)[[1]]
}

#' Normalize explicit warm-start overrides for a refit
#'
#' @description
#' Validate and normalize the `warm_start` list supplied to
#' [refit_poll_of_polls()]. The helper requires a named list, rejects duplicate
#' or unsupported element names, validates `init_mode`, and enforces the public
#' `metric_type` name rather than the backend sampler argument name `metric`.
#'
#' An empty list means that `refit_poll_of_polls()` should fall back to its
#' automatic warm-start defaults. Named `NULL` entries are preserved so callers
#' can explicitly disable a default warm-start component later in the refit
#' pipeline.
#'
#' @param warm_start A named list of explicit warm-start overrides. Supported
#'   elements are `init`, `init_mode`, `inv_metric`, `metric_type`, and
#'   `step_size`. When supplied, `init_mode` must be one of `"last"` or
#'   `"random"`. The default is `"last"`.
#'
#' @return The validated `warm_start` list, preserving any named `NULL`
#'   elements.
#'
#' @keywords internal
normalize_refit_warm_start <- function(warm_start) {
  checkmate::assert_list(warm_start, null.ok = FALSE)
  if(length(warm_start) == 0L) {
    return(warm_start)
  }
  if(is.null(names(warm_start)) || any(names(warm_start) == "")) {
    stop("All elements of 'warm_start' must be named.", call. = FALSE)
  }
  if(any(duplicated(names(warm_start)))) {
    stop("Elements of 'warm_start' must have unique names.", call. = FALSE)
  }
  if("metric" %in% names(warm_start)) {
    stop(
      "Use 'metric_type' rather than 'metric' in 'warm_start'.",
      call. = FALSE
    )
  }
  unknown_names <- setdiff(names(warm_start), refit_warm_start_argument_names())
  if(length(unknown_names) > 0L) {
    stop(
      "Unknown 'warm_start' element(s): ",
      paste0(unknown_names, collapse = ", "),
      ". Supported elements are: ",
      paste0(refit_warm_start_argument_names(), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if("init_mode" %in% names(warm_start) && !is.null(warm_start$init_mode)) {
    checkmate::assert_choice(
      warm_start$init_mode,
      choices = refit_init_mode_choices()
    )
  }
  warm_start
}

#' @keywords internal
refit_remove_inherited_warm_start_arguments <- function(sample_args) {
  checkmate::assert_list(sample_args, names = "named")
  sample_args[c("init", "inv_metric", "step_size", "metric_file")] <- NULL
  sample_args
}

#' @keywords internal
refit_stored_warm_start_state <- function(x) {
  assert_pop(x)
  if("warm_start_state" %in% names(x) && is.list(x$warm_start_state)) {
    return(x$warm_start_state)
  }
  NULL
}

#' @keywords internal
refit_stored_warm_start_field <- function(x, name) {
  state <- refit_stored_warm_start_state(x)
  if(is.null(state) || !name %in% names(state)) {
    return(NULL)
  }
  state[[name]]
}

#' Test whether cached automatic init reuse is available
#'
#' @description
#' Return whether a stored `warm_start_state` explicitly says that automatic
#' `init` reuse is available. Older objects that do not store this flag return
#' `NULL`, so callers can fall back to backend recovery when needed.
#'
#' @param x A fitted `poll_of_polls` object.
#'
#' @return `TRUE`, `FALSE`, or `NULL`.
#'
#' @keywords internal
refit_cached_init_is_complete <- function(x) {
  state <- refit_stored_warm_start_state(x)
  if(is.null(state) || !"init_complete" %in% names(state)) {
    return(NULL)
  }
  isTRUE(state$init_complete)
}

#' Materialize warm-start arguments for a refit call
#'
#' @description
#' Fill the backend sampling arguments needed for a warm-started refit. The
#' helper starts from an existing list of sampler arguments, then inserts or
#' removes `init`, `inv_metric`, `metric_type`, and `step_size` according to
#' the explicit `warm_start` list.
#'
#' Omitted warm-start elements are filled automatically from the stored fit in
#' `x`. By default (`init_mode = "last"`), the final constrained draw from
#' each stored chain is reused for `init`, which requires the refit to use the
#' same number of chains. With `init_mode = "random"`, one post-warmup
#' posterior draw is sampled per refit chain from `x$stan_fit`; the sampler
#' state for `inv_metric` and `step_size` is then taken from the sampled source
#' chain. When an inverse metric is present and no explicit `metric_type` is
#' supplied, the metric type is inferred from the shape of that inverse
#' metric. Named `NULL` entries in `warm_start` explicitly disable the
#' corresponding warm-start component.
#'
#' This helper only materializes warm-start related sampler arguments. It does
#' not rebuild the model inputs or merge ordinary sampler overrides.
#'
#' @param x A fitted `poll_of_polls` object that provides the stored fit and
#'   sampler state for automatic warm-start defaults.
#' @param backend Backend for the refit sampler arguments.
#' @param sample_args A named list of backend sampling arguments after ordinary
#'   sampler overrides have been merged.
#' @param warm_start A validated named list of explicit warm-start overrides.
#'   Supported elements are `init`, `init_mode`, `inv_metric`,
#'   `metric_type`, and `step_size`.
#'
#' @return A named list of sampling arguments with warm-start fields
#'   materialized.
#'
#' @keywords internal
refit_materialize_warm_start_arguments <- function(x,
                                                   backend,
                                                   sample_args,
                                                   warm_start = list()) {
  assert_pop(x)
  assert_pop_backend(backend)
  checkmate::assert_list(sample_args, names = "named")
  warm_start <- normalize_refit_warm_start(warm_start)
  init_mode <- refit_default_init_mode(warm_start)
  auto_init <- NULL
  auto_init_source_chain_ids <- NULL

  if(!("init" %in% names(warm_start))) {
    refit_chains <- refit_resolve_chain_count(x, sample_args)
    auto_init_payload <- refit_materialize_automatic_init(
      x = x,
      chains = refit_chains,
      init_mode = init_mode,
      sample_args = sample_args
    )
    auto_init <- auto_init_payload$init
    auto_init_source_chain_ids <- auto_init_payload$source_chain_ids
  }

  # Handle init
  if("init" %in% names(warm_start)) {
    sample_args <- refit_set_named_argument(sample_args, "init", warm_start$init)
  } else {
    if(!is.null(auto_init)) {
      sample_args <- refit_set_named_argument(sample_args, "init", auto_init)
    }
  }

  need_sampler_state <- (
    !("inv_metric" %in% names(warm_start)) && is.null(sample_args$metric_file)
  ) || !("step_size" %in% names(warm_start))
  state <- NULL
  if(need_sampler_state) {
    state <- refit_stored_warm_start_field(x, "sampler_state")
    if(is.null(state)) {
      if(is.null(x$stan_fit)) {
        stop("The stored fit is missing, so sampler state cannot be reused.", call. = FALSE)
      }
      state <- backend_get_sampler_state(x$backend, x$stan_fit)
    }
    if(!is.null(auto_init_source_chain_ids)) {
      state <- refit_subset_sampler_state(
        state = state,
        chain_ids = auto_init_source_chain_ids
      )
    }
  }

  # Handle inv metric
  if("inv_metric" %in% names(warm_start)) {
    sample_args <- refit_set_inv_metric_argument(
      sample_args,
      backend,
      warm_start$inv_metric
    )
  } else if(is.null(sample_args$metric_file)) {
    sample_args <- refit_set_inv_metric_argument(
      sample_args,
      backend,
      refit_sampler_state_field(state, "inv_metric")
    )
  }
  inv_metric_value <- sample_args$inv_metric

  # `metric_type` is the public refit name; CmdStanR expects the sampler arg `metric`.
  if("metric_type" %in% names(warm_start)) {
    sample_args <- refit_set_metric_type_argument(
      sample_args,
      backend,
      warm_start$metric_type
    )
    if(!is.null(sample_args$metric) && !is.null(inv_metric_value)) {
      assert_metric_type_matches_inv_metric(
        metric_type = sample_args$metric,
        inv_metric = inv_metric_value
      )
    }
  } else if(!is.null(inv_metric_value)) {
    sample_args <- refit_set_metric_type_argument(
      sample_args,
      backend,
      refit_metric_type_from_inv_metric(inv_metric_value)
    )
  }

  if("step_size" %in% names(warm_start)) {
    sample_args <- refit_set_step_size_argument(sample_args, backend, warm_start$step_size)
  } else {
    sample_args <- refit_set_step_size_argument(
      sample_args,
      backend,
      refit_sampler_state_field(state, "step_size", simplify = TRUE)
    )
  }
  sample_args
}

#' Translate stored sample arguments between backends
#'
#' @keywords internal
refit_translate_sample_arguments <- function(sample_args, from_backend, to_backend) {
  checkmate::assert_list(sample_args, names = "named", null.ok = TRUE)
  assert_pop_backend(from_backend)
  assert_pop_backend(to_backend)

  if(identical(from_backend, to_backend) || length(sample_args) == 0L) {
    return(sample_args)
  }
  if(from_backend == "rstan" && to_backend == "cmdstanr") {
    return(refit_translate_sample_arguments_rstan_to_cmdstanr(sample_args))
  }
  if(from_backend == "cmdstanr" && to_backend == "rstan") {
    return(refit_translate_sample_arguments_cmdstanr_to_rstan(sample_args))
  }
  sample_args
}

#' @keywords internal
refit_translate_sample_arguments_rstan_to_cmdstanr <- function(sample_args) {
  out <- sample_args
  control <- refit_default(sample_args$control, list())

  if("iter" %in% names(sample_args)) {
    warmup <- refit_default(sample_args$warmup, floor(sample_args$iter / 2))
    out$iter_warmup <- warmup
    out$iter_sampling <- max(sample_args$iter - warmup, 0)
  } else if("warmup" %in% names(sample_args)) {
    out$iter_warmup <- sample_args$warmup
  }

  if("cores" %in% names(sample_args)) out$parallel_chains <- sample_args$cores
  if("algorithm" %in% names(sample_args) && identical(sample_args$algorithm, "Fixed_param")) {
    out$fixed_param <- TRUE
  }
  if("adapt_delta" %in% names(control)) out$adapt_delta <- control$adapt_delta
  if("max_treedepth" %in% names(control)) out$max_treedepth <- control$max_treedepth
  if("stepsize" %in% names(control)) out$step_size <- control$stepsize
  if("metric" %in% names(control)) out$metric <- control$metric
  if("adapt_engaged" %in% names(control)) out$adapt_engaged <- control$adapt_engaged

  out[c("file", "model_name", "data", "control", "iter", "warmup", "cores", "algorithm", "init_r")] <- NULL
  out
}

#' @keywords internal
refit_translate_sample_arguments_cmdstanr_to_rstan <- function(sample_args) {
  out <- sample_args
  control <- refit_default(sample_args$control, list())

  if("iter_warmup" %in% names(sample_args) || "iter_sampling" %in% names(sample_args)) {
    warmup <- refit_default(sample_args$iter_warmup, 0L)
    iter_sampling <- refit_default(sample_args$iter_sampling, 0L)
    out$warmup <- warmup
    out$iter <- warmup + iter_sampling
  }

  if("parallel_chains" %in% names(sample_args)) out$cores <- sample_args$parallel_chains
  if("adapt_delta" %in% names(sample_args)) control$adapt_delta <- sample_args$adapt_delta
  if("max_treedepth" %in% names(sample_args)) control$max_treedepth <- sample_args$max_treedepth
  if("step_size" %in% names(sample_args)) control$stepsize <- sample_args$step_size
  if("stepsize" %in% names(sample_args)) control$stepsize <- sample_args$stepsize
  if("metric" %in% names(sample_args)) control$metric <- sample_args$metric
  if("adapt_engaged" %in% names(sample_args)) control$adapt_engaged <- sample_args$adapt_engaged
  if(length(control) > 0) out$control <- control

  if("fixed_param" %in% names(sample_args) && isTRUE(sample_args$fixed_param)) {
    out$algorithm <- "Fixed_param"
  }

  out[c("data",
        "save_latent_dynamics",
        "output_dir",
        "output_basename",
        "sig_figs",
        "parallel_chains",
        "chain_ids",
        "threads_per_chain",
        "opencl_ids",
        "iter_warmup",
        "iter_sampling",
        "save_warmup",
        "max_treedepth",
        "adapt_engaged",
        "adapt_delta",
        "step_size",
        "metric",
        "metric_file",
        "inv_metric",
        "init_buffer",
        "term_buffer",
        "window",
        "fixed_param",
        "show_messages",
        "show_exceptions",
        "diagnostics",
        "save_metric",
        "save_cmdstan_config",
        "cores",
        "num_cores",
        "num_chains",
        "num_warmup",
        "num_samples",
        "validate_csv",
        "save_extra_diagnostics",
        "max_depth",
        "stepsize")] <- NULL
  out
}

#' Merge stored and explicit sample arguments for a refit
#'
#' @keywords internal
refit_merge_sample_arguments <- function(sample_args, sample_overrides) {
  checkmate::assert_list(sample_args, names = "named")
  checkmate::assert_list(sample_overrides, null.ok = TRUE)

  if(length(sample_overrides) == 0L) {
    return(sample_args)
  }
  if(is.null(names(sample_overrides)) || any(names(sample_overrides) == "")) {
    stop("Sampler overrides in '...' must all be named.", call. = FALSE)
  }

  for(i in seq_along(sample_overrides)) {
    name <- names(sample_overrides)[[i]]
    value <- sample_overrides[[i]]
    sample_args <- refit_set_named_argument(sample_args, name, value)
  }
  sample_args
}

#' @keywords internal
refit_set_named_argument <- function(args, name, value) {
  checkmate::assert_list(args, names = "named")
  checkmate::assert_string(name)

  if(is.null(value)) {
    args[[name]] <- NULL
    return(args)
  }
  args[[name]] <- value
  args
}

#' @keywords internal
refit_set_inv_metric_argument <- function(sample_args, backend, value) {
  assert_pop_backend(backend)
  if(backend != "cmdstanr" && !is.null(value)) {
    stop(
      "Inverse metric reuse requires backend = 'cmdstanr'.",
      call. = FALSE
    )
  }
  if(backend == "cmdstanr" && !is.null(value)) {
    sample_args$metric_file <- NULL
  }
  refit_set_named_argument(sample_args, "inv_metric", value)
}

#' @keywords internal
refit_set_metric_type_argument <- function(sample_args, backend, value) {
  assert_pop_backend(backend)
  if(!is.null(value)) {
    checkmate::assert_choice(value, choices = c("diag_e", "dense_e", "unit_e"))
  }
  if(backend == "cmdstanr") {
    return(refit_set_named_argument(sample_args, "metric", value))
  }
  refit_set_rstan_control_argument(sample_args, "metric", value)
}

#' @keywords internal
refit_set_step_size_argument <- function(sample_args, backend, value) {
  assert_pop_backend(backend)

  if(backend == "cmdstanr") {
    if(!is.null(value)) {
      checkmate::assert_numeric(value, lower = .Machine$double.eps, any.missing = FALSE)
    }
    return(refit_set_named_argument(sample_args, "step_size", value))
  }

  if(is.null(value)) {
    return(refit_set_rstan_control_argument(sample_args, "stepsize", NULL))
  }

  # RStan only supports one shared step size, so collapse repeated values and reject chain-specific ones.
  checkmate::assert_numeric(value, lower = .Machine$double.eps, any.missing = FALSE)
  if(length(value) > 1L) {
    unique_value <- unique(as.numeric(value))
    if(length(unique_value) != 1L) {
      stop(
        "RStan only accepts a single step size value. ",
        "Use backend = 'cmdstanr' for chain-specific step sizes.",
        call. = FALSE
      )
    }
    value <- unique_value[[1]]
  }
  refit_set_rstan_control_argument(sample_args, "stepsize", value)
}

#' @keywords internal
refit_set_rstan_control_argument <- function(sample_args, name, value) {
  checkmate::assert_list(sample_args, names = "named")
  checkmate::assert_string(name)

  control <- refit_default(sample_args$control, list())
  if(is.null(value)) {
    control[[name]] <- NULL
  } else {
    control[[name]] <- value
  }

  if(length(control) == 0L) {
    sample_args$control <- NULL
  } else {
    sample_args$control <- control
  }
  sample_args
}

#' Extract one field from per-chain sampler state
#'
#' @description
#' Pull a single named field from the sampler state returned by
#' [backend_get_sampler_state()]. The input `state` is a list with one entry
#' per chain, and this helper collects the requested field across chains.
#'
#' By default the return value preserves that per-chain list structure, which
#' is appropriate for fields such as `inv_metric`. When `simplify = TRUE`, the
#' extracted values are flattened to an atomic vector, which is convenient for
#' scalar-per-chain fields such as `step_size`.
#'
#' @param state A per-chain sampler state list, typically returned by
#'   [backend_get_sampler_state()].
#' @param field Name of the sampler-state field to extract from each chain.
#' @param simplify Logical indicating whether to flatten the extracted values
#'   with [unlist()].
#'
#' @return A list of per-chain field values, or an atomic vector when
#'   `simplify = TRUE`.
#'
#' @keywords internal
refit_sampler_state_field <- function(state, field, simplify = FALSE) {
  checkmate::assert_list(state)
  checkmate::assert_string(field)

  values <- lapply(state, function(x) x[[field]])
  if(!isTRUE(simplify)) {
    return(values)
  }
  unlist(values, use.names = FALSE)
}

#' Assert warm-start compatibility before sampling
#'
#' @description
#' Validate that the warm-start inputs selected for a refit are compatible with
#' the requested chain count and with the parameter dimensions implied by the
#' refit model. When `polls_data` changes, this helper rebuilds the refit model
#' dimensions before sampling so incompatible `init` values or inverse metrics
#' fail early with a focused error.
#'
#' @param x Existing [poll_of_polls] object that provides the stored fit used
#'   for warm-start defaults.
#' @param constructor_args Named constructor argument list for the refit call.
#' @param sample_args Named sampler argument list after warm-start arguments
#'   have been materialized.
#'
#' @return Invisible `TRUE` when the selected warm-start settings are
#'   compatible with the refit. Otherwise an error is thrown before sampling.
#'
#' @keywords internal
assert_warm_start_is_compatible <- function(x, constructor_args, sample_args) {
  assert_pop(x)
  checkmate::assert_list(constructor_args, names = "named")
  checkmate::assert_list(sample_args, names = "named")

  # If no warm-start state is being reused, there is nothing to validate here.
  if(!any(c("init", "inv_metric", "step_size") %in% names(sample_args))) {
    return(invisible(TRUE))
  }

  # Resolve the chain count used by the refit, then coerce warm-start inputs into comparable per-chain forms.
  no_of_chains_in_refit <- refit_resolve_chain_count(x, sample_args)
  normalized_init <- refit_normalize_init_for_validation(sample_args$init, no_of_chains_in_refit)
  normalized_inv_metric <- refit_normalize_inv_metric_for_validation(sample_args$inv_metric, no_of_chains_in_refit)
  refit_normalize_step_size_for_validation(sample_args$step_size, no_of_chains_in_refit)
  if(is.null(normalized_init) && is.null(normalized_inv_metric)) {
    return(invisible(TRUE))
  }

  # Start from the parameter dimensions in x, then replace them if changed polls_data requires a rebuilt refit shape.
  changed_polls_data <- !identical(constructor_args$polls_data, x$polls_data)
  need_old_num_upars <- !is.null(normalized_inv_metric) ||
    (isTRUE(changed_polls_data) && !is.null(normalized_init))
  old_num_upars <- NULL
  if(need_old_num_upars) {
    old_num_upars <- refit_stored_warm_start_field(x, "num_upars")
    if(is.null(old_num_upars)) {
      old_num_upars <- backend_get_num_upars(x$backend, x$stan_fit)
    }
  }
  expected_num_upars <- NULL
  expected_skeleton <- NULL

  # If polls_data changed, rebuild the refit parameter dimensions before validating reuse.
  if(changed_polls_data && (!is.null(normalized_init) || !is.null(normalized_inv_metric))) {
    expected_dimensions <- refit_build_expected_parameter_dimensions(constructor_args)
    expected_num_upars <- expected_dimensions$num_upars
    if(!is.null(normalized_init)) {
      expected_skeleton <- refit_nonempty_init_skeleton(expected_dimensions$init_skeleton)
    }
  } else if(!is.null(normalized_inv_metric)) {
    expected_num_upars <- old_num_upars
  }

  # Check that any reused init still matches the constrained parameter dimensions.
  if(!is.null(normalized_init)) {
    if(is.null(expected_skeleton)) {
      expected_skeleton <- refit_stored_warm_start_field(x, "init_skeleton")
      if(is.null(expected_skeleton)) {
        expected_skeleton <- backend_get_init_skeleton(x$backend, x$stan_fit)
      }
      expected_skeleton <- refit_nonempty_init_skeleton(expected_skeleton)
    }
    assert_refit_init_matches_skeleton(
      init = normalized_init,
      expected = refit_recycle_init_skeleton(expected_skeleton, length(normalized_init)),
      source_label = refit_parameter_dimension_source_label(changed_polls_data),
      changed_polls_data = changed_polls_data,
      old_num_upars = if(isTRUE(changed_polls_data)) old_num_upars else NULL,
      new_num_upars = if(isTRUE(changed_polls_data)) expected_num_upars else NULL
    )
  }

  # Check that any reused inverse metric still matches the unconstrained dimension.
  if(!is.null(normalized_inv_metric)) {
    assert_refit_inv_metric_matches_upars(
      inv_metric = normalized_inv_metric,
      expected_num_upars = expected_num_upars,
      source_label = refit_parameter_dimension_source_label(changed_polls_data),
      changed_polls_data = changed_polls_data,
      old_num_upars = if(isTRUE(changed_polls_data)) old_num_upars else NULL
    )
  }

  invisible(TRUE)
}

#' Resolve the refit chain count for warm-start validation
#'
#' @description
#' Resolve how many chains the refit will use when validating warm-start
#' inputs. The helper prefers an explicit `chains` sampler argument, otherwise
#' it infers the chain count from chain-specific warm-start values such as
#' `init`, `inv_metric`, or `step_size`, and finally falls back to the number
#' of chains stored in `x`.
#'
#' @param x Existing [poll_of_polls] object being refit.
#' @param sample_args Named sampler argument list for the refit.
#'
#' @return Integer scalar giving the refit chain count.
#'
#' @keywords internal
refit_resolve_chain_count <- function(x, sample_args) {
  checkmate::assert_list(sample_args, names = "named")

  # An explicit sampler chains argument overrides any chain count implied by warm-start values.
  if(!is.null(sample_args$chains)) {
    checkmate::assert_integerish(sample_args$chains, len = 1L, lower = 1L, any.missing = FALSE)
    return(as.integer(sample_args$chains)[[1]])
  }

  # If chains was not supplied explicitly, infer it from any chain-specific warm-start inputs and require them to agree.
  inferred_lengths <- c(
    refit_chain_specific_argument_length(sample_args$init, "init"),
    refit_chain_specific_argument_length(sample_args$inv_metric, "inv_metric"),
    refit_chain_specific_argument_length(sample_args$step_size, "step_size")
  )
  inferred_lengths <- inferred_lengths[!is.na(inferred_lengths)]
  if(length(inferred_lengths) > 0L) {
    unique_lengths <- unique(inferred_lengths)
    if(length(unique_lengths) != 1L) {
      stop(
        "Warm-start arguments imply incompatible chain counts. ",
        "Supply consistent chain-specific warm-start values or set 'chains' explicitly.",
        call. = FALSE
      )
    }
    return(unique_lengths[[1]])
  }

  sampler_state <- refit_stored_warm_start_field(x, "sampler_state")
  if(!is.null(sampler_state)) {
    return(length(sampler_state))
  }

  length(backend_get_sampler_state(x$backend, x$stan_fit))
}

#' Detect chain-specific warm-start lengths
#'
#' @description
#' Return the number of chains encoded in a warm-start value when that value is
#' chain-specific. Shared values return `NA_integer_`, which signals that the
#' value can be recycled across chains.
#'
#' @param value Warm-start value to inspect.
#' @param name Name of the warm-start component. Supported values are `init`,
#'   `inv_metric`, and `step_size`.
#'
#' @return Integer scalar giving the chain count implied by `value`, or
#'   `NA_integer_` when `value` is not chain-specific.
#'
#' @keywords internal
refit_chain_specific_argument_length <- function(value, name) {
  checkmate::assert_string(name)

  if(is.null(value)) {
    return(NA_integer_)
  }
  if(name == "init") {
    if(refit_is_per_chain_init_list(value)) {
      return(length(value))
    }
    return(NA_integer_)
  }
  if(name == "inv_metric" && is.list(value)) {
    return(length(value))
  }
  if(name == "step_size" && length(value) > 1L) {
    return(length(value))
  }
  NA_integer_
}

#' Detect per-chain init lists
#'
#' @description
#' Test whether an `init` value is already expressed as one named init list per
#' chain, which is the structure expected for chain-specific warm-start
#' validation.
#'
#' @param x Candidate init value.
#'
#' @return Logical scalar.
#'
#' @keywords internal
refit_is_per_chain_init_list <- function(x) {
  is.list(x) &&
    length(x) > 0L &&
    (is.null(names(x)) || all(names(x) == "")) &&
    all(vapply(x, is.list, logical(1)))
}

#' Normalize init values for warm-start validation
#'
#' @description
#' Convert a supplied `init` value into a per-chain list that can be validated
#' against the expected init skeleton. Shared init lists are recycled across
#' chains, while incompatible chain counts trigger an early error.
#'
#' @param init Warm-start `init` value.
#' @param chains Integer scalar giving the number of chains.
#'
#' @return A list with one init object per chain, or `NULL` when no init should
#'   be validated.
#'
#' @keywords internal
refit_normalize_init_for_validation <- function(init, chains) {
  checkmate::assert_integerish(chains, len = 1L, lower = 1L, any.missing = FALSE)

  if(is.null(init) || !is.list(init)) {
    return(NULL)
  }
  if(refit_is_per_chain_init_list(init)) {
    if(length(init) != chains) {
      stop(
        "The warm-start argument 'init' contains ", length(init),
        " chain(s), but 'chains = ", chains, "'. ",
        "Use a matching 'chains' value or disable init reuse with warm_start = list(init = NULL).",
        call. = FALSE
      )
    }
    return(init)
  }

  # A single named init list is treated as one shared init to reuse across all chains.
  if(!is.null(names(init)) && all(names(init) != "")) {
    return(rep(list(init), chains))
  }
  NULL
}

#' Normalize inverse metrics for warm-start validation
#'
#' @description
#' Convert a supplied inverse metric into one inverse metric per chain for
#' validation. Shared inverse metrics are recycled across chains, while
#' incompatible chain counts trigger an early error.
#'
#' @param inv_metric Warm-start inverse metric value.
#' @param chains Integer scalar giving the effective number of chains.
#'
#' @return A list with one inverse metric per chain, or `NULL` when no inverse
#'   metric should be validated.
#'
#' @keywords internal
refit_normalize_inv_metric_for_validation <- function(inv_metric, chains) {
  checkmate::assert_integerish(chains, len = 1L, lower = 1L, any.missing = FALSE)

  if(is.null(inv_metric)) {
    return(NULL)
  }
  if(is.list(inv_metric)) {
    if(length(inv_metric) != chains) {
      stop(
        "The warm-start argument 'inv_metric' contains ", length(inv_metric),
        " chain(s), but 'chains = ", chains, "'. ",
        "Use a matching 'chains' value or disable inverse-metric reuse with warm_start = list(inv_metric = NULL).",
        call. = FALSE
      )
    }
    return(inv_metric)
  }
  rep(list(inv_metric), chains)
}

#' Normalize step sizes for warm-start validation
#'
#' @description
#' Normalize a supplied step size to one numeric value per chain for validation
#' against the requested chain count. Shared step sizes are recycled, while
#' incompatible chain counts trigger an early error.
#'
#' @param step_size Warm-start step size value.
#' @param chains Integer scalar giving the number of chains.
#'
#' @return Numeric vector of length `chains`, or `NULL` when no step size
#'   should be validated.
#'
#' @keywords internal
refit_normalize_step_size_for_validation <- function(step_size, chains) {
  checkmate::assert_integerish(chains, len = 1L, lower = 1L, any.missing = FALSE)

  if(is.null(step_size)) {
    return(NULL)
  }

  checkmate::assert_numeric(step_size, lower = .Machine$double.eps, any.missing = FALSE)
  if(length(step_size) == 1L) {
    return(rep(as.numeric(step_size), chains))
  }
  if(length(step_size) != chains) {
    stop(
      "The warm-start argument 'step_size' contains ", length(step_size),
      " chain-specific value(s), but 'chains = ", chains, "'. ",
      "Use a matching 'chains' value or disable step-size reuse with warm_start = list(step_size = NULL).",
      call. = FALSE
    )
  }
  as.numeric(step_size)
}

#' Remove zero-length roots from an init skeleton
#'
#' @description
#' Drop parameter roots with zero length from a per-chain init skeleton. These
#' roots do not produce usable init values and should not be treated as missing
#' warm-start parameters.
#'
#' @param skeleton Per-chain init skeleton, typically from
#'   [backend_get_init_skeleton()].
#'
#' @return The filtered per-chain init skeleton.
#'
#' @keywords internal
refit_nonempty_init_skeleton <- function(skeleton) {
  checkmate::assert_list(skeleton)
  lapply(skeleton, function(chain_expected) {
    chain_expected[vapply(chain_expected, length, integer(1)) > 0L]
  })
}

#' Recycle init skeletons to the requested chain count
#'
#' @description
#' Ensure that an expected init skeleton is available for each chain being
#' validated. A single-chain skeleton is recycled across chains; otherwise the
#' skeleton list must already match the requested chain count.
#'
#' @param expected Per-chain init skeleton.
#' @param chains Integer scalar giving the requested chain count.
#'
#' @return A per-chain init skeleton with length `chains`.
#'
#' @keywords internal
refit_recycle_init_skeleton <- function(expected, chains) {
  checkmate::assert_list(expected)
  checkmate::assert_integerish(chains, len = 1L, lower = 1L, any.missing = FALSE)

  if(length(expected) == chains) {
    return(expected)
  }
  if(length(expected) == 1L) {
    return(rep(expected, chains))
  }
  stop(
    "Expected init skeleton for ", chains, " chain(s), but found ", length(expected), ".",
    call. = FALSE
  )
}

#' Assert that warm-start init values match expected parameter dimensions
#'
#' @description
#' Compare a per-chain warm-start `init` against the expected constrained
#' parameter names and shapes for the refit. The helper collects missing roots,
#' unexpected roots, shape mismatches, and missing values into one focused error
#' message.
#'
#' @param init Per-chain warm-start init list.
#' @param expected Per-chain expected init skeleton.
#' @param source_label Short text describing where the expected parameter
#'   dimensions came from.
#' @param changed_polls_data Logical indicating whether `polls_data` changed.
#' @param old_num_upars Number of unconstrained parameters in `x`.
#' @param new_num_upars Number of unconstrained parameters implied by the refit.
#'
#' @return Invisible `TRUE` when `init` matches the expected parameter
#'   dimensions.
#'
#' @keywords internal
assert_refit_init_matches_skeleton <- function(init,
                                               expected,
                                               source_label,
                                               changed_polls_data = FALSE,
                                               old_num_upars = NULL,
                                               new_num_upars = NULL) {
  checkmate::assert_list(init)
  checkmate::assert_list(expected)
  checkmate::assert_string(source_label)

  if(length(init) != length(expected)) {
    stop(
      "Warm-start init validation expected ", length(expected),
      " chain(s), but found ", length(init), ".",
      call. = FALSE
    )
  }

  # Compare each chain's init against the expected parameter names and shapes,
  # then collect all incompatibilities into one error message.
  issues <- unlist(lapply(seq_along(expected), function(chain_id) {
    chain_init <- init[[chain_id]]
    chain_expected <- expected[[chain_id]]
    if(!is.list(chain_init) || is.null(names(chain_init)) || any(names(chain_init) == "")) {
      return(paste0("chain ", chain_id, ": init must be a named parameter list"))
    }

    missing <- setdiff(names(chain_expected), names(chain_init))
    unexpected <- setdiff(names(chain_init), names(chain_expected))
    shape_mismatches <- names(chain_expected)[vapply(names(chain_expected), function(root) {
      if(!root %in% names(chain_init)) {
        return(FALSE)
      }
      !identical(
        refit_object_shape(chain_init[[root]]),
        refit_object_shape(chain_expected[[root]])
      )
    }, logical(1))]
    missing_values <- names(chain_init)[vapply(chain_init, anyNA, logical(1))]

    c(
      if(length(missing) > 0L) paste0("chain ", chain_id, " missing parameter roots: ", paste0(missing, collapse = ", ")),
      if(length(unexpected) > 0L) paste0("chain ", chain_id, " has unexpected parameter roots: ", paste0(unexpected, collapse = ", ")),
      if(length(shape_mismatches) > 0L) paste0("chain ", chain_id, " has changed parameter shape(s): ", paste0(shape_mismatches, collapse = ", ")),
      if(length(missing_values) > 0L) paste0("chain ", chain_id, " has missing values in parameter root(s): ", paste0(missing_values, collapse = ", "))
    )
  }), use.names = FALSE)

  # Print error message if any issues were found.
  if(length(issues) > 0L) {
    stop(
      "Warm-start init is incompatible with ", source_label, ". ",
      refit_parameter_dimension_change_hint(
        changed_polls_data = changed_polls_data,
        old_num_upars = old_num_upars,
        new_num_upars = new_num_upars
      ),
      paste0(issues, collapse = "; "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Assert that inverse metrics match expected unconstrained dimensions
#'
#' @description
#' Validate that each supplied inverse metric encodes the same number of
#' unconstrained parameters as the refit model. This catches incompatible mass
#' matrices before Stan starts sampling.
#'
#' @param inv_metric Per-chain inverse metric list.
#' @param expected_num_upars Integer scalar giving the expected number of
#'   unconstrained parameters for the refit.
#' @param source_label Short text describing where the expected parameter
#'   dimensions came from.
#' @param changed_polls_data Logical indicating whether `polls_data` changed.
#' @param old_num_upars Number of unconstrained parameters in `x`.
#'
#' @return Invisible `TRUE` when all inverse metrics match the expected
#'   unconstrained dimension.
#'
#' @keywords internal
assert_refit_inv_metric_matches_upars <- function(inv_metric,
                                                  expected_num_upars,
                                                  source_label,
                                                  changed_polls_data = FALSE,
                                                  old_num_upars = NULL) {
  checkmate::assert_list(inv_metric)
  checkmate::assert_integerish(expected_num_upars, len = 1L, lower = 1L, any.missing = FALSE)
  checkmate::assert_string(source_label)

  metric_dims <- vapply(inv_metric, refit_inv_metric_dimension, integer(1))
  mismatched_chains <- which(metric_dims != expected_num_upars)
  if(length(mismatched_chains) > 0L) {
    chain_details <- paste0(
      "chain ", mismatched_chains,
      " encodes ", metric_dims[mismatched_chains], " unconstrained parameter(s)"
    )
    stop(
      "Warm-start inv_metric is incompatible with ", source_label, ". ",
      refit_parameter_dimension_change_hint(
        changed_polls_data = changed_polls_data,
        old_num_upars = old_num_upars,
        new_num_upars = expected_num_upars
      ),
      "The refit model expects ", expected_num_upars,
      " unconstrained parameter(s), but ",
      paste0(chain_details, collapse = "; "),
      ". Disable inverse-metric reuse with warm_start = list(inv_metric = NULL).",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Determine inverse-metric dimension
#'
#' @description
#' Return the unconstrained parameter dimension encoded by an inverse metric.
#' Dense metrics are validated as square matrices.
#'
#' @param inv_metric Inverse metric value, either a numeric vector or a square
#'   numeric matrix.
#'
#' @return Integer scalar giving the unconstrained parameter dimension.
#'
#' @keywords internal
refit_inv_metric_dimension <- function(inv_metric) {
  if(is.matrix(inv_metric)) {
    if(nrow(inv_metric) != ncol(inv_metric)) {
      stop("Each dense inverse metric must be a square matrix.", call. = FALSE)
    }
    return(nrow(inv_metric))
  }

  checkmate::assert_numeric(inv_metric, any.missing = FALSE, null.ok = FALSE)
  length(inv_metric)
}

#' Summarize object shape for comparison
#'
#' @description
#' Return a compact shape descriptor for an object: scalars and vectors are
#' represented by their length, while arrays and matrices are represented by
#' their full dimensions. This is used when comparing warm-start init values
#' with the expected parameter dimensions.
#'
#' @param x Object whose shape should be summarized.
#'
#' @return An integer vector describing the shape of `x`.
#'
#' @keywords internal
refit_object_shape <- function(x) {
  if(is.null(dim(x))) {
    return(length(x))
  }
  dim(x)
}

#' Describe the source of expected parameter dimensions
#'
#' @description
#' Build a short label for error messages that explains whether warm-start
#' validation is comparing against the stored fit in `x` or against parameter
#' dimensions rebuilt from changed `polls_data`.
#'
#' @param changed_polls_data Logical indicating whether `polls_data` changed.
#'
#' @return Character scalar used in warm-start validation errors.
#'
#' @keywords internal
refit_parameter_dimension_source_label <- function(changed_polls_data) {
  if(isTRUE(changed_polls_data)) {
    return("the refit model dimensions built from 'polls_data'")
  }
  "the stored parameter dimensions in 'x'"
}

#' Explain parameter-dimension changes in warm-start errors
#'
#' @description
#' Build the part of a validation error message that explains how the number of
#' unconstrained parameters changed between the stored fit and the refit.
#'
#' @param changed_polls_data Logical indicating whether `polls_data` changed.
#' @param old_num_upars Number of unconstrained parameters in `x`.
#' @param new_num_upars Number of unconstrained parameters implied by the refit.
#'
#' @return Character scalar, or `""` when no extra hint is needed.
#'
#' @keywords internal
refit_parameter_dimension_change_hint <- function(changed_polls_data,
                                                  old_num_upars = NULL,
                                                  new_num_upars = NULL) {
  if(!isTRUE(changed_polls_data) ||
     is.null(old_num_upars) ||
     is.null(new_num_upars) ||
     identical(old_num_upars, new_num_upars)) {
    return("")
  }
  paste0(
    "'x' has ", old_num_upars,
    " unconstrained parameter(s) but the refit model has ", new_num_upars,
    ". This usually means the changed polls_data altered parameter dimensions. "
  )
}

#' Rebuild refit parameter dimensions for warm-start checks
#'
#' @description
#' Construct Stan data for the refit inputs and run a minimal one-chain RStan
#' fit to recover the unconstrained parameter count and init skeleton implied by
#' the refit model. This is used only for early validation when changed
#' `polls_data` requires checking whether warm-start values still fit.
#'
#' @param constructor_args Named constructor argument list for the refit call.
#'
#' @return A list with elements `num_upars` and `init_skeleton`.
#'
#' @keywords internal
refit_build_expected_parameter_dimensions <- function(constructor_args) {
  checkmate::assert_list(constructor_args, names = "named")

  model_context <- refit_model_context(constructor_args$model)
  stan_data <- stan_polls_data(
    x = constructor_args$polls_data,
    y_name = constructor_args$y,
    model = model_context$model_name,
    time_scale = constructor_args$time_scale,
    time_scale_overrides = constructor_args$time_scale_overrides,
    known_state = constructor_args$known_state,
    model_time_range = constructor_args$model_time_range,
    latent_time_ranges = constructor_args$latent_time_ranges,
    hyper_parameters = constructor_args$hyper_parameters,
    slow_scales = constructor_args$slow_scales
  )

  expected_fit <- suppressWarnings(
    utils::capture.output(
      fit <- rstan::stan(
        file = model_context$stan_file,
        model_name = model_context$model_name,
        data = stan_data$stan_data,
        iter = 1,
        warmup = 0,
        chains = 1,
        refresh = 0
      )
    )
  )
  rm(expected_fit)

  list(
    num_upars = backend_get_num_upars("rstan", fit),
    init_skeleton = backend_get_init_skeleton("rstan", fit)
  )
}

#' Resolve Stan model context for expected-dimension validation
#'
#' @description
#' Resolve the Stan file path and model name needed for a minimal fit that
#' recovers the expected parameter dimensions for refit validation.
#' The input may be either a built-in `poll_of_polls` model name or an explicit
#' path to a `.stan` file.
#'
#' @param model Model identifier or `.stan` file path.
#'
#' @return A list with elements `stan_file` and `model_name`.
#'
#' @keywords internal
refit_model_context <- function(model) {
  checkmate::assert_string(model)

  if(checkmate::test_file_exists(model, extension = "stan")) {
    return(list(
      stan_file = model,
      model_name = remove_file_extension(basename(model))
    ))
  }

  checkmate::assert_choice(model, choices = supported_pop_models())
  list(
    stan_file = get_pop_stan_model_file_path(model),
    model_name = model
  )
}

#' @keywords internal
assert_refit_last_draws_complete_for_init <- function(x, init) {
  assert_pop(x)
  checkmate::assert_list(init)

  expected <- refit_stored_warm_start_field(x, "init_skeleton")
  if(is.null(expected)) {
    expected <- backend_get_init_skeleton(x$backend, x$stan_fit)
  }
  expected <- refit_nonempty_init_skeleton(expected)
  if(length(init) != length(expected)) {
    stop(
      "Automatic init reuse requires one complete last draw per chain. ",
      "Expected ", length(expected), " chain(s) but found ", length(init), ".",
      call. = FALSE
    )
  }

  missing_roots <- unlist(lapply(seq_along(expected), function(chain_id) {
    missing <- setdiff(names(expected[[chain_id]]), names(init[[chain_id]]))
    if(length(missing) == 0L) {
      return(character(0))
    }
    paste0("chain ", chain_id, ": ", missing)
  }), use.names = FALSE)

  if(length(missing_roots) > 0L) {
    stop(
      "Automatic init reuse requires a complete last draw in 'x'. ",
      "Missing parameter roots: ",
      paste0(missing_roots, collapse = ", "),
      ". Disable init reuse with warm_start = list(init = NULL) or refit the original model with all parameters needed for initialization saved.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' @keywords internal
refit_metric_type_from_inv_metric <- function(inv_metric) {
  if(is.list(inv_metric)) {
    metric_types <- unique(vapply(inv_metric, backend_metric_type_from_inv_metric, character(1)))
    if(length(metric_types) != 1L) {
      stop("All inverse metrics must use the same metric type.", call. = FALSE)
    }
    return(metric_types[[1]])
  }
  backend_metric_type_from_inv_metric(inv_metric)
}

#' @keywords internal
assert_metric_type_matches_inv_metric <- function(metric_type, inv_metric) {
  checkmate::assert_choice(metric_type, choices = c("diag_e", "dense_e", "unit_e"))
  if(metric_type == "unit_e") {
    stop(
      "'metric = \"unit_e\"' is incompatible with a supplied inverse metric.",
      call. = FALSE
    )
  }
  inferred_metric_type <- refit_metric_type_from_inv_metric(inv_metric)
  if(!identical(metric_type, inferred_metric_type)) {
    stop(
      "The supplied 'metric' does not match the shape of 'inv_metric'. ",
      "Expected '", inferred_metric_type, "'.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' @keywords internal
refit_default <- function(x, default) {
  if(is.null(x)) default else x
}
