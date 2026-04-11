#' Refit a poll_of_polls object
#'
#' @description
#' `refit_poll_of_polls()` reconstructs a call to [poll_of_polls()] from an
#' existing [poll_of_polls] object. The public API is intentionally small:
#' supply the existing object `x`, optionally choose a `backend`, and use `...`
#' for any overrides.
#'
#' Named entries in `...` are interpreted in two groups:
#'
#' 1. Names matching [poll_of_polls()] constructor arguments, except `y`,
#'    `model`, and `backend`, override the stored model inputs.
#' 2. All other names in `...` are treated as backend sampler arguments.
#'
#' Constructor arguments inherit from `x` when omitted. Supplying a value in
#' `...` replaces the stored value, including `NULL` for arguments where
#' clearing the stored value is meaningful, such as `known_state`,
#' `compile_args`, or `cache_dir`.
#'
#' Sampler arguments also inherit from `x` and can be overridden or removed via
#' `...`. Named `NULL` values remove stored sampler arguments.
#'
#' Warm starts are filled in automatically unless you override them in
#' `warm_start`. Omitted `warm_start` elements are taken from the last draw and
#' sampler state of `x`.
#'
#' The original `y` and `model` are always inherited from `x`; use
#' [poll_of_polls()] directly to change them.
#'
#' @param x Existing [poll_of_polls] object to refit.
#' @param warm_start A named list controlling warm-start initialization.
#'   Supported elements are `init`, `inv_metric`, `metric_type`, and
#'   `step_size`.
#'   Omitted elements use the automatic defaults extracted from `x`.
#'   Set an element to `NULL` to disable that default warm-start component.
#'   For example, `warm_start = list(inv_metric = NULL)` reuses the last draw
#'   but does not reuse the inverse metric.
#' @param backend Stan backend to use for the refit. Only `backend = "cmdstanr"`
#'   is currently supported because RStan does not accept a supplied mass
#'   matrix.
#'   The argument remains explicit so a future version can extend backend
#'   support without changing the public API.
#' @param ... Named overrides. Names matching [poll_of_polls()] constructor
#'   arguments, except `y`, `model`, and `backend`, override stored model
#'   inputs. All other names are treated as backend sampler arguments.
#'   Warm-start controls such as `init`, `inv_metric`, `metric_type`, and
#'   `step_size` must be supplied through `warm_start`, not `...`.
#'
#' @return A refitted [poll_of_polls] object.
#' @export
refit_poll_of_polls <- function(x, warm_start = list(), backend = "cmdstanr", ...) {
  dots <- list(...)

  resolved <- resolve_refit_poll_of_polls_arguments(
    x = x,
    backend = backend,
    dots = dots,
    warm_start = warm_start
  )
  resolved$sample_args <- refit_materialize_warm_start_arguments(
    x = x,
    backend = resolved$constructor_args$backend,
    sample_args = resolved$sample_args,
    warm_start = resolved$warm_start
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
#' Split a refit call into the pieces needed to rebuild a [poll_of_polls()]
#' call from an existing object. This helper validates the requested backend,
#' separates constructor overrides from backend sampler overrides, normalizes
#' the `warm_start` specification, and returns the inherited constructor and
#' sampler arguments after explicit overrides have been applied. Automatic
#' warm-start defaults are not inserted here; they are added later by
#' [refit_materialize_warm_start_arguments()].
#'
#' @keywords internal
resolve_refit_poll_of_polls_arguments <- function(x,
                                                  backend,
                                                  dots,
                                                  warm_start) {
  assert_pop(x)
  assert_refit_poll_of_polls_backend(backend)
  checkmate::assert_list(dots, null.ok = TRUE)
  warm_start <- normalize_refit_warm_start(warm_start)
  if(length(dots) > 0L && (is.null(names(dots)) || any(names(dots) == ""))) {
    stop("All overrides in '...' must be named.", call. = FALSE)
  }

  # Check the that inputs are correct
  disallowed_override_names <- intersect(names(dots), c("y", "model", "backend"))
  if(length(disallowed_override_names) > 0L) {
    stop(
      "refit_poll_of_polls() always inherits 'y' and 'model' from 'x'. ",
      "Supply 'backend' as an explicit argument, not through '...'. ",
      "Use poll_of_polls() directly to change 'y' or 'model'.",
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


  # Split refit overrides into poll_of_polls() inputs versus rstan/cmdstanr sampler args.
  constructor_override_names <- intersect(
    names(dots),
    refit_constructor_argument_names()
  )
  constructor_overrides <- dots[constructor_override_names]
  sample_overrides <- dots[setdiff(names(dots), constructor_override_names)]

  # Rebuild the poll_of_polls() constructor args from x, then apply explicit refit overrides.
  constructor_args <- unclass(extract_poll_of_polls_refit_arguments(x))
  for(name in names(constructor_overrides)) {
    constructor_args[name] <- list(constructor_overrides[[name]])
  }
  constructor_args$backend <- backend

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
    sample_overrides = sample_overrides
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
refit_constructor_argument_names <- function() {
  setdiff(
    names(formals(poll_of_polls)),
    c("...", "y", "model", "backend")
  )
}

#' @keywords internal
refit_warm_start_argument_names <- function() {
  c("init", "inv_metric", "metric_type", "step_size")
}

#' Normalize explicit warm-start overrides for a refit
#'
#' @description
#' Validate and normalize the `warm_start` list supplied to
#' [refit_poll_of_polls()]. The helper requires a named list, rejects duplicate
#' or unsupported element names, and enforces the public `metric_type` name
#' rather than the backend sampler argument name `metric`.
#'
#' An empty list means that `refit_poll_of_polls()` should fall back to its
#' automatic warm-start defaults. Named `NULL` entries are preserved so callers
#' can explicitly disable a default warm-start component later in the refit
#' pipeline.
#'
#' @param warm_start A named list of explicit warm-start overrides. Supported
#'   elements are `init`, `inv_metric`, `metric_type`, and `step_size`.
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
  warm_start
}

#' @keywords internal
refit_remove_inherited_warm_start_arguments <- function(sample_args) {
  checkmate::assert_list(sample_args, names = "named")
  sample_args[c("init", "inv_metric", "step_size", "metric_file")] <- NULL
  sample_args
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
#' `x`: the final constrained draw is reused for `init`, while the sampler
#' state supplies `inv_metric` and `step_size`. When an inverse metric is
#' present and no explicit `metric_type` is supplied, the metric type is
#' inferred from the shape of that inverse metric. Named `NULL` entries in
#' `warm_start` explicitly disable the corresponding warm-start component.
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
#'   Supported elements are `init`, `inv_metric`, `metric_type`, and
#'   `step_size`.
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

  state <- NULL
  last_draws <- NULL

  get_sampler_state <- function() {
    if(is.null(state)) {
      if(is.null(x$stan_fit)) {
        stop("The stored fit is missing, so sampler state cannot be reused.", call. = FALSE)
      }
      state <- backend_get_sampler_state(x$backend, x$stan_fit)
    }
    state
  }

  get_last_draws <- function() {
    if(is.null(last_draws)) {
      if(is.null(x$stan_fit)) {
        stop("The stored fit is missing, so init values cannot be reused.", call. = FALSE)
      }
      last_draws <- backend_get_last_draws_for_init(x$backend, x$stan_fit)
    }
    last_draws
  }

  # Handle init
  if("init" %in% names(warm_start)) {
    sample_args <- refit_set_named_argument(sample_args, "init", warm_start$init)
  } else {
    sample_args <- refit_set_named_argument(sample_args, "init", get_last_draws())
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
      refit_sampler_state_field(get_sampler_state(), "inv_metric")
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
      refit_assert_metric_type_matches_inv_metric(
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
      refit_sampler_state_field(get_sampler_state(), "step_size", simplify = TRUE)
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
