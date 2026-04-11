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
