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
#'   Supported elements are `init`, `inv_metric`, `metric`, and `step_size`.
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
#'   Warm-start controls such as `init`, `inv_metric`, `metric`, and
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
