#' Rerun a poll_of_polls object without warm-start reuse
#'
#' @description
#' `rerun_poll_of_polls()` is a backward-compatible wrapper around
#' [refit_poll_of_polls()]. It uses the refit implementation but disables
#' automatic warm-start reuse by default, so the rerun behaves like a plain
#' re-estimation unless you call [refit_poll_of_polls()] directly.
#'
#' All arguments in `...` are forwarded to [refit_poll_of_polls()] except
#' `warm_start`, which is intentionally not supported here.
#'
#' @param x Existing [poll_of_polls] object to rerun.
#' @param ... Arguments forwarded to [refit_poll_of_polls()], such as
#'   `polls_data`, `backend`, `compile_args`, `cache_dir`, and backend sampler
#'   arguments.
#'
#' @return A rerun [poll_of_polls] object.
#' @export
rerun_poll_of_polls <- function(x, ...) {
  dots <- list(...)

  if(length(dots) > 0L && "warm_start" %in% names(dots)) {
    stop(
      "rerun_poll_of_polls() does not accept 'warm_start'. ",
      "Use refit_poll_of_polls() for explicit warm-start control.",
      call. = FALSE
    )
  }

  do.call(
    refit_poll_of_polls,
    c(
      list(
        x = x,
        warm_start = list(
          init = NULL,
          inv_metric = NULL,
          step_size = NULL
        )
      ),
      dots
    )
  )
}
