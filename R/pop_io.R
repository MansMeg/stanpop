#' Save a poll_of_polls object
#'
#' @description
#' Save a [poll_of_polls] object in stanpop's wrapped on-disk format. The file
#' contains a small metadata wrapper around the original object so
#' [load_pop()] can verify that the file was created by [save_pop()] rather
#' than by a plain [base::saveRDS()] call.
#'
#' @param x A [poll_of_polls] object.
#' @param file Path to the output `.rds` file.
#' @param compress Compression passed to [base::saveRDS()].
#'
#' @return Invisibly returns `x`.
#' @export
save_pop <- function(x, file, compress = "xz") {
  assert_pop(x)
  checkmate::assert_path_for_output(file)
  x <- prepare_pop_for_save(x)

  saveRDS(
    object = pop_save_payload(x),
    file = file,
    compress = compress
  )

  invisible(x)
}

#' Load a poll_of_polls object saved with save_pop
#'
#' @description
#' Read a wrapped `.rds` file created by [save_pop()] and return the stored
#' [poll_of_polls] object. Files not created by [save_pop()] are rejected so
#' the loader can rely on the wrapper metadata and avoid silently accepting
#' arbitrary serialized objects.
#'
#' @param file Path to a file previously created by [save_pop()].
#'
#' @return A [poll_of_polls] object.
#' @export
load_pop <- function(file) {
  checkmate::assert_file(file)

  x <- readRDS(file = file)
  assert_save_pop_payload(x)

  x$pop
}

#' Prepare a poll_of_polls object for serialization
#'
#' @description
#' Internal backend-dispatch helper used by [save_pop()]. It gives each backend
#' a chance to normalize or materialize backend-specific state before the object
#' is wrapped and serialized to disk.
#'
#' @param x A [poll_of_polls] object.
#'
#' @return A [poll_of_polls] object ready to be passed to [pop_save_payload()].
#' @keywords internal
prepare_pop_for_save <- function(x) {
  assert_pop(x)
  assert_pop_backend(x$backend)

  if(identical(x$backend, "rstan")) {
    return(x)
  }
  if(identical(x$backend, "cmdstanr")) {
    return(prepare_cmdstanr_pop_for_save(x))
  }

  stop("Unknown backend '", x$backend, "'.", call. = FALSE)
}

#' Prepare a cmdstanr-backed poll_of_polls object for serialization
#'
#' @description
#' Internal helper for backend-specific pre-save handling of cmdstanr-backed
#' [poll_of_polls] objects. It materializes the CmdStanR fit into a
#' self-contained R object using CmdStanR's public `$save_object()` API and
#' then replaces `x$stan_fit` with the reloaded object before serialization.
#'
#' @param x A cmdstanr-backed [poll_of_polls] object.
#'
#' @return A [poll_of_polls] object whose `stan_fit` element is ready for
#'   serialization without depending on the original CmdStan output files.
#' @keywords internal
prepare_cmdstanr_pop_for_save <- function(x) {
  assert_pop(x)
  if(!identical(x$backend, "cmdstanr")) {
    stop("prepare_cmdstanr_pop_for_save() requires backend = 'cmdstanr'.", call. = FALSE)
  }
  if(is.null(x$stan_fit)) {
    stop("cmdstanr-backed poll_of_polls objects must contain 'stan_fit'.", call. = FALSE)
  }

  x$stan_fit <- materialize_cmdstanr_fit_for_save(x$stan_fit)

  x
}

#' Materialize a CmdStanR fit for serialization
#'
#' @description
#' Convert a CmdStanR fit that may still rely on external CmdStan output files
#' into a self-contained R object by roundtripping through CmdStanR's public
#' `$save_object()` method and [base::readRDS()]. This keeps the serialization
#' logic aligned with CmdStanR's supported save path while returning the
#' materialized fit object to stanpop.
#'
#' @param fit A CmdStanR fit object providing a `$save_object()` method.
#'
#' @return A materialized CmdStanR fit object read back from a temporary `.rds`
#'   file.
#' @keywords internal
materialize_cmdstanr_fit_for_save <- function(fit) {
  assert_cmdstanr_available()
  if(is.null(fit$save_object) || !is.function(fit$save_object)) {
    stop("CmdStanR fit does not provide a '$save_object()' method.", call. = FALSE)
  }

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  fit$save_object(file = tmp)
  readRDS(tmp)
}


#' Build the wrapped payload saved by save_pop
#'
#' @description
#' Construct the versioned wrapper written to disk by [save_pop()]. The wrapper
#' stores metadata about the serialization format, the stanpop version, the
#' backend used to fit the model, and the original [poll_of_polls] object.
#'
#' @param x A [poll_of_polls] object.
#'
#' @return A named list containing serialization metadata and the original pop
#'   object in the `pop` field.
#' @keywords internal
pop_save_payload <- function(x) {
  assert_pop(x)

  list(
    format = "stanpop_pop",
    format_version = 1L,
    saved_at = Sys.time(),
    stanpop_version = as.character(utils::packageVersion("stanpop")),
    backend = x$backend,
    backend_package_version = pop_backend_package_version(x$backend),
    pop = x
  )
}

#' Return the installed version of a Stan backend package
#'
#' @description
#' Look up the installed version string for a supported Stan backend package. If
#' the package is not installed in the current R session, return `NA_character_`
#' instead of failing so the save payload can still be created.
#'
#' @param backend Name of a supported Stan backend.
#'
#' @return A character scalar giving the installed package version, or
#'   `NA_character_` if the package is unavailable.
#' @keywords internal
pop_backend_package_version <- function(backend) {
  assert_pop_backend(backend)

  if(!requireNamespace(backend, quietly = TRUE)) {
    return(NA_character_)
  }

  as.character(utils::packageVersion(backend))
}

#' Validate a wrapped save_pop payload
#'
#' @description
#' Internal validator for the on-disk wrapper format created by [save_pop()].
#' The helper checks both the wrapper metadata and the embedded `pop` object and
#' throws targeted errors for malformed or unsupported payloads.
#'
#' @param x Object read from disk.
#'
#' @return Invisibly returns [TRUE] when `x` is a valid wrapped payload.
#' @keywords internal
assert_save_pop_payload <- function(x) {
  if(!is.list(x)) {
    stop("Loaded object is not a valid save_pop() payload.", call. = FALSE)
  }
  if(!"format" %in% names(x)) {
    stop("Missing 'format' field in save_pop() payload.", call. = FALSE)
  }
  if(!identical(x$format, "stanpop_pop")) {
    stop(
      "Unsupported save_pop() payload format '", x$format,
      "'. Expected 'stanpop_pop'.",
      call. = FALSE
    )
  }
  if(!"format_version" %in% names(x)) {
    stop("Missing 'format_version' field in save_pop() payload.", call. = FALSE)
  }
  if(!identical(x$format_version, 1L)) {
    stop(
      "Unsupported save_pop() format version '", x$format_version,
      "'. Expected '1'.",
      call. = FALSE
    )
  }
  if(!"saved_at" %in% names(x)) {
    stop("Missing 'saved_at' field in save_pop() payload.", call. = FALSE)
  }
  if(!"stanpop_version" %in% names(x)) {
    stop("Missing 'stanpop_version' field in save_pop() payload.", call. = FALSE)
  }
  if(!"backend" %in% names(x)) {
    stop("Missing 'backend' field in save_pop() payload.", call. = FALSE)
  }
  if(!"pop" %in% names(x)) {
    stop("Missing 'pop' field in save_pop() payload.", call. = FALSE)
  }
  if(!inherits(x$pop, "poll_of_polls")) {
    stop("The 'pop' field is not a poll_of_polls object.", call. = FALSE)
  }
  if(!identical(x$backend, x$pop$backend)) {
    stop("The payload backend does not match pop$backend.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Test whether an object matches the save_pop wrapper format
#'
#' @description
#' Internal predicate wrapper around [assert_save_pop_payload()]. This is useful
#' in tests and other code paths that want a boolean check rather than an error.
#'
#' @param x Object read from disk.
#'
#' @return Logical scalar indicating whether `x` matches the expected wrapped
#'   payload structure.
#' @keywords internal
is_save_pop_payload <- function(x) {
  isTRUE(tryCatch(
    {
      assert_save_pop_payload(x)
      TRUE
    },
    error = function(...) FALSE
  ))
}
