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
