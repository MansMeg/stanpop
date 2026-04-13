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
  if(!is_save_pop_payload(x)) {
    stop("File was not created by save_pop().", call. = FALSE)
  }

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

#' Test whether an object matches the save_pop wrapper format
#'
#' @description
#' Internal structural validator used by [load_pop()] to distinguish files
#' created by [save_pop()] from plain `saveRDS()` output or unrelated serialized
#' objects.
#'
#' @param x Object read from disk.
#'
#' @return Logical scalar indicating whether `x` matches the expected wrapped
#'   payload structure.
#' @keywords internal
is_save_pop_payload <- function(x) {
  is.list(x) &&
    identical(x$format, "stanpop_pop") &&
    identical(x$format_version, 1L) &&
    "saved_at" %in% names(x) &&
    "stanpop_version" %in% names(x) &&
    "backend" %in% names(x) &&
    "pop" %in% names(x) &&
    inherits(x$pop, "poll_of_polls") &&
    identical(x$backend, x$pop$backend)
}
