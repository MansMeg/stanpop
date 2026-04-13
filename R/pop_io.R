#' Save a poll_of_polls object
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
  assert_pop_save_compress(compress)

  saveRDS(
    object = pop_save_payload(x),
    file = file,
    compress = compress
  )

  invisible(x)
}

#' Load a poll_of_polls object saved with save_pop
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

pop_backend_package_version <- function(backend) {
  assert_pop_backend(backend)

  if(!requireNamespace(backend, quietly = TRUE)) {
    return(NA_character_)
  }

  as.character(utils::packageVersion(backend))
}

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
