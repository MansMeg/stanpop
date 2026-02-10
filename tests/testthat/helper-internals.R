
# Helper to access non-exported functions in a controlled way
get_internal <- function(name, pkg = "stanpop") {
  getFromNamespace(name, pkg)
}
