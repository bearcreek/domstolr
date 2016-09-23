#' @import parallelMap

.onAttach = function(libname, pkgname) {
  # ...
  parallelRegisterLevels(package = "domstolr", levels = c("file", "case"))
}
