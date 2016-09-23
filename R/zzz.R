#' @import parallelMap
#' @import xml2

.onAttach = function(libname, pkgname) {
  # ...
  parallelRegisterLevels(package = "domstolr", levels = c("file", "case"))
}
