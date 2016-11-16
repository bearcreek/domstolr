#' Extract data as data frame from domstolr data
#'
#' Extract data as data frame from domstolr data
#'
#' @param data A domstolr object (from \code{domstolr_import()}).
#' @param type Which data frame to extract (cases, paragraphs,
#'   references, parties, proceedings, judges, or keywords).
#' @keywords domstolr extract cases paragraps references parties
#'   proceedings judges keywords
#'
#' @examples
#'\dontrun{
#' domstol_data <- domstolr_import(directiory = "data/")
#' cases <- get_cases(domstol_data)
#' }
#' @export
get_data <- function(data, type = "cases") {
  func <- get(paste0("get_", type))
  return(func(data))
}

#' @rdname get_data
#' @export
get_cases <- function(data) as.data.frame(data$data_cases)

#' @rdname get_data
#' @export
get_paragraphs <- function(data) as.data.frame(data$data_paragraphs)

#' @rdname get_data
#' @export
get_references <- function(data) as.data.frame(data$data_references)

#' @rdname get_data
#' @export
get_parties <- function(data) as.data.frame(data$data_parties)

#' @rdname get_data
#' @export
get_proceedings <- function(data) as.data.frame(data$data_proceedings)

#' @rdname get_data
#' @export
get_judges <- function(data) as.data.frame(data$data_judges)

#' @rdname get_data
#' @export
get_keywords <- function(data) as.data.frame(data$data_keywords)
