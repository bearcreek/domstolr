#' Import selections downloaded as html from Lovdata Pro (C).
#'
#' Import selections downloaded as html from Lovdata Pro (C).
#'
#' @importFrom parallelMap parallelMap
#' @importFrom rvest read_html html_nodes html_table
#' @importFrom xml2 xml_find_all
#' @importFrom tidyr spread fill
#' @importFrom dplyr rename bind_rows group_by mutate filter ungroup
#' @importFrom magrittr %>%
#'
#' @param file Single file to import.
#' @param directory Directory to import files from.
#' @param regex Regular Expression to use when searching for files within directory. Default is to extract all html files.
#' @param recursive If TRUE then it will also search for files within subfolders of the provided directory.
#' @param meta_only If TRUE it will only return the data within the header table of each case.
#' @param verbose If TRUE it will print out dots for each parsed case to signal how far the function have come.
#' @keywords domstolr lovdata
#'
#' @examples
#'\dontrun{
#' domstol_data <- domstolr_import(directiory = "data/")
#' save(data, file = "domstol_data.RData")
#' }
#' @export
domstolr_import <- function(file = NULL, directory = NULL, regex = ".*.html$", recursive = TRUE,
                            meta_only = FALSE, verbose = FALSE) {

  old_encodingopt <- options()$encoding
  options(encoding = "UTF-8")

  if (!is.null(directory)) {
    file  <- list.files(path = gsub("^/+", "", directory), recursive = recursive,
                        pattern = regex, full.names = TRUE)
    if (length(file) == 0) stop("Unable to find any files.")
  }

  data_all <- parallelMap::parallelMap(extract_data,
                          file = file,
                          more.args = list(meta_only = meta_only, verbose = verbose))  # , level = "file")

  if (meta_only) {
    out <- as.data.frame(bind_rows(data_all))
    class(out) <- c("data.frame", "domstolr")
    return(out)
  }

  out <- lapply(data_all, function(x) x$data_case)
  out <- do.call("rbind", out)

  data_references <- lapply(data_all, function(x) x[[2]])
  meta_data <- lapply(1:length(data_references[[1]]), function(x) lapply(data_references, function(y) y[[x]]))
  meta_data <- lapply(meta_data, function(x) as.data.frame(bind_rows(x)))
  names(meta_data) <- names(data_references[[1]])
  attr(out, "meta_data") <- meta_data

  options(encoding = old_encodingopt)

  class(out) <- c("data.frame", "domstolr")
  return(out)
}

extract_data <- function(file, meta_only = FALSE, verbose = FALSE) {

  ## Split the html file into separate html snippets for each case.
  all_cases <- rvest::read_html(file, encoding = "UTF-8")
  all_cases <- rvest::html_nodes(all_cases, "body br ~ div")

  if (verbose) message(paste("\nParsing", length(all_cases), "cases: "), appendLF = FALSE)

  ## Extract meta data, text, and references from the html code.
  ##
  ## The main parse functions that parse the html are in a separate
  ## file (extract-data-1-html.R).
  extract_data_case <- function(.case, meta_only, verbose) {

    all_tables <- xml2::xml_find_all(.case, ".//table")

    data_meta <- all_tables[[1]] %>%
      rvest::html_table() %>%
      dplyr::rename(variable = X1,
             value = X2) %>%
      tidyr::spread(variable, value)

    if (meta_only) return(list(data_meta = data_meta))

    data_extracted_inner <- extract_data_html(.case, data_meta, all_tables, verbose)

    data_references <- lapply(data_extracted_inner, attr, which = ".case_references")
    data_references <- do.call("rbind", data_references)
    data_references$publisert <- data_meta$Publisert

    data_case <- data_extracted_inner %>%
      dplyr::bind_rows() %>%
      tidyr::fill(avsnitt) %>%
      dplyr::group_by(avsnitt) %>%
      dplyr::mutate(tekst = paste0(tekst, collapse = " ")) %>%
      dplyr::filter(!duplicated(tekst)) %>%
      dplyr::ungroup()

    return(list(data_case = data_case, data_references = data_references))
  }

  data_extracted <- parallelMap(extract_data_case,
                                .case = all_cases,
                                more.args = list(meta_only = meta_only, verbose = verbose)) #, level = "case")

  ## Extract and clean case data
  data_case <- lapply(data_extracted, function(x) x$data_case) %>% bind_rows()
  names(data_case) <- gsub(" ", "_", tolower(names(data_case)))
  data_case$dato <- as.Date(data_case$dato)

  ## Extract and clean reference data
  data_references <- lapply(data_extracted, function(x) x$data_references) %>% bind_rows()
  names(data_references) <- gsub(" ", "_", tolower(names(data_references)))

  ## Extract additional meta data from the text and references.
  ##
  ## These functions are in a separate file
  ## (extract-data-2-meta.R). The standard setup is to first update or
  ## extract the data (e.g., list of judges) and then run a test to
  ## check if everything looks ok. There are two main types of
  ## operations. The first is to update or clean the data extracted
  ## from the html (e.g., add a new variable). The second is to use
  ## the data extracted from the html to create new data sets.

  ## Case parties
  data_case <- add_data_parties(data_case)
  data_parties <- extract_data_parties(data_case)

  ## Decision type (Dom/Kjennelse)
  data_case <- add_data_decision_type(data_case)

  ## Case Type (straffe vs sivil sak)
  data_case <- add_data_case_type(data_case)

  ## Case proceedings (case flow)
  data_proceedings <- extract_data_case_proceedings(data_case)

  ## Judges
  data_judges <- extract_data_judges(data_case)

  ## Keywords
  data_keywords <- extract_data_keywords(data_case)

  ## Section: Extract properties of the text, such as which judges
  ## that are speaking.
  data_case <- add_data_section(data_case)

  ## Return all data
  out <- list(data_case = data_case,
              attached = list(data_references = data_references,
                              data_parties = data_parties,
                              data_proceedings = data_proceedings,
                              data_judges = data_judges,
                              data_keywords = data_keywords))
  return(out)
}
