#' Import selections downloaded as html from Lovdata Pro (C)
#'
#' Import selections downloaded as html from Lovdata Pro (C)
#'
#' @importFrom parallelMap parallelMap
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 xml_find_all read_html
#' @importFrom tidyr spread fill
#' @importFrom dplyr rename bind_rows group_by mutate filter ungroup matches
#' @importFrom readr read_lines
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
                            meta_only = FALSE, match_judges = TRUE, verbose = FALSE) {

  if (!is.null(directory)) {
    file  <- list.files(path = gsub("^/+", "", directory), recursive = recursive,
                        pattern = regex, full.names = TRUE)
    if (length(file) == 0) stop("Unable to find any files.")
  }

  data_all <- lapply(file, extract_data, meta_only = meta_only, verbose = verbose, match_judges = match_judges)

  if (meta_only) {
    out <- as.data.frame(bind_rows(data_all))
    return(out)
  }

  data_paragraphs <- lapply(data_all, function(x) x$data_case) %>% bind_rows()
  data_references <- lapply(data_all, function(x) x$data_references) %>% bind_rows()
  data_parties <- lapply(data_all, function(x) x$data_parties) %>% bind_rows()
  data_proceedings <- lapply(data_all, function(x) x$data_proceedings) %>% bind_rows()
  data_judges <- lapply(data_all, function(x) x$data_judges) %>% bind_rows()
  data_keywords <- lapply(data_all, function(x) x$data_keywords) %>% bind_rows()


  casevars <- grep("^case_", names(data_paragraphs), value = TRUE)
  data_cases <- data_paragraphs %>%
    dplyr::group_by_(.dots = casevars) %>%
    dplyr::summarize(case_text = paste0(par_text, collapse = " ")) %>%
    dplyr::ungroup()

  data_meta <- list(imported_on = Sys.time(),
                    file = file,
                    meta_only = meta_only,
                    match_judges = match_judges,
                    n_cases = nrow(data_cases),
                    n_judges = length(unique(data_judges$judge_name)),
                    date_min = min(data_cases$case_date),
                    date_max = max(data_cases$case_date))


  out <- list(meta = data_meta,
              data_cases = data_cases,
              data_paragraphs = data_paragraphs,
              data_parties = data_parties,
              data_proceedings = data_proceedings,
              data_judges = data_judges,
              data_keywords = data_keywords)
  class(out) <- "domstolr"

  return(out)
}

## Print and plot methods
print.domstolr <- function(data) {
  dta <- gsub("data_", "", names(data)[grep("data_", names(data))])
  cat("Norwegian Supreme Court Data (domstolr)\n", append = TRUE)
  cat(paste0("N Files: ",  length(data$meta$file), "\n"))
  cat(paste0("N Cases: ", data$meta$n_cases, "\n"))
  cat(paste0("N Judges: ", data$meta$n_judges, "\n"))
  cat(paste0("Min date: ", data$meta$date_min, "\n"))
  cat(paste0("Max date: ", data$meta$date_max, "\n"))
  cat(paste0("Available data: ", paste0(dta, collapse = ", "), "\n"))
  cat(paste0("(run get_*data*() to extract the df, e.g., get_", dta[1], "().)"))
  cat("\n")
}

plot.domstolr <- function(data) {
  hist_data <- as.Date(data$data_cases$case_date)
  brks <- ifelse(length(hist_data) > 500, "q", "month")
  hist(x = hist_data,
       breaks = brks,
       freq = TRUE,
       main = paste0("Number of cases by ", ifelse(brks == "q", "quarter", "month")),
       xlab = "Date",
       ylab = "Number of cases")
}

## Parse function ran on file
extract_data <- function(file, meta_only = FALSE, verbose = FALSE, match_judges = TRUE) {

  ## Split the html file into separate html snippets for each case.
  all_cases <- file %>%
    readr::read_lines() %>%
    xml2::read_html(encoding = "UTF-8") %>%
    rvest::html_nodes("body br ~ div")

  if (verbose) message(sprintf("\nFile: %s\nN Cases: %s\nNow parsing (one dot is one case).", file, length(all_cases)))

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
      tidyr::spread(variable, value) %>%
      rename(case_date = Dato,
             case_citation = Publisert,
             case_judges = Forfatter,
             case_instance = Instans,
             case_parties = Parter,
             case_history = Saksgang,
             case_summary = Sammendrag,
             case_keywords = Stikkord) %>%
      mutate(case_date = as.Date(case_date))

    if (meta_only) return(list(data_meta = data_meta))

    data_extracted_inner <- extract_data_html(.case, data_meta, all_tables, verbose)

    data_references <- attr(data_extracted_inner, which = ".case_references") %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(case_citation = data_meta$case_citation)

    data_case <- data_extracted_inner %>%
      dplyr::bind_rows() %>%
      tidyr::fill(paragraph) %>%
      dplyr::group_by(paragraph) %>%
      dplyr::mutate(text = paste0(text, collapse = " ")) %>%
      dplyr::filter(!duplicated(text)) %>%
      dplyr::ungroup() %>%
      rename(par_text = text,
             par_paragraph = paragraph)

    return(list(data_case = data_case, data_references = data_references))
  }

  data_extracted <- parallelMap(extract_data_case,
                                .case = all_cases,
                                more.args = list(meta_only = meta_only, verbose = verbose))

  if (verbose) message(paste("\nFinished going through the html.",
                             "Now extracting additional data.\n"),
                       appendLF = FALSE)

  ## Extract and clean case/par data and references
  data_case <- lapply(data_extracted, function(x) x$data_case) %>% bind_rows()
  data_references <- lapply(data_extracted, function(x) x$data_references) %>% bind_rows()

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
  data_judges <- extract_data_judges(data_case, match_judges = match_judges)

  ## Keywords
  data_keywords <- extract_data_keywords(data_case)

  ## Section: Extract properties of the text, such as which judges
  ## that are speaking.
  data_case <- add_data_section(data_case, data_judges)

  ## Return all data
  if (verbose) message("Done.\n", appendLF = FALSE)
  out <- list(data_case = data_case,
              data_references = data_references,
              data_parties = data_parties,
              data_proceedings = data_proceedings,
              data_judges = data_judges,
              data_keywords = data_keywords)
  return(out)
}
