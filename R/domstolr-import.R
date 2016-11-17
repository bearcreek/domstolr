#' Import selections downloaded as html from Lovdata Pro (C)
#'
#' Import selections downloaded as html from Lovdata Pro (C)
#'
#' @importFrom parallelMap parallelMap
#' @importFrom rvest html_nodes html_table html_text html_attr
#' @importFrom xml2 xml_find_all read_html
#' @importFrom tidyr spread fill unnest
#' @importFrom dplyr rename bind_rows group_by mutate filter ungroup
#'   matches data_frame bind_cols summarize slice select
#' @importFrom readr read_lines
#' @importFrom fuzzyjoin stringdist_left_join
#' @importFrom magrittr %>%
#'
#' @param file Single file to import.
#' @param directory Directory to import files from.
#' @param regex Regular Expression to use when searching for files
#'   within directory. Default is to extract all html files.
#' @param recursive If TRUE then it will also search for files within
#'   subfolders of the provided directory.
#' @param meta_only If TRUE it will only return the data within the
#'   header table of each case.
#' @param verbose If TRUE it will print out dots for each parsed case
#'   to signal how far the function have come.
#' @keywords domstolr lovdata
#'
#' @examples
#' \dontrun{
#' domstol_data <- domstolr_import(directiory = "data/")
#' save(domstol_data, file = "domstol_data.RData")
#' }
#' @export
domstolr_import <- function(file = NULL, directory = NULL, regex = ".*.html$", recursive = TRUE,
                            meta_only = FALSE, match_judges = TRUE, verbose = TRUE) {

  if (!is.null(directory)) {
    file  <- list.files(path = gsub("^/+", "", directory), recursive = recursive,
                        pattern = regex, full.names = TRUE)
    if (length(file) == 0) stop("Unable to find any files.")
  }

  data_all <- lapply(file, extract_data, meta_only = meta_only, verbose = verbose, match_judges = match_judges)

  if (meta_only) {
    out <- as.data.frame(dplyr::bind_rows(data_all))
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

  data_list <- list(
    list(name = "cases",
         unit = "case",
         prefix = "case_",
         nobs = nrow(data_cases),
         nvars = ncol(data_cases),
         vars = paste(names(data_cases), collapse = ", ")),
    list(name = "paragraphs",
         unit = "paragraph of text (within case)",
         prefix = "par_",
         nobs = nrow(data_paragraphs),
         nvars = ncol(data_paragraphs),
         vars = paste(names(data_paragraphs), collapse = ", ")),
    list(name = "references",
         unit = "law or case reference (within paragraph of text within case)",
         prefix = "ref_",
         nobs = nrow(data_references),
         nvars = ncol(data_references),
         vars = ""),
    list(name = "judges",
         unit = "judge (within case)",
         prefix = "judge_",
         nobs = nrow(data_judges),
         nvars = ncol(data_judges),
         vars = paste(names(data_judges), collapse = ", ")),
    list(name = "parties",
         unit = "party (within case)",
         prefix = "prt_",
         nobs = nrow(data_parties),
         nvars = ncol(data_parties),
         vars = paste(names(data_parties), collapse = ", ")),
    list(name = "proceedings",
         unit = "institution (within history of case)",
         prefix = "proc_",
         nobs = nrow(data_proceedings),
         nvars = ncol(data_proceedings),
         vars = paste(names(data_proceedings), collapse = ", ")),
    list(name = "keywords",
         unit = "keyword (within case)",
         prefix = "kw_",
         nobs = nrow(data_keywords),
         nvars = ncol(data_keywords),
         vars = paste(names(data_keywords), collapse = ", "))
  )

  out <- list(meta = data_meta,
              meta_data = data_list,
              data_cases = data_cases,
              data_paragraphs = data_paragraphs,
              data_references = data_references,
              data_judges = data_judges,
              data_parties = data_parties,
              data_proceedings = data_proceedings,
              data_keywords = data_keywords)
  class(out) <- "domstolr"

  return(out)
}

## Print and plot methods
#' @export
print.domstolr <- function(data) {
  out_1 <- paste0("Norwegian Supreme Court Data (domstolr class)\n",
                  "files (n): ", prettyNum(length(data$meta$file), bigmark = ",", scientific = FALSE), "\n",
                  "cases (n): ", prettyNum(data$meta$n_cases, bigmark = ",", scientific = FALSE), "\n",
                  "dates (min, max): ", data$meta$date_min, ", ", data$meta$date_max, "\n",
                  "data available: \n")
  out_2 <- lapply(data$meta_data, function(x) {
    paste0(x$name, " (", paste0("get_", x$name, "()"), ")\n",
           "    unit: ", x$unit, "\n",
           "    obs: ", prettyNum(x$nobs, big.mark = ",", scientific = FALSE),
           ", vars: ", prettyNum(x$nvars, bigmark = ",", scientific = FALSE)) #,
  })
  cat(out_1)
  for (i in seq_along(out_2)) cat(paste0("  ", out_2[[i]], "\n"))
}

#' @export
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
      tidyr::spread(variable, value)

    if ("Dato" %in% names(data_meta)) {
      data_meta <- rename(data_meta, case_date = Dato)

      if (data_meta$case_date == "1971-09-00")
        data_meta <- mutate(data_meta, case_date = "1971-09-01")
      if (data_meta$case_date == "1966-03")
        data_meta <- mutate(data_meta, case_date = "1966-03-01")
      if (nchar(data_meta$case_date) == 4)
        data_meta <- mutate(data_meta, case_date = paste0(case_date, "-01-01"))
      if (grepl("[Uu]datert|^1967-00-00$", data_meta$case_date))
        data_meta <- mutate(data_meta, case_date = paste0(substr(case_date, 1, 4), "-1-1"))

      data_meta <- mutate(data_meta, case_date = as.Date(case_date))
    }
    if ("Publisert" %in% names(data_meta))
      data_meta <- rename(data_meta, case_citation = Publisert)

    if ("Forfatter" %in% names(data_meta))
      data_meta <- rename(data_meta, case_judges = Forfatter)

    if ("Instans" %in% names(data_meta))
      data_meta <- rename(data_meta, case_instance = Instans)

    if ("Parter" %in% names(data_meta))
      data_meta <- rename(data_meta, case_parties = Parter)

    if ("Saksgang" %in% names(data_meta))
      data_meta <- rename(data_meta, case_history = Saksgang)

    if ("Sammendrag" %in% names(data_meta))
      data_meta <- rename(data_meta, case_summary = Sammendrag)

    if ("Stikkord" %in% names(data_meta))
      data_meta <- rename(data_meta, case_keywords = Stikkord)

    if (meta_only) return(list(data_meta = data_meta))

    data_extracted_inner <- extract_data_html(.case, data_meta, all_tables, verbose)

    out <- list(data_case = data_extracted_inner$data_case,
                data_references = data_extracted_inner$data_references)
    ## %>%
    ##   dplyr::bind_rows() %>%
    ##   tidyr::fill(paragraph) %>%
    ##   dplyr::group_by(paragraph) %>%
    ##   dplyr::mutate(text = paste0(text, collapse = " ")) %>%
    ##   dplyr::filter(!duplicated(text)) %>%
    ##   dplyr::ungroup() %>%
    ##   dplyr::rename(par_text = text,
    ##                 par_paragraph = paragraph)

    return(out)
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
