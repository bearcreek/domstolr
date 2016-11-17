## ' Extract data from the html code
## '
## ' Takes the html code from one case and extracts text and
## ' references. The html must be parsed differently depending on when
## ' the case is from.
## '
## ' @importFrom rvest html_text html_attr
## ' @importFrom dplyr data_frame mutate bind_cols bind_rows group_by filter summarize ungroup
## ' @importFrom xml2 xml_find_all

extract_data_html <- function(.case, data_meta, all_tables, verbose) {

  if (verbose) message(".", appendLF = FALSE)

  ## First find what method to use
  date <- as.numeric(gsub("^(\\d{4}).*", "\\1", data_meta$case_date))
  if (date >= 2003)
    class(.case) <- c("sc_after_2003", class(.case))
  else
    class(.case) <- c("sc_before_2003", class(.case))

  ## Then use it
  UseMethod("extract_data_html", .case)
}

## Cases after 2003 are split into tables where each table is a
## official paragraph within the court decision
extract_data_html.sc_after_2003 <- function(.case, data_meta, all_tables, ...) {

  if (length(all_tables) == 1) {
    data_case <- dplyr::data_frame(par_paragraph = 0,
                            par_text = "")
    data_case <- suppressWarnings(cbind(data_case, data_meta))
    data_references <- NULL
    out <- list(data_case = data_case,
                data_references = data_references)
    return(out)
  }

  data_all <- lapply(all_tables[2:length(all_tables)], function(.table) {

    if (is.null(.table)) return(NULL) # if is.null() means no content whatsoever

    ## Get text + paragraph
    text_inner <- rvest::html_text(.table)
    data_case_inner <- dplyr::data_frame(par_paragraph = gsub("^\\((\\d+)\\).+", "(\\1)", text_inner),
                                  par_text = gsub("^\\(\\d+\\)", "", text_inner)) %>%
      dplyr::mutate(par_paragraph = ifelse(par_paragraph == par_text, NA, par_paragraph),
             par_paragraph = as.numeric(gsub("[^0-9.-]+", "", as.character(par_paragraph)))) %>%
      dplyr::bind_cols(data_meta)

    ## Get references
    data_references_inner <- extract_references(.table, data_case_inner$par_paragraph) %>%
      dplyr::mutate(case_citation = data_meta$case_citation,
             case_date = data_meta$case_date)

    out_inner <- list(data_case = data_case_inner,
                      data_references = data_references_inner)
    return(out_inner)
  })

  data_case <- lapply(data_all, function(x) x$data_case) %>% bind_rows()
  data_references <- lapply(data_all, function(x) x$data_references) %>% bind_rows()

  out <- list(data_case = data_case,
              data_references = data_references)

  return(out)
}

## Cases before 2003 are split in paragraphs (<p>), although not
## officially a paragraph we use these to denote e.g. what paragraph a
## reference where made
extract_data_html.sc_before_2003 <- function(.case, data_meta, all_tables, ...) {

  nodes <- xml2::xml_find_all(.case, ".//p[preceding-sibling::div[@align='center']] | .//table[preceding-sibling::div[@align='center']]")
  text <- rvest::html_text(nodes)
  text <- gsub("_", "", text)

  keep <- which(!(text == "") & grepl("\\w", text))
  if (length(keep) == 0) {
    data_case <- dplyr::data_frame(par_paragraph = 0,
                                   par_text = "")
    data_case <- suppressWarnings(cbind(data_case, data_meta))
    data_references <- NULL
    out <- list(data_case = data_case,
                data_references = data_references)
    return(out)
  }

  data_case <- dplyr::data_frame(par_paragraph = 1:length(text[keep]),
                                 par_paragraph_org = 1:length(text[keep]),
                                 par_text = text[keep])

  ## If there are page numbers (e.g., "Side 1729") it probably means
  ## that the paragraph got cut off, so we glue the paragaph on each
  ## side together
  page_ind <- grep("^Side *\\d+.*", data_case$par_text)
  page_ind <- page_ind[!is.na(page_ind)]
  page_ind_org <- page_ind
  if (!is.null(page_ind)) {
    done <- FALSE
    x <- 1
    while (!done) {
      ind <- page_ind[x]
      if (!is.na(ind)) {
        nshift <- ifelse(ind == 1, 1, 2)
        data_case <- data_case %>%
          dplyr::filter(par_paragraph != ind) %>%
          dplyr::mutate(par_paragraph = ifelse(par_paragraph %in% (ind + 1):max(par_paragraph), par_paragraph - nshift, par_paragraph)) %>%
          dplyr::group_by(par_paragraph) %>%
          dplyr::summarize(par_paragraph_org = ifelse(length(par_paragraph_org) > 1, par_paragraph_org, list(par_paragraph_org)),
                           par_text = paste0(par_text, collapse = " ")) %>%
          ## dplyr::filter(paragraph != ind) %>%
          ## dplyr::mutate(paragraph = ifelse(paragraph %in% (ind + 1):max(paragraph), paragraph - nshift, paragraph)) %>%
          ## dplyr::group_by(paragraph) %>%
          ## dplyr::summarize(paragraph_org = ifelse(length(paragraph_org) > 1, paragraph_org, list(paragraph_org)),
          ##                  text = paste0(text, collapse = " ")) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(par_paragraph_org = lapply(par_paragraph_org, function(x) unlist(x)))
        page_ind <- page_ind - nshift
      }
      x <- x + 1
      if (x > length(page_ind)) done <- TRUE
    }
  }
  data_case <- suppressWarnings(cbind(data_case, data_meta))

  ## To correct par_paragraph in references if paragraphs shifted
  pl <- dplyr::select(data_case, par_paragraph, par_paragraph_org)
  if (is.list(data_case$par_paragraph_org)) pl <- tidyr::unnest(pl)
  data_case <- dplyr::select(data_case, -par_paragraph_org)

  ## Get in-text references
  data_references <- lapply(1:length(keep),
                            function(x) extract_references(nodes[keep][x], x))
  data_references <- data_references %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(case_citation = data_meta$case_citation,
           case_date = data_meta$case_date)
  if (nrow(data_references) > 0) { # correct paragraph after shift
    data_references <- data_references %>%
      dplyr::mutate(par_paragraph = pl$par_paragraph[match(par_paragraph, pl$par_paragraph_org)])
  }

  out <- list(data_case = data_case,
              data_references = data_references)
  return(out)
}

## Takes the html data and extracts the reference. References within
## the html code are always links (<href>).
extract_references <- function(node, par_paragraph = 1) {
  link_node <- node %>%
    xml2::xml_find_all(".//a[preceding-sibling::span]")
  ref_text <- rvest::html_text(link_node)
  ref_link <- rvest::html_attr(link_node, "href")

  ## In-text references to prework
  ref_link_pre <- ref_link[grep("/forarbeid/", ref_link)]
  if (length(ref_link_pre) > 0) {
    ref_pre <- dplyr::data_frame(ref_type = "prepwork",
                          ref_type_no = "forarbeid",
                          ref_law = NA,
                          ref_reference = gsub(".*/forarbeid/(.+-\\d*-*\\d*-*\\d+).*", "\\1", ref_link_pre),
                          ref_paragraph = gsub(".*/(s\\d*)$", "\\1", ref_link_pre),
                          ref_text = ref_text[grep("/forarbeid/", ref_link)],
                          ref_link = ref_link_pre) %>%
      dplyr::mutate(ref_paragraph = ifelse(ref_paragraph == ref_link, NA, ref_paragraph))
  } else {
    ref_pre <- NULL
  }

  ## In-text references to regulations
  ref_link_reg <- ref_link[grep("/forskrift/", ref_link)]
  if (length(ref_link_reg) > 0) {
    ref_reg <- dplyr::data_frame(ref_type = "regulation",
                          ref_type_no = "forskrift",
                          ref_law = NA,
                          ref_reference = gsub(".*/forskrift/(\\d+-\\d+-\\d+-*\\d*).*", "\\1", ref_link_reg),
                          ref_paragraph = ifelse(grepl("§", ref_link_reg),
                                                 gsub(".*(§.+)$", "\\1", ref_link_reg),
                                                 NA),
                          ref_text = ref_text[grep("/forskrift/", ref_link)],
                          ref_link = ref_link_reg) %>%
      dplyr::mutate(ref_paragraph = ifelse(ref_paragraph == ref_link, NA, ref_paragraph))
  } else {
    ref_reg <- NULL
  }
  ## In-text references to law
  ref_link_law <- ref_link[grep("/lov/", ref_link)]
  if (length(ref_link_law) > 0) {
    ref_law <- dplyr::data_frame(ref_type = "law",
                          ref_type_no = "lov",
                          ref_law = ifelse(gsub(".*\\d+/(\\w+)/.*", "\\1", ref_link_law) == ref_link_law,
                                           "nlo", gsub(".*\\d+/(\\w+)/.*", "\\1", ref_link_law)),
                          ref_reference = gsub(".*/lov/(\\d+-\\d+-\\d+-*\\d*).*", "\\1", ref_link_law),
                          ref_paragraph = ifelse(grepl("§", ref_link_law),
                                                 gsub(".*(§\\d+).*", "\\1", ref_link_law),
                                                 gsub(".*/(a.+)$", "\\1", ref_link_law)),
                          ref_text = ref_text[grep("/lov/", ref_link)],
                          ref_link = ref_link_law) %>%
      dplyr::mutate(ref_paragraph = ifelse(ref_paragraph == ref_link, NA, ref_paragraph))
  } else {
    ref_law <- NULL
  }

  ## In-text references for decisions
  ref_link_dec <- ref_link[grep("/avgjorelse/", ref_link)]
  if (length(ref_link_dec) > 0) {
    ref_dec <- dplyr::data_frame(ref_type = "case",
                          ref_type_no = "avgjorelse",
                          ref_case = gsub("(.*)/.+$", "\\1", gsub("^.+avgjorelse/(.*)", "\\1", ref_link_dec)),
                          ref_case_paragraph = gsub(".*/(.*)$", "\\1", gsub("^.+avgjorelse/(.*)", "\\1", ref_link_dec)),
                          ref_text = ref_text[grep("/avgjorelse/", ref_link)],
                          ref_link = ref_link_dec[grep("/avgjorelse/", ref_link)]) %>%
      dplyr::mutate(ref_case_paragraph = ifelse(ref_case == ref_case_paragraph, NA, ref_case_paragraph))
  } else {
    ref_dec <- NULL
  }

  data_references <- dplyr::bind_rows(ref_pre, ref_reg, ref_law, ref_dec) %>%
    dplyr::mutate(par_paragraph = par_paragraph)

  return(data_references)
}
