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
  get_references <- TRUE
  if (length(all_tables) == 1) {
    .case_data <- dplyr::data_frame(paragraph = 0,
                                    text = "")
    .case_data <- suppressWarnings(cbind(.case_data, data_meta))
    attr(.case_data, ".case_references") <- NULL
    return(.case_data)
  }
  .case_data <- lapply(all_tables[2:length(all_tables)], function(.table) {
    if (is.null(.table)) return(NULL)
    .text <- rvest::html_text(.table)
    .inner_case_data <- dplyr::data_frame(paragraph = gsub("^\\((\\d+)\\).+", "(\\1)", .text),
                                          text = gsub("^\\(\\d+\\)", "", .text)) %>%
      dplyr::mutate(paragraph = ifelse(paragraph == text, NA, paragraph),
                    paragraph = as.numeric(gsub("[^0-9.-]+", "", as.character(paragraph)))) %>%
      dplyr::bind_cols(data_meta)
    if (get_references) {
      .case_references <- .extract_references(.table, .inner_case_data$paragraph, "law")
    } else {
      .case_references <- NULL
    }
    attr(.inner_case_data, ".case_references") <- .case_references
    return(.inner_case_data)
  })
  return(.case_data)
}

## Cases before 2003 are split in paragraphs (<p>), although not
## officially a paragraph we use these to denote e.g. what paragraph a
## reference where made
extract_data_html.sc_before_2003 <- function(.case, data_meta, all_tables, ...) {
  get_references <- TRUE
  nodes <- xml2::xml_find_all(.case, ".//p[preceding-sibling::div[@align='center']] | .//table[preceding-sibling::div[@align='center']]")
  .text <- rvest::html_text(nodes)
  .text <- gsub("_", "", .text)

  keep <- which(!(.text == "") & grepl("\\w", .text))
  if (length(keep) == 0) {
    .case_data <- dplyr::data_frame(paragraph = 0,
                                    text = "")
    .case_data <- suppressWarnings(cbind(.case_data, data_meta))
    attr(.case_data, ".case_references") <- NULL
    return(.case_data)
  }

  .case_data <- dplyr::data_frame(paragraph = 1:length(.text[keep]),
                                  paragraph_org = 1:length(.text[keep]),
                                  text = .text[keep])

  ## If there are page numbers (e.g., "Side 1729") it probably means
  ## that the paragraph got cut off, so we glue the paragaph on each
  ## side together
  page_ind <- grep("^Side *\\d+.*", .case_data$text)
  page_ind <- page_ind[!is.na(page_ind)]
  page_ind_org <- page_ind
  if (!is.null(page_ind)) {
    done <- FALSE
    x <- 1
    while (!done) {
      ind <- page_ind[x]
      if (!is.na(ind)) {
        nshift <- ifelse(ind == 1, 1, 2)
        .case_data <- .case_data %>%
          dplyr::filter(paragraph != ind) %>%
          dplyr::mutate(paragraph = ifelse(paragraph %in% (ind + 1):max(paragraph), paragraph - nshift, paragraph)) %>%
          dplyr::group_by(paragraph) %>%
          dplyr::summarize(paragraph_org = ifelse(length(paragraph_org) > 1, paragraph_org, list(paragraph_org)),
                           text = paste0(text, collapse = " ")) %>%
          ## dplyr::filter(paragraph != ind) %>%
          ## dplyr::mutate(paragraph = ifelse(paragraph %in% (ind + 1):max(paragraph), paragraph - nshift, paragraph)) %>%
          ## dplyr::group_by(paragraph) %>%
          ## dplyr::summarize(paragraph_org = ifelse(length(paragraph_org) > 1, paragraph_org, list(paragraph_org)),
          ##                  text = paste0(text, collapse = " ")) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(paragraph_org = lapply(paragraph_org, function(x) unlist(x)))
        page_ind <- page_ind - nshift
      }
      x <- x + 1
      if (x > length(page_ind)) done <- TRUE
    }
  }
  .case_data <- suppressWarnings(cbind(.case_data, data_meta))
  paragraph_link <- dplyr::select(.case_data, paragraph, paragraph_org)
  if (is.list(.case_data$paragraph_org)) paragraph_link <- tidyr::unnest(paragraph_link)
  if (get_references) {
    .case_references <- lapply(1:length(keep), function(x)
      .extract_references(nodes[keep][x], x, "law")) %>%
      dplyr::bind_rows()
    if (nrow(.case_references) > 0) {  # correct paragraph after shift
      .case_references$paragraph <- paragraph_link$paragraph[match(.case_references$paragraph, paragraph_link$paragraph_org)]
    }
  } else {
    .case_references <- NULL
  }
  attr(.case_data, ".case_references") <- .case_references
  .case_data <- select(.case_data, -paragraph_org)
  return(.case_data)
}

## Takes the html data and extracts the reference. References within
## the html code are always links (<href>).
.extract_references <- function(node, avsnitt = 1, type) {
  node <- node %>%
    xml2::xml_find_all(".//a[preceding-sibling::span]")
  ref_text <- rvest::html_text(node)
  ref_link <- rvest::html_attr(node, "href")

  if (type == "law") {
    ## In-text references to prework
    ref_link_pre <- ref_link[grep("/forarbeid/", ref_link)]
    if (length(ref_link_pre) > 0) {
      ref_pre <- dplyr::data_frame(type = "forarbeid",
                                   lov = NA,
                                   referanse = gsub(".*/forarbeid/(.+-\\d*-*\\d*-*\\d+).*", "\\1", ref_link_pre),
                                   paragraph = gsub(".*/(s\\d*)$", "\\1", ref_link_pre),
                                   tekst = ref_text[grep("/forarbeid/", ref_link)],
                                   link = ref_link_pre) %>%
        dplyr::mutate(paragraph = ifelse(paragraph == link, NA, paragraph),
                      avsnitt = avsnitt)
    } else {
      ref_pre <- NULL
    }
    ## In-text references to regulations
    ref_link_reg <- ref_link[grep("/forskrift/", ref_link)]
    if (length(ref_link_reg) > 0) {
      ref_reg <- dplyr::data_frame(type = "forskrift",
                                   lov = NA,
                                   referanse = gsub(".*/forskrift/(\\d+-\\d+-\\d+-*\\d*).*", "\\1", ref_link_reg),
                                   paragraph = ifelse(grepl("§", ref_link_reg),
                                                      gsub(".*(§.+)$", "\\1", ref_link_reg),
                                                      NA),
                                   tekst = ref_text[grep("/forskrift/", ref_link)],
                                   link = ref_link_reg) %>%
        dplyr::mutate(avsnitt = avsnitt)
    } else {
      ref_reg <- NULL
    }
    ## In-text references to law
    ref_link_law <- ref_link[grep("/lov/", ref_link)]
    if (length(ref_link_law) > 0) {
      ref_law <- dplyr::data_frame(type = "lov",
                                   lov = ifelse(gsub(".*\\d+/(\\w+)/.*", "\\1", ref_link_law) == ref_link_law,
                                                "nlo", gsub(".*\\d+/(\\w+)/.*", "\\1", ref_link_law)),
                                   referanse = gsub(".*/lov/(\\d+-\\d+-\\d+-*\\d*).*", "\\1", ref_link_law),
                                   paragraph = ifelse(grepl("§", ref_link_law),
                                                      gsub(".*(§\\d+).*", "\\1", ref_link_law),
                                                      gsub(".*/(a.+)$", "\\1", ref_link_law)),
                                   tekst = ref_text[grep("/lov/", ref_link)],
                                   link = ref_link_law) %>%
        dplyr::mutate(paragraph = ifelse(paragraph == link, NA, paragraph),
                      avsnitt = avsnitt)
    } else {
      ref_law <- NULL
    }
    ref <- dplyr::bind_rows(ref_pre, ref_reg, ref_law)
    return(ref)

  } else if (type == "decision") {

    ## In-text references for decisions
    ref_link_dec <- ref_link[grep("/avgjorelse/", ref_link)]
    if (length(ref_link_dec) > 0) {
      ref_dec <- dplyr::data_frame(type = "avgjorelse",
                                   sak = gsub("(.*)/.+$", "\\1", gsub("^.+avgjorelse/(.*)", "\\1", ref_link_dec)),
                                   ref_avsnitt = gsub(".*/(.*)$", "\\1", gsub("^.+avgjorelse/(.*)", "\\1", ref_link_dec)),
                                   tekst = ref_text[grep("/avgjorelse/", ref_link)],
                                   link = ref_link_dec[grep("/avgjorelse/", ref_link)]) %>%
        dplyr::mutate(ref_avsnitt = ifelse(sak == ref_avsnitt, NA, ref_avsnitt))
    } else {
      ref_dec <- NULL
    }
    return(ref_dec)
  }
}
