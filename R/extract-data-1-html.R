
## Takes the html code from one case and extracts text and
## references. The html must be parsed differently depending on when
## the case is from.
extract_data_html <- function(.case, data_meta, all_tables) {
  
  
  ## First find what method to use
  date <- as.numeric(gsub("^(\\d{4}).*", "\\1", data_meta$Dato))
  if (date >= 2003)
    class(.case) <- c("sc_after_2003", class(.case))
  else
    class(.case) <- c("sc_before_2003", class(.case))
  
  ## Then use it
  UseMethod("extract_data_html", .case)
}

## Cases after 2003 are split into tables where each table is a
## official paragraph within the court decision
extract_data_html.sc_after_2003 <- function(.case, data_meta, all_tables) {
  get_references <- TRUE
  .case_data <- lapply(all_tables[2:length(all_tables)], function(.table) {
    if (is.null(.table)) return(NULL)
    .text <- html_text(.table)
    .inner_case_data <- data_frame(avsnitt = gsub("^\\((\\d+)\\).+", "(\\1)", .text),
                                   tekst = gsub("^\\(\\d+\\)", "", .text)) %>%
      mutate(avsnitt = ifelse(avsnitt == tekst, NA, avsnitt),
             avsnitt = as.numeric(gsub("[^0-9.-]+", "", as.character(avsnitt)))) %>%
      bind_cols(data_meta)
    
    if (get_references) {
      .case_references <- .extract_references(.table, .inner_case_data$avsnitt, "law")
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
extract_data_html.sc_before_2003 <- function(.case, data_meta, all_tables) {
  get_references <- TRUE
  nodes <- xml_find_all(.case, ".//p[preceding-sibling::hr] | .//table[preceding-sibling::hr]")
  .text <- html_text(nodes)
  keep <- which(!(gsub("_", "", .text) == "") & grepl("\\w", .text))
  
  .case_data <- data_frame(avsnitt = 1:length(.text[keep]),
                           tekst = .text[keep])
  .case_data <- lapply(1:nrow(.case_data), function(x)
    bind_cols(.case_data[x, ], data_meta)) %>%
    bind_rows()
  
  if (get_references) {
    .case_references <- lapply(1:length(keep), function(x)
      .extract_references(nodes[keep][x], x, "law")) %>%
      bind_rows()
  } else {
    .case_references <- NULL
  }
  attr(.case_data, ".case_references") <- .case_references
  return(.case_data)
}

## Takes the html data and extracts the reference. References within
## the html code are always links (<href>).
.extract_references <- function(node, avsnitt = 1, type) {
  node <- node %>%
    xml_find_all(".//a[preceding-sibling::span]")
  ref_text <- html_text(node)
  ref_link <- html_attr(node, "href")
  if (type == "law") {
    ## In-text references to prework
    ref_link_pre <- ref_link[grep("/forarbeid/", ref_link)]
    if (length(ref_link_pre) > 0) {
      ref_pre <- data_frame(type = "forarbeid",
                            lov = NA,
                            referanse = gsub(".*/forarbeid/(.+-\\d*-*\\d*-*\\d+).*", "\\1", ref_link_pre),
                            paragraph = gsub(".*/(s\\d*)$", "\\1", ref_link_pre),
                            tekst = ref_text[grep("/forarbeid/", ref_link)],
                            link = ref_link_pre) %>%
        mutate(paragraph = ifelse(paragraph == link, NA, paragraph),
               avsnitt = avsnitt)
    } else {
      ref_pre <- NULL
    }
    ## In-text references to regulations
    ref_link_reg <- ref_link[grep("/forskrift/", ref_link)]
    if (length(ref_link_reg) > 0) {
      ref_reg <- data_frame(type = "forskrift",
                            lov = NA,
                            referanse = gsub(".*/forskrift/(\\d+-\\d+-\\d+-*\\d*).*", "\\1", ref_link_reg),
                            paragraph = ifelse(grepl("§", ref_link_reg),
                                               gsub(".*(§.+)$", "\\1", ref_link_reg),
                                               NA),
                            tekst = ref_text[grep("/forskrift/", ref_link)],
                            link = ref_link_reg) %>%
        mutate(avsnitt = avsnitt)
    } else {
      ref_reg <- NULL
    }
    ## In-text references to law
    ref_link_law <- ref_link[grep("/lov/", ref_link)]
    if (length(ref_link_law) > 0) {
      ref_law <- data_frame(type = "lov",
                            lov = ifelse(gsub(".*\\d+/(\\w+)/.*", "\\1", ref_link_law) == ref_link_law,
                                         "nlo", gsub(".*\\d+/(\\w+)/.*", "\\1", ref_link_law)),
                            referanse = gsub(".*/lov/(\\d+-\\d+-\\d+-*\\d*).*", "\\1", ref_link_law),
                            paragraph = ifelse(grepl("§", ref_link_law),
                                               gsub(".*(§\\d+).*", "\\1", ref_link_law),
                                               gsub(".*/(a.+)$", "\\1", ref_link_law)),
                            tekst = ref_text[grep("/lov/", ref_link)],
                            link = ref_link_law) %>%
        mutate(paragraph = ifelse(paragraph == link, NA, paragraph),
               avsnitt = avsnitt)
    } else {
      ref_law <- NULL
    }
    return(ref)
  } else if (type == "decision") {
    ## In-text references for decisions
    ref_link_dec <- ref_link[grep("/avgjorelse/", ref_link)]
    if (length(ref_link_dec) > 0) {
      ref_dec <- data_frame(type = "avgjorelse",
                            sak = gsub("(.*)/.+$", "\\1", gsub("^.+avgjorelse/(.*)", "\\1", ref_link_dec)),
                            ref_avsnitt = gsub(".*/(.*)$", "\\1", gsub("^.+avgjorelse/(.*)", "\\1", ref_link_dec)),
                            tekst = ref_text[grep("/avgjorelse/", ref_link)],
                            link = ref_link_dec[grep("/avgjorelse/", ref_link)]) %>%
        mutate(ref_avsnitt = ifelse(sak == ref_avsnitt, NA, ref_avsnitt))
  }
}


