# clear all
# rm(list=ls(all=TRUE))

###############


### Load packages
pkgs <- c("rvest", "dplyr", "tidyr", "tokenizers", "tidytext", "ggplot2", "igraph", "stringr", "psych", "doBy", "ddply")
for (pkg in pkgs) if (!require(pkg, character.only = TRUE)) install.packages(pkg, character.only = TRUE)

if (!require(ggraph)) {
  devtools::install_github('thomasp85/ggforce')
  devtools::install_github('thomasp85/ggraph')
}

if (!require(tidytext)) {
  devtools::install_github("juliasilge/tidytext")
  devtools::install_github("lmullen/tokenizers")
}

for (pkg in pkgs) library(pkg, character.only = TRUE)

#### domstolr function
domstolr <- function(file = NULL, directory = NULL, regex = ".*.html$", recursive = TRUE,
                    not_text = FALSE, get_extra = TRUE, mash_text = FALSE,
                    verbose = FALSE) {

  if (!is.null(directory)) {
    file  <- list.files(path = gsub("^/+", "", directory), recursive = recursive,
                        pattern = regex, full.names = TRUE)
    if (length(file) == 0) stop("Unable to find any files.")
  }

  all_data <- lapply(file, .import_ld, meta_only = not_text, get_references = get_extra,
                mash_text = mash_text, verbose = verbose)

  if (not_text) {
    out <- as.data.frame(bind_rows(all_data))
    class(out) <- c("data.frame", "ld")
    return(out)
  }

  out <- lapply(all_data, function(x) x[[1]]) %>%
    bind_rows() %>%
    as.data.frame()

  if (get_extra) {
    case_references <- lapply(all_data, function(x) x[[2]])
    meta_data <- lapply(1:length(case_references[[1]]), function(x) lapply(case_references, function(y) y[[x]]))
    meta_data <- lapply(meta_data, function(x) as.data.frame(bind_rows(x)))
    names(meta_data) <- names(case_references[[1]])
    attr(out, "meta_data") <- meta_data
  }

  class(out) <- c("data.frame", "ld")
  return(out)
}

##
## Internal import functions ----------------------------------------------------------------------
##

.import_ld <- function(file, meta_only = FALSE, get_references = TRUE, mash_text = FALSE,
                      verbose = FALSE) {

  cat(file)
  
  if (meta_only) get_references <- FALSE
  all_cases <- file %>%
    read_html(encoding = "UTF-8") %>%
    html_nodes("body br ~ div")

  all_data <- lapply(all_cases, function(.case) {

    if (verbose) message(".", appendLF = FALSE)

    all_tables <- xml_find_all(.case, ".//table")

    meta_data <- all_tables[[1]] %>%
      html_table() %>%
      rename(variable = X1,
             value = X2) %>%
      spread(variable, value)

    if (meta_only) return(meta_data)

    if (length(all_tables) == 1) {

      nodes <- xml_find_all(.case, ".//p[preceding-sibling::hr]")
      .text <- html_text(nodes)
      keep <- which(!(gsub(" ", "", .text) == ""))

      .case_data <- data_frame(avsnitt = 1:length(.text[keep]),
                              tekst = .text[keep])
      .case_data <- lapply(1:nrow(.case_data), function(x)
        bind_cols(.case_data[x, ], meta_data)) %>%
        bind_rows()

      if (get_references) {
        .case_references <- lapply(1:length(keep), function(x)
          .extract_references(nodes[keep][x], x, "law")) %>%
          bind_rows()
      } else {
        .case_references <- NULL
      }
      attr(.case_data, ".case_references") <- .case_references
    } else {
      .case_data <- lapply(all_tables[2:length(all_tables)], function(.table) {
        if (is.null(.table)) return(NULL)
        .text <- html_text(.table)
        .inner_case_data <- data_frame(avsnitt = gsub("^\\((\\d+)\\).+", "(\\1)", .text),
                                       tekst = gsub("^\\(\\d+\\)", "", .text)) %>%
          mutate(avsnitt = ifelse(avsnitt == tekst, NA, avsnitt),
                 avsnitt = extract_numeric(avsnitt)) %>%
          bind_cols(meta_data)

        if (get_references) {
          .case_references <- .extract_references(.table, .inner_case_data$avsnitt, "law")
        } else {
          .case_references <- NULL
        }
        attr(.inner_case_data, ".case_references") <- .case_references
        return(.inner_case_data)
      })
    }

    if (get_references) {
      .case_references <- lapply(.case_data, attr, which = ".case_references") %>%
        bind_rows() %>%
        mutate(publisert = meta_data$Publisert)
    } else {
      .case_references <- NULL
    }

    .case_data <- bind_rows(.case_data) %>%
      fill(avsnitt) %>%
      group_by(avsnitt) %>%
      mutate(tekst = paste0(tekst, collapse = " ")) %>%
      filter(!duplicated(tekst)) %>%
      ungroup()

    return(list(.case_data, .case_references))
  })

  if (meta_only) {
    all_data <- bind_rows(all_data)
    names(all_data) <- gsub(" ", "_", tolower(names(all_data)))
    return(all_data)
  }

  case_data <- lapply(all_data, function(x) x[[1]]) %>%
    bind_rows()
  names(case_data) <- gsub(" ", "_", tolower(names(case_data)))
  case_data <- case_data %>%
    mutate(dato = as.Date(dato))

  if (get_references) {

    ## Parter
    case_data <- case_data %>%
      mutate(part_a = gsub(" mot .+$", "", case_data$parter),
             part_b = gsub("^.+ mot ", "", case_data$parter))

    parter <- lapply(c("part_a", "part_b"), function(y) {
      data_frame(id = case_data$publisert,
                 part = case_data[[y]],
                 side = y,
                 advokat = lapply(strsplit(case_data[[y]], "\\("), function(x) gsub(").*$", "", x[grep("\\)", x)]))) %>%
        unnest(advokat)
    })
    parter <- bind_rows(parter)

    ## Type 1
    type <- gsub("^.* - ", "", case_data$instans)
    type <- gsub("og", "", type)
    type <- gsub("\\.", "", type)
    type <- gsub(" +", " ", type)
    type <- tolower(type)
    type <- strsplit(type, " ")

    case_data <- case_data %>%
      mutate(type = type) %>%
      unnest(type) %>%
      mutate(type_value = 1) %>%
      spread(type, type_value, fill = 0)

    ## Type 2 (straffe vs sivil sak)
    case_data <- case_data %>%
      mutate(type = ifelse(grepl("sivil sak", case_data$saksgang, ignore.case = TRUE), "sivil sak", NA),
            type = ifelse(grepl("straffesak", case_data$saksgang, ignore.case = TRUE), "straffesak", type))

    ## Saksgang
    saksgang <- data_frame(id = case_data$publisert,
                           instans = strsplit(case_data$saksgang, " -"),
                           rekke = sapply(instans, function(x) 1:length(x))) %>%
      unnest() %>%
      mutate(instans = gsub("^ +| +$", "", instans))

    ## Dommere

    dommere <- data_frame(id = case_data$publisert,
                          dommer = strsplit(gsub("\\.", "", case_data$forfatter), " og |, |[dD]issens|[sS]ærmerknad[er]*"),
                          nr = lapply(dommer, function(x) 1:length(x))) %>%
      unnest() %>%
      mutate(justitiarius = ifelse(grepl("Justitiarius", dommer), 1, 0),
             kst = ifelse(nr == 1, 1, 0),
             dommer = gsub("[dD]ommer[ne]* ", "", dommer),
             dommer = gsub("[dD]elvis", "", dommer),
             dommer = gsub("\\:|\\,|[kK]st ", "", dommer),
             dommer = gsub("Justitiarius ", "", dommer),
             dommer = gsub("og", "", dommer),
             dommer = gsub("^ +| +$", "", dommer)) %>%
      select(-nr) %>%
      mutate(dommer = strsplit(dommer, " ")) %>%
      unnest()

    ## Stikkord
    stikkord <- data_frame(id = case_data$publisert,
                           stikkord = strsplit(case_data$stikkord, "\\. *")) %>%
      unnest()

  }
  

  case_data <- lapply(unique(case_data$publisert), function(case) {
    data <- case_data[case_data$publisert == case, ]
    data$seksjon <- NA
    if (any(grepl("Jeg er kommet til ", data$tekst))) {
      data$seksjon[grep("Jeg er kommet til ", data$tekst)] <- "førstevoterende"
      if (!is.na(data$seksjon[1])) {
        data$seksjon[1] <- "saksoversikt"
      }
    } else {
      data$seksjon[1] <- "førstevoterende"
    }
    data$seksjon[grep("Dommer", data$tekst)[grep("Dommer", data$tekst) > 1][1]] <- "annenvoterende"
    data <- fill(data, seksjon)
    data$seksjon[is.na(data$seksjon)] <- "saksoversikt"
    return(data)
  })
  case_data <- bind_rows(case_data)


  if (mash_text) {
    case_data <- case_data %>%
      group_by(dato, forfatter, instans, parter, publisert,
               saksgang, sammendrag, sist_oppdatert, stikkord) %>%
      summarize(tekst = paste0(tekst, collapse = " ")) %>%
      ungroup()
  }

  if (get_references) {
    case_references <- lapply(all_data, function(x) x[[2]]) %>%
      bind_rows()
    names(case_references) <- gsub(" ", "_", tolower(names(case_references)))
    meta_data <- list(case_references = case_references,
                      saksgang = saksgang,
                      parter = parter,
                      dommere = dommere,
                      stikkord = stikkord)

  } else {
    meta_data <- NULL
  }

  if (verbose) message("")

  return(list(case_data, meta_data))
}

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
      ref <- bind_rows(ref_pre, ref_reg, ref_law)
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
        } else {
          ref_dec <- NULL
        }
      return(ref_dec)
  }
}



str_wrap <- function(string, width = 30) {
  paste0(strwrap(string, width), sep="", collapse="\n")
}

get_stikkord <- function(ld_data) attr(ld_data, "meta_data")[["stikkord"]]
get_saksgang <- function(ld_data) attr(ld_data, "meta_data")[["saksgang"]]
get_dommere <- function(ld_data) attr(ld_data, "meta_data")[["dommere"]]
get_parter <- function(ld_data) attr(ld_data, "meta_data")[["parter"]]
get_references <- function(ld_data) attr(ld_data, "meta_data")[["case_references"]]

##########


  
