#' Extract additional data from text and meta data
#'
#' Various functions for importing extract additional data from text
#' and meta data
#'
#' @importFrom dplyr data_frame bind_rows select filter slice
#' @importFrom tidyr unnest spread fill
#' @importFrom parallelMap parallelMap
#' @importFrom fuzzyjoin stringdist_left_join

## Case parties
add_data_parties <- function(data_case) {
  data_case <- data_case %>%
  dplyr::mutate(part_a = gsub(" mot .+$", "", data_case$parter),
                part_b = gsub("^.+ mot ", "", data_case$parter))
  return(data_case)
}

extract_data_parties <- function(data_case) {
  parter <- lapply(c("part_a", "part_b"), function(y) {
    dplyr::data_frame(id = data_case$publisert,
                      part = data_case[[y]],
                      side = y,
                      advokat = lapply(strsplit(data_case[[y]], "\\("), function(x) gsub(").*$", "", x[grep("\\)", x)]))) %>%
      tidyr::unnest(advokat)
  })
  parter <- dplyr::bind_rows(parter)
  return(parter)
}

## Decision type (Dom/Kjennelse)
add_data_decision_type <- function(data_case) {
  type <- gsub("^.* - ", "", data_case$instans)
  type <- gsub("og", "", type)
  type <- gsub("\\.", "", type)
  type <- gsub(" +", " ", type)
  type <- tolower(type)
  type <- strsplit(type, " ")

  data_case <- data_case %>%
    dplyr::mutate(type = type) %>%
    tidyr::unnest(type) %>%
    dplyr::mutate(type_value = 1) %>%
    tidyr::spread(type, type_value, fill = 0)

  return(data_case)
}

## Case Type (Straff/Sivil)
add_data_case_type <- function(data_case) {
  data_case <- data_case %>%
    dplyr::mutate(type = ifelse(grepl("sivil sak", data_case$saksgang, ignore.case = TRUE), "sivil sak", NA),
                  type = ifelse(grepl("straffesak", data_case$saksgang, ignore.case = TRUE), "straffesak", type))
  return(data_case)
}

## Case Proceedings (case flow/saksgang)
extract_data_case_proceedings <- function(data_case) {
  saksgang <- dplyr::data_frame(id = data_case$publisert,
                         instans = strsplit(data_case$saksgang, " -"),
                         rekke = sapply(instans, function(x) 1:length(x))) %>%
    tidyr::unnest() %>%
    dplyr::mutate(instans = gsub("^ +| +$", "", instans))
  return(saksgang)
}

## Judges (now matches with judges data set)
extract_data_judges <- function(data_case, match_judges = TRUE) {

  judges <- lapply(unique(data_case$publisert), function(.id) {

    data <- data_case %>% dplyr::filter(publisert == .id) %>% dplyr::slice(1)

    judges <- dplyr::data_frame(id = .id,
                                dato = data$dato,
                                judge = strsplit(gsub("\\.", "", data$forfatter),
                                                 " og |, |[dD]issens|[sS]ærmerknad[er]*"),
                                nr = lapply(judge, function(x) 1:length(x))) %>%
      tidyr::unnest() %>%
      dplyr::mutate(justitiarius = ifelse(grepl("Justitiarius", judge), 1, 0),
                    kst = ifelse(nr == 1, 1, 0),
                    judge = gsub("[dD]ommer[ne]* ", "", judge),
                    judge = gsub("[dD]elvis", "", judge),
                    judge = gsub("\\:|\\,|[kK]st ", "", judge),
                    judge = gsub("Justitiarius ", "", judge),
                    judge = gsub("og", "", judge),
                    judge = gsub("^ +| +$", "", judge)) %>%
      dplyr::select(-nr) %>%
      dplyr::mutate(judge = strsplit(judge, " ")) %>%
      tidyr::unnest()

    if (match_judges) {
      judges_elligable <- domstolr::judges %>%
        filter(!(is.na(start) & is.na(end)),
               ifelse(is.na(start), TRUE, start < judges$dato[1]),
               ifelse(is.na(end), TRUE, end > judges$dato[1]))
      judges <- judges %>%
        fuzzyjoin::stringdist_left_join(judges_elligable, by = c(judge = "name_last")) %>%
        dplyr::select(-judge) %>%
        dplyr::mutate(judge = name_last,
                      judge_full = name_full) %>%
        dplyr::select(id, dato, judge, judge_full, JNR, PNR, justitiarius, kst, interim, chief, start, end)
    }

    return(judges)
  })
  judges <- bind_rows(judges)

  return(judges)
}

## Keywords
extract_data_keywords <- function(data_case) {
  stikkord <- dplyr::data_frame(id = data_case$publisert,
                                stikkord = strsplit(data_case$stikkord, "\\. *")) %>%
    tidyr::unnest()
  return(stikkord)
}

## Section: Extract properties of the text, such as what part of the
# decision it is and which judges that are speaking.
add_data_section <- function(data_case, data_judges) {

  voting <- data_case$publisert[data_case$avsnitt != 1][grep("^ *Domm[ea]r [A-ZÆØÅ].*:", data_case$tekst[data_case$avsnitt != 1])]
  voting <- unique(voting)

  add_section_information_case <- function(case) {
    data <- data_case[data_case$publisert == case, ]
    data$section <- NA

    ## Judges speaking as section_judge
    data <- data %>%
      dplyr::mutate(section_judge = ifelse(grepl("^ *Domm[ea]r[ne]* .*:.*$|^ *Justit[ui]arius .*:.*$|^ *Kst\\. domm[ea]r .*:.*$", tekst),
                                           gsub("^ *(Domm[ea]r[ne]*|Justit[ui]arius|Kst\\. domm[ea]r) (.*?):.*$", "\\2", tekst), NA),
                    section_judge = strsplit(gsub("\\.", "", section_judge), " og |, ")) %>%
      tidyr::unnest() %>%
      dplyr::mutate(section_judge = gsub("[dD]omm[ea]r[ne]* ", "", section_judge),
                    section_judge = gsub("[dD]elvis", "", section_judge),
                    section_judge = gsub("\\:|\\,|[kK]st ", "", section_judge),
                    section_judge = gsub("[jJ]ustit[ui]arius ", "", section_judge),
                    section_judge = gsub(" og ", "", section_judge),
                    section_judge = gsub("^ +| +$", "", section_judge))
    if (is.na(data$section_judge[1])) {
      data$section_judge[1] <- data_judges$judge[data_judges$id == case][1]
    }
    data <- tidyr::fill(data, section_judge)

    ## Func to add to section using pattern. Expects data$tekst.
    find_section <- function(patterns) {
      section_place <- as.numeric(unlist(sapply(patterns, grep, x = data$tekst)))
      section_place <- section_place[!is.na(section_place)]
      return(section_place)
    }

    ## Syllabus
    data$section[1] <- "syllabus"

    ## lower_court_excerpt
    lower_court_excerpt <- find_section(c("^ *Av herredsrettens dom .*:$",
                                          "^ *Av byrettens dom .*:$"))
    data$section[lower_court_excerpt] <- "lower_court_excerpt"

    if (case %in% voting) {
      data$case_have_vote <- "voting"

      ## Main opinion
      main_opinion <- find_section(c("^ *Jeg er kommet til ",
                                     "^ *Jeg starter med å se",
                                     "^ *Jeg finner at",
                                     "^ *Jeg bemerker at saken",
                                     "^ *Mitt syn på saken",
                                     "^ *Eg er komen til",
                                     "^ *Egne bemerkninger",
                                     "^ *Jeg ser først på",
                                     "^ *Jeg ser slik på saken:"))
      main_opinion <- main_opinion[1]
      data$section[main_opinion] <- "Main opinion"

      ## Votes
      votes_1 <- find_section(c("^ *Eg røystar etter dette",
                                "[Jj]eg stemmer for",
                                "^dom:$",
                                "^ *Jeg stemmer etter dette",
                                "^ *Etter dette stemmer jeg for",
                                "^Da jeg er i mindretall, former jeg ingen konklusjon"))
      votes_1 <- votes_1[1]
      votes <- find_section("^ *Domm[ea]r[ne]* .*:.*$|^ *Justit[ui]arius .*:.*$|^ *Kst\\. domm[ea]r .*:.*$")

      ## votes <- find_section(c("^ *Domm[ea]r[ne]* ",
      ##                         "^ *Justituarius ",
      ##                         "^ *Justitiarius ",
      ##                         "^ *Kst domm[ea]r "))
      if (length(main_opinion > 0)) {
        votes <- votes[votes > max(main_opinion)]
      } else {
        votes <- votes[votes != 1]
      }
      votes <- c(votes_1, votes)
      for (i in 1:length(votes)) data$section[votes[i]] <- paste0("vote_", i)

      ## Judgement
      judgement <- find_section(c("^ *Etter stemmegivningen avsa Høyesterett denne",
                                  "^ *Etter røystinga sa Høgsterett slik"))
      data$section[judgement] <- "judgement"

    } else {  # Non-voting decions

      data$case_have_vote <- "non_voting"

      ## Main opinon
      main_opinion <- find_section(c("^ *Jeg er kommet til ",
                                     "^ *Høyesteretts ankeutvalg",
                                     "^ *Høyesteretts kompetanse",
                                     "^ *Jeg starter med å se",
                                     "^ *Jeg finner at",
                                     "^ *Jeg bemerker at saken",
                                     "^ *Mitt syn på saken",
                                     "^ *Eg er komen til",
                                     "^ *Egne bemerkninger",
                                     "^ *Jeg ser først på",
                                     "^ *Jeg ser slik på saken:",
                                     "^ *Høyesterett bemerker at "))
      data$section[main_opinion[1]] <- "Main opinion"

      ## Judgement
      judgement <- find_section(c("^ *Jeg stemmer for denne",
                                  "^ *Jeg stemmer etter dette for denne",
                                  "^ *Eg røystar etter dette",
                                  "^ *Slutning:",
                                  "^ *kjennelse:"))
      data$section[judgement[1]] <- "judgement"
    }

    data <- tidyr::fill(data, section)

    ## Verify section judge and pnr/jnr
    judges_elligable <- domstolr::judges %>%
      filter(!(is.na(start) & is.na(end)),
             ifelse(is.na(start), TRUE, start < data$dato[1]),
             ifelse(is.na(end), TRUE, end > data$dato[1]))

    data <- data %>%
      mutate(section_judge_matches = ifelse(section_judge %in% judges_elligable$name_last, 1, 0),
             section_judge_JNR = judges_elligable$JNR[match(section_judge, judges_elligable$name_last)],
             section_judge_PNR = judges_elligable$PNR[match(section_judge, judges_elligable$name_last)])
    data$section_judge <- ifelse(data$section == "judgement", NA, data$section_judge)
    data$section_judge_JNR <- ifelse(data$section == "judgement", NA, data$section_judge_JNR)
    data$section_judge_PNR <- ifelse(data$section == "judgement", NA, data$section_judge_PNR)

    ## Minor fixes from the page splitter f'ing up
    data$tekst <- gsub("dennekjennelse", "denne kjennelse", data$tekst)
    data$tekst <- gsub("dennedom", "denne dom", data$tekst)
    return(data)
  }
  data_case <- parallelMap::parallelMap(add_section_information_case,
                                        case = unique(data_case$publisert)) #, level = "case")
  data_case <- dplyr::bind_rows(data_case)
  return(data_case)
}
