## ' Extract additional data from text and meta data
## '
## ' Various functions for importing extract additional data from text
## ' and meta data
## '
## ' @importFrom dplyr data_frame bind_rows select filter slice
## ' @importFrom tidyr unnest spread fill
## ' @importFrom parallelMap parallelMap
## ' @importFrom fuzzyjoin stringdist_left_join

## Case parties
add_data_parties <- function(data_case) {
  data_case <- data_case %>%
    dplyr::mutate(case_appellant = gsub(" mot .+$", "", data_case$case_parties),
                  case_respondent = gsub("^.+ mot ", "", data_case$case_parties))
  return(data_case)
}

extract_data_parties <- function(data_case) {
  .dt <- filter(data_case, !duplicated(case_citation))
  parties <- lapply(c("case_appellant", "case_respondent"), function(y) {
    dplyr::data_frame(case_citation = .dt$case_citation,
                      case_date = .dt$case_date,
                      prt_type = gsub("case_", "", y),
                      prt_party = .dt[[y]],
                      prt_lawyer = lapply(strsplit(.dt[[y]], "\\("),
                                          function(x) gsub(").*$", "", x[grep("\\)", x)]))) %>%
      tidyr::unnest(prt_lawyer)
  })
  parties <- dplyr::bind_rows(parties)
  return(parties)
}

## Decision type (Dom/Kjennelse)
add_data_decision_type <- function(data_case) {
  type <- gsub("^.* - ", "", data_case$case_instance)
  type <- gsub("og", "", type)
  type <- gsub("\\.", "", type)
  type <- gsub(" +", " ", type)
  type <- tolower(type)
  ## type <- strsplit(type, " ")

  data_case <- data_case %>%
    mutate(case_decision_type = as.character(type))

  return(data_case)
}

## Case Type (Straff/Sivil)
add_data_case_type <- function(data_case) {
  data_case <- data_case %>%
    dplyr::mutate(case_type = ifelse(grepl("sivil sak", data_case$case_history, ignore.case = TRUE), "Criminal Case", NA),
                  case_type = ifelse(grepl("straffesak", data_case$case_history, ignore.case = TRUE), "Civil Case", case_type))
  return(data_case)
}

## Case Proceedings (case flow/saksgang)
extract_data_case_proceedings <- function(data_case) {
  .dt <- filter(data_case, !duplicated(case_citation))
  saksgang <- dplyr::data_frame(case_citation = .dt$case_citation,
                                case_date = .dt$case_date,
                                proc_instance = strsplit(.dt$case_history, " -"),
                                proc_lineage = sapply(proc_instance, function(x) 1:length(x))) %>%
    tidyr::unnest() %>%
    dplyr::mutate(proc_instance = gsub("^ +| +$", "", proc_instance))
  return(saksgang)
}

## Judges (now matches with judges data set)
extract_data_judges <- function(data_case, match_judges = TRUE) {

  judges <- lapply(unique(data_case$case_citation), function(.id) {

    data <- data_case %>% dplyr::filter(case_citation == .id) %>% dplyr::slice(1)

    judges <- dplyr::data_frame(case_citation = .id,
                                case_date = data$case_date,
                                judge_name = strsplit(gsub("\\.", "", data$case_judges),
                                                 " og |, |[dD]issens|[sS]ærmerknad[er]*"),
                                nr = lapply(judge_name, function(x) 1:length(x))) %>%
      tidyr::unnest() %>%
      dplyr::mutate(judge_justitiarius = ifelse(grepl("Justitiarius", judge_name), 1, 0),
                    judge_kst = ifelse(nr == 1, 1, 0),
                    judge_name = gsub("[dD]ommer[ne]* ", "", judge_name),
                    judge_name = gsub("[dD]elvis", "", judge_name),
                    judge_name = gsub("\\:|\\,|[kK]st ", "", judge_name),
                    judge_name = gsub("Justitiarius ", "", judge_name),
                    judge_name = gsub("og", "", judge_name),
                    judge_name = gsub("^ +| +$", "", judge_name)) %>%
      dplyr::select(-nr) %>%
      dplyr::mutate(judge_name = strsplit(judge_name, " ")) %>%
      tidyr::unnest()

    if (match_judges & nrow(judges) > 1) {
      judges_elligable <- domstolr::judges %>%
        filter(!(is.na(start) & is.na(end)),
               ifelse(is.na(start), TRUE, start < judges$case_date[1]),
               ifelse(is.na(end), TRUE, end > judges$case_date[1]))
      judges <- judges %>%
        ## mutate(judge_unmatched = name_last) %>%
        fuzzyjoin::stringdist_left_join(judges_elligable, by = c(judge_name = "name_last")) %>%
        dplyr::rename(judge_matched_name = name_last,
                      judge_matched_name_full = name_full,
                      judge_matched_PNR = PNR,
                      judge_matched_JNR = JNR,
                      judge_matched_interm = interim,
                      judge_matched_chief = chief,
                      judge_matched_start = start,
                      judge_matched_end = end)
    }
    return(judges)
  })
  judges <- bind_rows(judges)

  return(judges)
}

## Keywords
extract_data_keywords <- function(data_case) {
  .dt <- filter(data_case, !duplicated(case_citation))
  keywords <- dplyr::data_frame(case_citation = .dt$case_citation,
                                case_date =  .dt$case_date,
                                keyword = strsplit(.dt$case_keywords, "\\. *")) %>%
    tidyr::unnest()
  return(keywords)
}

## Section: Extract properties of the text, such as what part of the
# decision it is and which judges that are speaking.
add_data_section <- function(data_case, data_judges, match_judges = TRUE) {

  voting <- data_case$case_citation[data_case$par_paragraph != 1][grep("^ *Domm[ea]r [A-ZÆØÅ].*:", data_case$par_text[data_case$par_paragraph != 1])]
  voting <- unique(voting)

  add_section_information_case <- function(case) {
    data <- data_case[data_case$case_citation == case, ]
    data$par_section <- NA

    ## Judges speaking as section_judge
    data <- data %>%
      dplyr::mutate(par_judge = ifelse(grepl("^ *Domm[ea]r[ne]* .*:.*$|^ *Justit[ui]arius .*:.*$|^ *Kst\\. domm[ea]r .*:.*$", par_text),
                                           gsub("^ *(Domm[ea]r[ne]*|Justit[ui]arius|Kst\\. domm[ea]r) (.*?):.*$", "\\2", par_text), NA),
                    par_judge = strsplit(gsub("\\.", "", par_judge), " og |, ")) %>%
      tidyr::unnest() %>%
      dplyr::mutate(par_judge = gsub("[dD]omm[ea]r[ne]* ", "", par_judge),
                    par_judge = gsub("[dD]elvis", "", par_judge),
                    par_judge = gsub("\\:|\\,|[kK]st ", "", par_judge),
                    par_judge = gsub("[jJ]ustit[ui]arius ", "", par_judge),
                    par_judge = gsub(" og ", "", par_judge),
                    par_judge = gsub("^ +| +$", "", par_judge))
    if (is.na(data$par_judge[1])) {
      data$par_judge[1] <- data_judges$judge_name[data_judges$case_citation == case][1]
    }
    data <- tidyr::fill(data, par_judge)

    ## Func to add to section using pattern. Expects data$tekst.
    find_section <- function(patterns) {
      section_place <- as.numeric(unlist(sapply(patterns, grep, x = data$par_text)))
      section_place <- section_place[!is.na(section_place)]
      return(section_place)
    }

    ## Syllabus
    data$par_section[1] <- "syllabus"

    ## lower_court_excerpt
    lower_court_excerpt <- find_section(c("^ *Av herredsrettens dom .*:*$",
                                          "^ *Av byrettens dom .*:*$",
                                          "^ *Av lagmannsrettens dom .*:*$",
                                          "^ *Av underskjønnet .*:*$",
                                          "^ *Av overskjønnet .*:*$",
                                          "^ *Av forhørsrettens dom .*:*$"))
    data$par_section[lower_court_excerpt] <- "lower_court_excerpt"

    if (case %in% voting) {
      data$case_have_vote <- "voting"

      ## Main opinion
      main_opinion <- find_section(c("^ *Jeg er kommet til ",
                                     "^ *Jeg starter med å se",
                                     "^ *Jeg finner at",
                                     "^ *Jeg bemerker at saken",
                                     "^ *Mitt syn på saken",
                                     "^ *Eg er komen til",
                                     "^ *Mitt syn på saka",
                                     "^ *Egne bemerkninger",
                                     "^ *Jeg ser først på",
                                     "^ *Jeg ser slik på saken:",
                                     "^ *Jeg er når det gjelder utmålingen ",
                                     "^ *Jeg er blitt stående ved at anken bør tas til følge",
                                     "^ *Jeg finner det naturlig å behandle anken over saksbehandlingen først"))
      main_opinion <- main_opinion[1]
      data$par_section[main_opinion] <- "Main opinion"

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
      for (i in 1:length(votes)) data$par_section[votes[i]] <- paste0("vote_", i)

      ## Judgement
      judgement <- find_section(c("^ *Etter stemmegivningen avsa Høyesterett denne",
                                  "^ *Etter røystinga sa Høgsterett slik"))
      data$par_section[judgement] <- "judgement"

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
      data$par_section[main_opinion[1]] <- "Main opinion"

      ## Judgement
      judgement <- find_section(c("^ *Jeg stemmer for denne",
                                  "^ *Jeg stemmer etter dette for denne",
                                  "^ *Eg røystar etter dette",
                                  "^ *Eg røystar for",
                                  "^ *Slutning:",
                                  "^ *kjennelse:"))
      data$par_section[judgement[1]] <- "judgement"
    }

    data <- tidyr::fill(data, par_section) %>%
      dplyr::select(-case_have_vote)

    ## Verify par_judge and pnr/jnr using sep data
    if (match_judges) {
      judges_elligable <- domstolr::judges %>%
        filter(!(is.na(start) & is.na(end)),
               ifelse(is.na(start), TRUE, start < data$case_date[1]),
               ifelse(is.na(end), TRUE, end > data$case_date[1]))

      data <- data %>%
        mutate(par_judge_found_match = ifelse(par_judge %in% judges_elligable$name_last, 1, 0),
               par_judge_JNR = judges_elligable$JNR[match(par_judge, judges_elligable$name_last)],
               par_judge_PNR = judges_elligable$PNR[match(par_judge, judges_elligable$name_last)],
               par_judge = ifelse(par_section == "judgement", NA, par_judge),
               par_judge_JNR = ifelse(par_section == "judgement", NA, par_judge_JNR),
               par_judge_PNR = ifelse(par_section == "judgement", NA, par_judge_PNR),
               par_judge = ifelse(par_section == "lower_court_excerpt", NA, par_judge),
               par_judge_JNR = ifelse(par_section == "lower_court_excerpt", NA, par_judge_JNR),
               par_judge_PNR = ifelse(par_section == "lower_court_excerpt", NA, par_judge_PNR))
    }

    ## Minor fixes from the page splitter f'ing up
    data$par_text <- gsub("dennekjennelse", "denne kjennelse", data$par_text)
    data$par_text <- gsub("dennedom", "denne dom", data$par_text)
    data$par_text <- gsub("slikdom", "slik dom", data$par_text)
    return(data)
  }
  data_case <- parallelMap::parallelMap(add_section_information_case,
                                        case = unique(data_case$case_citation))
  data_case <- dplyr::bind_rows(data_case)

  return(data_case)
}
