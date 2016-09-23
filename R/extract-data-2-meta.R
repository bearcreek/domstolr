## Case parties
add_data_parties <- function(data_case) {
  data_case <- data_case %>%
    mutate(part_a = gsub(" mot .+$", "", data_case$parter),
           part_b = gsub("^.+ mot ", "", data_case$parter))
  return(data_case)
}

extract_data_parties <- function(data_case) {
  parter <- lapply(c("part_a", "part_b"), function(y) {
    data_frame(id = data_case$publisert,
               part = data_case[[y]],
               side = y,
               advokat = lapply(strsplit(data_case[[y]], "\\("), function(x) gsub(").*$", "", x[grep("\\)", x)]))) %>%
      unnest(advokat)
  })
  parter <- bind_rows(parter)
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
    mutate(type = type) %>%
    unnest(type) %>%
    mutate(type_value = 1) %>%
    spread(type, type_value, fill = 0)
  return(data_case)
}

## Case Type (Straff/Sivil)
add_data_case_type <- function(data_case) {
  data_case <- data_case %>%
    mutate(type = ifelse(grepl("sivil sak", data_case$saksgang, ignore.case = TRUE), "sivil sak", NA),
           type = ifelse(grepl("straffesak", data_case$saksgang, ignore.case = TRUE), "straffesak", type))
  return(data_case)
}

## Case Proceedings (case flow/saksgang)
extract_data_case_proceedings <- function(data_case) {
  saksgang <- data_frame(id = data_case$publisert,
                         instans = strsplit(data_case$saksgang, " -"),
                         rekke = sapply(instans, function(x) 1:length(x))) %>%
    unnest() %>%
    mutate(instans = gsub("^ +| +$", "", instans))
  return(saksgang)
}

## Judges
extract_data_judges <- function(data_case) {
  dommere <- data_frame(id = data_case$publisert,
                        dommer = strsplit(gsub("\\.", "", data_case$forfatter), " og |, |[dD]issens|[sS]ærmerknad[er]*"),
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
  return(dommere)
}

## Keywords
extract_data_keywords <- function(data_case) {
  stikkord <- data_frame(id = data_case$publisert,
                         stikkord = strsplit(data_case$stikkord, "\\. *")) %>%
    unnest()
  return(stikkord)
}

## Section: Extract properties of the text, such as what part of the
## decision it is and which judges that are speaking.
## data_case_org <- data_case

data_case <- data_case_org

add_data_section <- function(data_case) {

  voting <- data_case$publisert[data_case$avsnitt != 1][grep("^ *Domm[ea]r [A-ZÆØÅ].*:", data_case$tekst[data_case$avsnitt != 1])]
  voting <- unique(voting)

  ##data_case <- lapply(unique(data_case$publisert), function(case) {

  add_section_information_case <- function(case) {
    data <- data_case[data_case$publisert == case, ]
    data$section <- NA

    ## Judges speaking as section_judge
    data <- data %>%
      mutate(section_judge = ifelse(grepl("^ *Domm[ea]r[ne]* .*:.*$", tekst),
                                    gsub("^ *Domm[ea]r[ne]* (.*?):.*$", "\\1", tekst), NA),
             section_judge = strsplit(gsub("\\.", "", section_judge), " og |, ")) %>%
      unnest() %>%
      mutate(section_judge = gsub("[dD]omm[ea]r[ne]* ", "", section_judge),
             section_judge = gsub("[dD]elvis", "", section_judge),
             section_judge = gsub("\\:|\\,|[kK]st ", "", section_judge),
             section_judge = gsub("Justitiarius ", "", section_judge),
             section_judge = gsub("og", "", section_judge),
             section_judge = gsub("^ +| +$", "", section_judge),
             section_judge = strsplit(section_judge, " ")) %>%
      unnest() %>%
      fill(section_judge)


    ## Func to add to section using pattern. Expects data$tekst.
    find_section <- function(patterns) {
      section_place <- as.numeric(unlist(sapply(patterns, grep, x = data$tekst)))
      section_place <- section_place[!is.na(section_place)]
      return(section_place)
    }

    ## Syllabus
    data$section[1] <- "syllabus"

    ## lower_court_excerpt
    lower_court_excerpt <- find_section(c("^ *Av herredsrettens dom .*:$"))
    data$section[lower_court_excerpt] <- "lower_court_excerpt"

    if (case %in% voting) {
      data$case_have_vote <- "voting"

      ## Main opinion
      main_opinion <- find_section(c("^ *Jeg er kommet til ",
                                     "^ *Jeg starter med å se",
                                     "^ *Jeg finner at",
                                     "^ *Jeg bemerker at saken",
                                     "^ *Mitt syn på saken:",
                                     "^ *Eg er komen til",
                                     "^ *Egne bemerkninger",
                                     "^ *Jeg ser først på",
                                     "^ *Jeg ser slik på saken:"))
      data$section[main_opinion] <- "Main opinion"

      ## Votes
      votes_1 <- find_section(c("^ *Eg røystar etter dette",
                                "[Jj]eg stemmer for",
                                "^dom:$",
                                "^ *Jeg stemmer etter dette",
                                "^Da jeg er i mindretall, former jeg ingen konklusjon"))
      votes_1 <- votes_1[1]
      votes <- find_section(c("^ *Domm[ea]r[ne]* ",
                              "^ *Justituarius ",
                              "^ *Justitiarius ",
                              "^ *Kst domm[ea]r "))
      if (length(main_opinion > 0)) {
        votes <- votes[votes > max(main_opinion)]
      } else {
        votes <- votes[votes != 1]
      }
      votes <- c(votes_1, votes)
      for (i in 1:length(votes)) data$section[votes[i]] <- paste0("vote_", i)

      ## Judgement
      judgement <- find_section(c("^ *Etter stemmegivningen avsa Høyesterett denne"))
      data$section[judgement] <- "judgement"

    } else {  # Non-voting decions

      data$case_have_vpte <- "non_voting"

      ## Main opinon
      main_opinion <- find_section(c("^ *Jeg er kommet til ",
                                     "^ *Høyesteretts ankeutvalg",
                                     "^ *Høyesteretts kompetanse",
                                     "^ *Jeg starter med å se",
                                     "^ *Jeg finner at",
                                     "^ *Jeg bemerker at saken",
                                     "^ *Mitt syn på saken:",
                                     "^ *Eg er komen til",
                                     "^ *Egne bemerkninger",
                                     "^ *Jeg ser først på",
                                     "^ *Jeg ser slik på saken:",
                                     "^ *Høyesterett bemerker at "))
      data$section[main_opinion] <- "Main opinion"

      ## Judgement
      judgement <- find_section(c("^ *Jeg stemmer for denne",
                                  "^ *Eg røystar etter dette"))
      data$section[judgement] <- "judgement"
    }

    data <- fill(data, section)

    return(data)
  }

  data_case <- parallelMap(add_section_information_case,
                           case = unique(data_case$publisert)) #, level = "case")
  data_case <- bind_rows(data_case)

return(data_case)
}
