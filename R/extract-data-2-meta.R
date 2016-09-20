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

## Section: Extract properties of the text, such as which judges
## that are speaking.

## ## First, detect method
## ## Method 1: no votes being cast
## ## Method 2: votes being cast
## voting <- as.numeric(grep("Dommer", data$tekst))
##   if (vote == 1)
##     class(.case) <- c("non_voting", class(.case))
##   else
##     class(.case) <- c("voting", class(.case))

add_data_section <- function(data_case) {
  voting <- data_case$publisert[grep("Dommer", data_case$tekst)]
  voting <- voting[!voting %in% which(data_case$avsnitt != 1)]
  voting <- unique(voting)

  data_case <- lapply(unique(data_case$publisert), function(case) {

    data <- data_case[data_case$publisert == case, ]

    data$seksjon <- NA

    ## Syllabus
    data$seksjon[1] <- "Syllabus"

    if (case %in% voting) {
      data$voting <- "voting"

      ## Main opinion
      pattern_main <- c("^ *Jeg er kommet til ", "^ *Jeg starter med å se", "^ *Jeg finner at", "^ *Jeg bemerker at saken", "^ *Mitt syn på saken:", "^ *Eg er komen til", "^ *Egne bemerkninger", "^ *Jeg ser først på", "^ *Jeg ser slik på saken:")
      main_opinion <- as.numeric(unlist(sapply(pattern_main, grep, x = data$tekst)))
      main_opinion <- main_opinion[!is.na(main_opinion)]
      data$seksjon[main_opinion] <- "Main opinion"

      ## Votes
      pattern_votes_1 <- c("^ *Eg røystar etter dette", "^ *Jeg stemmer for")
      votes_1 <- as.numeric(unlist(sapply(pattern_votes_1, grep, x = data$tekst)))
      votes_1 <- votes_1[!is.na(votes_1)]
      pattern_votes <- c("^ *Dommer ", "^ *Justituarius ", "^ *Justitiarius ", "^ *Kst dommer ")
      votes <- as.numeric(unlist(sapply(pattern_votes, grep, x = data$tekst)))
      votes <- votes[!is.na(votes)]
      message(paste0("main_opinion: ", main_opinion, " | publisert: ", case))
      if (length(main_opinion > 0)) {
        votes <- votes[votes > max(main_opinion)]
      } else {
        votes <- votes[votes != 1]
      }
      votes <- c(votes_1, votes)
      for (i in 1:length(votes)) data$seksjon[votes[i]] <- paste0("vote_", i)

      ## Judgement
      pattern_judgement <- c("^ *Etter stemmegivningen avsa Høyesterett denne")
      judgement <- as.numeric(unlist(sapply(pattern_judgement, grep, x = data$tekst)))
      judgement <- judgement[!is.na(judgement)]
      data$seksjon[judgement] <- "Judgement"

    } else {
      data$voting <- "non_voting"

      ## Main opinon
      pattern_main <- c("^ *Jeg er kommet til ", "^ *Høyesteretts ankeutvalg", "^ *Høyesteretts kompetanse", "^ *Jeg starter med å se", "^ *Jeg finner at", "^ *Jeg bemerker at saken", "^ *Mitt syn på saken:", "^ *Eg er komen til", "^ *Egne bemerkninger", "^ *Jeg ser først på", "^ *Jeg ser slik på saken:", "^ *Høyesterett bemerker at ")
      main_opinion <- as.numeric(unlist(sapply(pattern_main, grep, x = data$tekst)))
      main_opinion <- main_opinion[!is.na(main_opinion)]
      data$seksjon[main_opinion] <- "Main opinion"

      ## Judgement
      pattern_judgement <- c("^ *Jeg stemmer for denne", "^ *Eg røystar etter dette")
      judgement <- as.numeric(unlist(sapply(pattern_judgement, grep, x = data$tekst)))
      judgement <- judgement[!is.na(judgement)]
      data$seksjon[judgement] <- "Judgement"
    }

    data <- fill(data, seksjon)

    ##gsub("Dommer ([A-ZÆØÅ].*): .*$", "\\1", "Dommer Hei-ho: fdsafdsa fsda ")

    return(data)
  })
  data_case <- bind_rows(data_case)
  return(data_case)
}
