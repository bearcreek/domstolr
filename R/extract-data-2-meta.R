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

## First, detect method
## Method 1: no votes being cast
## Method 2: votes being cast
# voting <- as.numeric(grep("Dommer", data$tekst))
#   if (vote == 1)
#     class(.case) <- c("non_voting", class(.case))
#   else
#     class(.case) <- c("voting", class(.case))
#   
#   ## Then use it
#   UseMethod("...", .case)
#   }

add_data_section <- function(data_case) {
    data_case <- lapply(unique(data_case$publisert), function(case) {
      data <- data_case[data_case$publisert == case, ]
      data$seksjon <- NA
      if (any(grepl("Jeg er kommet til ", data$tekst))) {
        data$seksjon[grep("Jeg er kommet til ", data$tekst)] <- "Main opinion"
        data$seksjon[grep("Jeg stemmer for ", data$tekst)] <- "Judgement"
        if (!is.na(data$seksjon[1])) {
          data$seksjon[1] <- "Syllabus"
        }
      } 
      ## Voting
    data$seksjon[grep("Dommer", data$tekst)[grep("Dommer", data$tekst) > 1][1]] <- "annenvoterende"
      data <- fill(data, seksjon)
      data$seksjon[is.na(data$seksjon)] <- "Syllabus"
      return(data)
    })
    data_case <- bind_rows(data_case)
    return(data_case)
  }
