
## library(tidyverse)

## doranoh <- readxl::read_excel("data/Decisions_T1_saksegenskaper-for-domstolr_v2.xlsx")
## domstolr <- domstolr::domstolr_import("data/hr_scrape_2014.html")


## domstolr_merge_doranoh <- function(domstolr, doranoh) {

## }

## domstolr <- data

## case <- domstolr[1, ]



## cases <- unique(domstolr$publisert)

## out <- lapply(case, function(.c) {

##   .c <- domstolr$publisert[1]

##   data <- filter(domstolr, publisert == .c)
##   data_don <- filter(doranoh, lubridate::ymd(doranoh$Dato) == lubridate::ymd(data$dato[1]))

##   gsub("(HR.*)( \\-.*)$", "\\1", doranoh$FullNummer)[1:100]

##   data$publisert[1] %in% data_don$Publisert


##   mat <- match(lubridate::ymd(case$dato), lubridate::ymd(doranoh$Dato))

## })
