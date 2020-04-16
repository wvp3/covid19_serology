library(tidyverse)
library(data.table)
library(readxl)

labresults <- read_excel("20200416incomlpete dataset for COVIDTF.xlsx") %>%
dplyr::rename_all(tolower) %>%
dplyr::rename(zip=`zip code`) %>%
dplyr::mutate(agegroup = case_when(
  age %in% 0:4 ~ "0-4",
  age %in% 5:17 ~ "5-17",
  age %in% 18:49 ~ "18-49",
  age %in% 50:64 ~ "50-64",
  age %in% 65:79 ~ "65-79",
  age >= 80 ~ "80+")) %>%
dplyr::mutate(zip=stringr::str_sub(zip,1,5)) %>%
dplyr::mutate(sex=case_when(
  sex %in% c("M","m") ~ "Male",
  sex %in% c("F","f") ~ "Female",
  TRUE ~ "Unknown"
)) %>%
dplyr::mutate(collection=as.Date(as.integer(collection),
                                 origin = as.Date("1899-30-12",format="%Y-%d-%m"))) %>%
# dplyr::mutate(received=as.Date(as.integer(received),
#                                origin = as.Date("1899-30-12",format="%Y-%d-%m"))) %>%
dplyr::filter(state %in% c("NY","WA")) 
  

labresultsunique <- filter(labresults,is.na(duplicate))

labresultsduplicate <- 
dplyr::filter(labresults,duplicate=="Yes") %>%
  # sort in reverse order so the most recent result is first
dplyr::arrange(desc(collection)) %>%
  # keep only distinct records
dplyr::distinct(age,sex,zip,duplicate,.keep_all=TRUE)

labresults_clean <- bind_rows(labresultsunique,labresultsduplicate) %>%
  select(-duplicate)

write.csv(labresults_clean,file="labresults_dedup_4-16-2020.csv",row.names=F)
