require(tidyverse)
require(readxl)
require(data.table)
require(reshape2)
require(lubridate)
require(kableExtra)

zipdata <- read.csv("ZIP-COUNTY-FIPS_2018-03.csv", as.is = T,header = T) %>% 
  mutate(ZIP=as.character(ZIP))
names(zipdata) <- tolower(names(zipdata))

read_gfat <- function(filename) {
  df <- readxl::read_excel(as.character(filename),sheet="Human FA Template") %>%
    slice(.,2:n()) %>%
    dplyr::select(-contains("Intermediate Submitter",ignore.case=T)) %>%
    dplyr::select(CUID,`Date Sent to CDC`,`Patient Age`,`Patient Sex`,
           `Specimen collected date`,contains("ZIP",ignore.case=T)) %>%
    dplyr::rename(age = `Patient Age`,
                  sex = `Patient Sex`,
                  datesent = `Date Sent to CDC`,
                  specimendate = `Specimen collected date`,
                  zip = `Original Submitter Address, Zip/Postal Code`
                  ) %>%
    dplyr::mutate(age=as.numeric(age)) %>%
    dplyr::mutate(agegroup = case_when(
                  age %in% 0:4 ~ "0-4",
                  age %in% 5:17 ~ "5-17",
                  age %in% 18:49 ~ "18-49",
                  age %in% 50:64 ~ "50-64",
                  age %in% 65:79 ~ "65-79",
                  age >= 80 ~ "80+")) %>%
    filter(!is.na(sex)) %>%
    mutate(zip=stringr::str_sub(zip,1,5)) %>%
    left_join(zipdata,by="zip") %>%
    dplyr::mutate(datesent = as.Date(as.integer(datesent),
                                     origin = as.Date("1899-30-12",format="%Y-%d-%m"))) %>%
    dplyr::mutate(specimendate = as.Date(as.integer(specimendate),
                                     origin = as.Date("1899-30-12",format="%Y-%d-%m"))) 
  return(df)
}

test <- read_gfat("gfat 14Apr2020 LabCorp.xlsx")

manifest <- read_excel("SR 317 CDC Shipping Manifest-ALL.xlsx",sheet="Combined") %>%
  dplyr::rename(age=`Patient Age`,sex=`PT Gender`,zip="Zip Code",
                specimendate=`Collection Date`) %>%
  dplyr::mutate(age=as.numeric(age)) %>%
  dplyr::mutate(agegroup = case_when(
    age %in% 0:4 ~ "0-4",
    age %in% 5:17 ~ "5-17",
    age %in% 18:49 ~ "18-49",
    age %in% 50:64 ~ "50-64",
    age %in% 65:79 ~ "65-79",
    age >= 80 ~ "80+")) %>%
  mutate(zip=stringr::str_sub(zip,1,5)) %>%
  left_join(zipdata,by="zip") %>%
  mutate(sex=case_when(
         sex=="M" ~ "Male",
         sex=="F" ~ "Female",
         TRUE ~ "Unknown"
  )) %>%
  dplyr::mutate(datesent=lubridate::mdy("4/14/2020")) %>%
  dplyr::mutate(specimendate = as.Date(as.integer(specimendate),
                                       origin = as.Date("1899-30-12",format="%Y-%d-%m")))

subtotal <- bind_rows(test,manifest) %>%
  mutate(studystate=ifelse(
    !(state %in% c("WA","NY")),"Other",
    state))

saveRDS(subtotal,file="result1.rds")
 

table2 <- subtotal %>% 
 # filter(state %in% c("NY","WA")) %>%
  group_by(state,agegroup) %>% 
  summarise(N_samples=n()) %>% 
  tidyr::spread(agegroup,N_samples) %>%
  select(state,`5-17`,names(.)) %>%
  ungroup() %>%
  bind_rows(.,summarise_at(.,vars(`5-17`:`80+`),list(sum))) %>%
  mutate(state=ifelse(is.na(state),"Total",state))
# 
# tab2 <- as.matrix(table2[,2:6]) 
# round((tab2 / rowSums(tab2)) *100,1)
# paste()

table3 <- subtotal %>% 
  #filter(state %in% c("NY","WA")) %>%
  group_by(state,sex) %>% 
  summarise(N_samples=n()) %>% 
  tidyr::spread(sex,N_samples) %>%
  ungroup() %>%
  bind_rows(.,summarise_at(.,vars(Male,Female,`Not applicable`,Unknown),list(sum))) %>%
  mutate(state=ifelse(is.na(state),"Total",state))
  
  DT::renderDataTable()

table1 <-  subtotal %>% 
    #filter(state %in% c("NY","WA")) %>%
    group_by(state,specimendate) %>% 
    summarise(N_samples=n()) %>% 
    tidyr::spread(specimendate,N_samples) %>%
  ungroup() %>%
  bind_rows(.,summarise_at(.,vars(2:length(names(.))),list(sum),na.rm=T)) %>%
  mutate(state=ifelse(is.na(state),"Total",state)) %>%
  mutate(total=select(.,2:length(names(.))) %>% rowSums(.,na.rm=T)) %>%
  ungroup() %>%
  datatable(rownames = F)