library(tidyverse)
library(ggplot2)
library(readxl)

###################################################################################################
###  Reading-in and preparing latest COVID-19 case data per county   ##########################
###################################################################################################
casefname <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
popfname <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv"

covdata <- read.csv(file = casefname,as.is = T, header = T)
studystates <- c("WA","NY","CT","UT","MO","FL","MN","GA","LA","CA","PA")

library(tidyverse)
countycases <- covdata %>%
  dplyr::rename_all(tolower) %>%
  filter(state %in% studystates) %>%
  gather(date,cases,5:length(names(covdata))) %>%
  mutate(date=lubridate::mdy(str_replace(date,"x",""))) %>%
  #filter(date > lubridate::mdy("03/01/2020")) %>%
  group_by(state,county.name) %>%
  arrange(state,county.name,date) %>%
  # "cases" data is already cumulative in source data; need moving diff "lag()"
  mutate(dailycases = cases-lag(cases,n=1))

# the following counties refer to the phase 1 commercial lab seroprevalence study
statecases <-  countycases %>%
  filter(
         (state=="NY" & county.name %in% c("Kings County","Queens County","Nassau County",
                                          "New York County","Bronx County")) |
         (state=="WA" & county.name %in% c("Kitsap County","Pierce County","Snohomish County",
                                          "King County","Skagit County")) |
         (state=="FL" & county.name %in% c("Miami-Dade County","Broward County","Martin County",
                                           "Palm Beach County")) |
         (state=="PA" & county.name %in% c("Lancaster County","Cumberland County",
                                           "Bucks County","Chester County","Montgomery County",
                                           "Delaware County","Philadelphia County",
                                           "Cumberland County","Lancaster County")) |
         (state=="MN" & county.name %in% c("Hennepin County","Ramsey County","Dakota County",
                                           "Anoka County","Washington County","Scott County",
                                           "Wright County","Carver County","Sherburne County",
                                           "St. Croix County","Chisago County","Pierce County",
                                           "Isanti County","Le Sueur County","Mille Lacs County",
                                           "Stearns County","Benton County","Rice County",
                                           "Goodhue County","Steele County","McLeod County")) |
         (state=="CA" & county.name %in% c("Alameda County","Contra Costa County","Marin County",
                                           "San Francisco County","San Mateo County",
                                           "Santa Clara County")) |
         (state %in% c("MO","CT","LA","UT","GA"))) %>%
  filter(date>=lubridate::mdy("03/01/2020")) %>%
  group_by(state,date) %>%
  summarise(cases=sum(cases,na.rm=T),dailycases=sum(dailycases,na.rm=T)) %>%
  ungroup() %>%
  # Scaling UT cases down; seroprevalence estimates excluded pediatric <=18 specimens
  mutate(cases=dplyr::if_else(state=="UT",round(cases*0.91),round(cases))) %>%
  # create a lagged date variable to compare cases and seroprevalence
  mutate(lagdate=date+14) %>%
  # calculate cumulative cases per 100,000 and rename for triptych facet order
  left_join(catchment_pops,by=c("state"="site")) %>%
  mutate(cumcasesper10k = (cases/pop)*10000)  %>%
  dplyr::rename(`3_cumulative_cases_per_10k`=cumcasesper10k,`2_daily_cases`=dailycases)
  

## Triptych function.  This requires the "all_results" df and the cases df created separately
# function requires state name abbreviation. can choose to add a custom chart title
# this function requires a timing file created separately
# this function requires the state cases df creatd above
plot_triptych <- function(st,title="") {
  results_plot <- all_results %>% 
    mutate(site=str_sub(site,1,2)) %>%
    filter(site==st) %>% 
    filter(as.numeric(round) %in% 1:5) %>%
    filter(label=="Total age-sex adjusted") %>%
    mutate(data="1_seroprevalence_data")
  
  cases_plot <- filter(statecases,state==st) %>%
    select(-cases) %>%
    gather(data,value,contains("cases")) %>%
    filter(date>(min(results_plot$lastdate,na.rm=T)-15),
           date<(max(results_plot$lastdate,na.rm=T)+5))
  
  time <- timing %>%
    mutate(state=str_sub(state,1,2)) %>%
    filter(state==st) 
  
  if ("newround" %in% names(time)) {time <- time %>% select(-round) %>% dplyr::rename(round=newround)}
  rectangle <- data.frame(starttemp=NA,endtemp=NA)
  for (i in unique(results_plot$round)) {
    rectangle$starttemp<-time$firstdate[time$round==as.character(i)]
    rectangle$endtemp<-time$lastdate[time$round==as.character(i)]
    rectangle <- rectangle %>% dplyr::rename_at(vars(starttemp,endtemp),funs(str_replace(.,"temp",as.character(i))))
  }
  rectangle$vert<-Inf 
  
  triptych <- ggplot() +
    geom_area(data=cases_plot,mapping=aes(x=date,y=value),fill="#456383",stat = "identity",alpha=1) +
    labs(title = NULL,y = "Number of Cases",x="Date") +
    ylim(0,NA)+
    facet_grid(data ~ .,scales="free")+
    geom_point(filter(results_plot,label=="Total age-sex adjusted"),
               mapping=aes(x=meddate,y=point_estimate,label=seroprevalence))+
    geom_text(filter(results_plot,label=="Total age-sex adjusted"),
              mapping=aes(x=meddate,y=point_estimate,label=seroprevalence),
              size=5,nudge_y=0.004,nudge_x=-1)+
    geom_line(results_plot,mapping=aes(x=meddate,y=point_estimate,ymax=upper,ymin=lower))+
    geom_linerange(results_plot,mapping=aes(x=meddate,y=point_estimate,ymax=upper,ymin=lower))+
    theme_bw()+
    theme(text=element_text(size=14))
    
    for (i in unique(results_plot$round)) {
    triptych <- triptych + geom_rect(data=rectangle,mapping=aes_string(
      xmin=paste0("start",i),
      xmax=paste0("end",i),ymin="-vert",ymax="vert"),
      alpha=0.4,fill="grey")
    }
  if (title!="") {triptych <- triptych+ggtitle(title)}
  
  return(triptych)
  }
