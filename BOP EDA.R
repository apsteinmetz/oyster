# Billion Oyster Project EDA

library(tidyverse)
library(googlesheets4)
library(lubridate)

wq_url <-
"https://docs.google.com/spreadsheets/d/1813b2nagaxZ80xRfyMZNNKySZOitro5Nt7W4E9WNQDA/edit?usp=sharing"

wq_meta_raw <- read_sheet(wq_url,"Information",range = "A10:J400")
wq_data_raw <- read_sheet(wq_url,"Data")

wq_meta <- wq_meta_raw
names(wq_meta) <- names(wq_meta) %>% str_replace_all(" ","_")
names(wq_meta) <- names(wq_meta) %>% str_replace_all("/","_")
names(wq_meta)[2] <- "Site"

wq_meta <- wq_meta %>%
  filter(!is.na(Site)) %>%
  mutate(Currently_Testing = if_else(is.na(Currently_Testing),0,1)) %>%
  mutate(Currently_Testing = as.logical(Currently_Testing))


wq_data <- wq_data_raw
names(wq_data) <- names(wq_data) %>% str_replace_all(" ","_")
names(wq_data) <- names(wq_data) %>% str_replace_all("/","_")


mash_datetime <- function(in_date,in_datetime){
  date(in_datetime) <- date(in_date)
  return(in_datetime)
}

wq_data %>%
  mutate(Full_Date = as_date(Full_Date)) %>%
  mutate(Sample_Time = hms::as_hms(Sample_Time)) %>%
  mutate(Battery_High_Tide = hms::as_hms(Battery_High_Tide))


