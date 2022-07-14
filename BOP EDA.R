# Billion Oyster Project EDA

library(tidyverse)
library(googlesheets4)
library(lubridate)

# Get Data ----------------------------------
wq_url <-
"https://docs.google.com/spreadsheets/d/1813b2nagaxZ80xRfyMZNNKySZOitro5Nt7W4E9WNQDA/edit?usp=sharing"

wq_meta_raw <- read_sheet(wq_url,"Information",range = "A10:J400")
wq_data_raw <- read_sheet(wq_url,"Data")

# Wrangle Data ----------------------------------
wq_meta <- wq_meta_raw %>%
  rename_with(~str_replace_all(.x," ","_")) %>%
  rename_with(~str_replace_all(.x,"/","_")) %>%
  rename("site" = 2) %>%
  filter(!is.na(site)) %>%
  mutate(Currently_Testing = if_else(is.na(Currently_Testing),0,1)) %>%
  mutate(Currently_Testing = as.logical(Currently_Testing)) %>%
  rename_with(tolower) %>%
  mutate(site = as.factor(site))


data_names <- c("site","date","year","month","high_tide","sample_time","bacteria",
                "precip_t0","precip_t-1","precip_t-2","precip_t-3","precip_t-4",
                "precip_t-5","precip_t-6","notes")

wq_data <- wq_data_raw
names(wq_data) <- data_names
wq_data <- wq_data %>%
  mutate(date = as_date(date)) %>%
  mutate(sample_time = hms::as_hms(sample_time)) %>%
  mutate(high_tide = hms::as_hms(high_tide)) %>%
  mutate(across(where(is.list), as.character)) %>%
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "<10", "0"))) %>%
  # > 24196 test limit?
  mutate(across(where(is.character), .fns = ~ str_replace(.x, ">", ""))) %>%
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "Trace", "0"))) %>%
  # get rid of snow inches next to precip as water
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "\\(.+\\)", ""))) %>%
  mutate(across(where(is.character), .fns = ~ na_if(.x, "N/A"))) %>%
  mutate(across(contains("precip"), as.numeric)) %>%
  mutate(bacteria = as.numeric(bacteria)) %>%
  mutate(notes = replace_na(notes, "")) %>%
  mutate(site = as.factor(site)) %>%
  # fix some typos
  mutate(site = str_replace(site, "Daylighted Section", "daylighted section")) %>%
  mutate(site = str_replace(site, "Govenors", "Governors")) %>%
  mutate(quality = cut(
    bacteria,
    breaks = c(-1, 0, 35, 104, 49999),
    labels = c("Not Detected", "Good", "Fair", "Unacceptable")
  ))


# EDA ---------------------------------------------

bacteria_by_site <- wq_data %>%
  group_by(site) %>%
  summarise(bacteria = round(mean(bacteria,na.rm =TRUE)))

bacteria_by_site <- wq_data %>%
  group_by(site,quality) %>%
  tally()


bacteria_by_site %>%
#  filter(bacteria < 500) %>%
  ggplot(aes(bacteria)) + geom_histogram()

wq_data %>%
  filter(bacteria < 200) %>%
  group_by(month) %>%
  ggplot(aes(as.factor(month),bacteria)) + geom_boxplot()
