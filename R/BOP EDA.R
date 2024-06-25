# Billion Oyster Project EDA

library(tidyverse)
library(RcppRoll)
library(googlesheets4)
library(lubridate)
library(rnoaa)
library(rvest)

# Get Water Quality Data ----------------------------------

googlesheets4::gs4_deauth()
wq_url <-
"https://docs.google.com/spreadsheets/d/1813b2nagaxZ80xRfyMZNNKySZOitro5Nt7W4E9WNQDA/edit?usp=sharing"

wq_data_meta <- gs4_get(wq_url)
wq_meta_raw <- read_sheet(wq_url,"Information",range = "A10:J400")
wq_data_raw <- read_sheet(wq_url,"Data")



# riverkeeper data ----------------------------------
# rk_url <- "https://www.hydroshare.org/resource/e22138bd77914201af48fce5bfc458f4/data/contents/Riverkeeper_Community_Monitoring_FIB_Data_Open_20220420.xlsx"
# rK_path <- "~/downloads/Riverkeeper_Community_Monitoring_FIB_Data_Open_20220420.xlsx"
# rk_data_raw <- readxl::read_xlsx(rK_path)

# rk_data_raw <- read_sheet(wq_url,"Data")


# Wrangle Data ----------------------------------
wq_meta <- wq_meta_raw %>%
  janitor::clean_names() |>
  rename("site" = 2) %>%
  filter(!is.na(site)) %>%
  # some longitudes are erroneously positive
  mutate(longitude = if_else(longitude >0,-longitude,longitude)) |>
  mutate(currently_testing = if_else(is.na(currently_testing),0,1)) %>%
  mutate(currently_testing = as.logical(currently_testing))
  # mutate(district_council_number = unlist(district_council_number)) |>


data_names <- c("site","date","year","month","high_tide","sample_time","bacteria",
                "precip_t0","precip_t1","precip_t2","precip_t3","precip_t4",
                "precip_t5","precip_t6","notes")

wq_data <- wq_data_raw |>
  set_names(data_names) |>
  mutate(date = as_date(date)) %>%
  mutate(sample_time = hms::as_hms(sample_time)) %>%
  mutate(high_tide = hms::as_hms(high_tide)) %>%
  mutate(sample_time = ymd_hms(paste(date,sample_time),tz= "America/New_York")) |>
  mutate(high_tide = ymd_hms(paste(date,high_tide),tz= "America/New_York")) |>
  mutate(across(where(is.list), as.character)) %>%
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "<10", "0"))) %>%
  # > 24196 test limit?
  mutate(across(where(is.character), .fns = ~ str_replace(.x, ">", ""))) %>%
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "Trace", "0"))) %>%
  # get rid of snow inches next to precip as water
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "\\(.+\\)", ""))) %>%
  mutate(across(where(is.character), .fns = ~ na_if(.x, "N/A"))) %>%
  mutate(across(contains("precip"), as.numeric)) %>%
  mutate(precip_wk = rowSums(select(., starts_with("precip")), na.rm = TRUE),.after="bacteria") |>
  mutate(bacteria = as.numeric(bacteria)) %>%
  mutate(notes = replace_na(notes, "")) %>%
  # fix some typos
  mutate(site = str_replace(site, "Daylighted Section", "daylighted section")) %>%
  mutate(site = str_replace(site, "Govenors", "Governors")) %>%
  mutate(quality = cut(
    bacteria,
    breaks = c(-1, 0, 35, 104, 49999),
    labels = c("Not Detected", "Good", "Fair", "Unacceptable")
  )) %>%
  mutate(site = as.factor(site))


# Save Data ----------------------------------
arrow::write_parquet(wq_data,"data/wq_data.parquet")
arrow::write_parquet(wq_meta,"data/wq_meta.parquet")

load("data/weather.rdata")
load("data/tides.rdata")

weather <- weather |>
  mutate(rain_7D = roll_sum(PRCP, 7, fill = NA, align = "right"))

rain_comp <- wq_data |>
  summarize(.by = date,precip_wk = mean(precip_wk,),precip = mean(precip_t0)) |>
  left_join(weather)

ggplot(rain_comp,aes(precip_wk,rain_7D)) + geom_point() +
  labs(title = "Rainfall Comparison",
       x = "Weekly Rainfall(in.) from CWQT Master Sheet",
       y = "Weekly Rainfall(in.) from NOAA Central Park Station")

ggplot(rain_comp,aes(precip,PRCP)) + geom_point() +
  labs(title = "Rainfall Comparison",
       x = "Daily Rainfall(in.) from CWQT Master Sheet",
       y = "Daily Rainfall(in.) from NOAA Central Park Station")

# precip_data <- wq_data %>%
#   select(date,starts_with("precip")) %>%
#   unique() %>%
#   pivot_longer(cols=starts_with("precip"),names_to = "lag",values_to = "precip") %>%
#   mutate(lag = as.numeric(str_remove(lag,"precip_t"))) %>%
#   mutate(date = date + lag) %>%
#   select(-lag) %>%
#   arrange(date)

# EDA ---------------------------------------------

bacteria_by_site <- wq_data %>%
  group_by(site) %>%
  summarise(bacteria = round(mean(bacteria,na.rm =TRUE)))

bacteria_by_site %>%
  #  filter(bacteria < 500) %>%
  ggplot(aes(bacteria)) + geom_histogram()

bacteria_by_site <- wq_data %>%
  group_by(site,quality) %>%
  tally()



wq_data %>%
  filter(bacteria < 200) %>%
  group_by(month) %>%
  ggplot(aes(month,bacteria,group = month)) + geom_boxplot()

load("data/weather.rdata")
weather_by_month <- weather %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise(temp = mean(TEMP),rain = mean(PRCP))

weather_by_month %>%
  ggplot(aes(month,temp)) + geom_col()

wq_data %>%
  filter(bacteria < 200) %>%
  group_by(month) %>%
  ggplot(aes(as.factor(month),bacteria,group = month)) + geom_boxplot() +
  geom_line(data=weather_by_month,aes(as.factor(month),temp,group=month))


closest_val <- function(vec,val){
  vec[which.min(abs(vec-val))]
}

all_dates <- wq_data$date %>% unique()
weather <- weather %>%
  mutate(rain_7D = roll_sum(PRCP, 7, fill = NA, align = "right"))  %>%
  filter(date %in% all_dates) %>%
  mutate(temp_color = cut(TEMP,5,labels = c("blue","lightblue","green","yellow","red"))) %>%
  mutate(rain_color = cut(rain_7D,5,labels = colorRampPalette(c("blue", "red"))( 5 ))
)

max_rain <- max(weather$rain_7D,na.rm = T)

weather_1d <- weather %>%
  filter(date == closest_val(all_dates, sample(all_dates, 1)))
temp_color_1d <- weather_1d$temp_color
rain_color_1d <- weather_1d$rain_color

weather_1d %>%
  ggplot(aes(date,rain_7D)) + geom_col(fill = rain_color_1d) +
  scale_y_continuous(limits = c(0,2))

weather_1d %>%
  ggplot(aes(date,TEMP)) + geom_col(fill=temp_color_1d) +
  scale_y_continuous(limits =c(0,100))


