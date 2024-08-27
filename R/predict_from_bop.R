# model using just BOP WQ spreadhsheet
# wrangle BOP spreadsheets and save as parquet files
library(tidyverse)
library(here)
library(googlesheets4)
library(rvest)
library(duckplyr)
library(leaflet)
library(htmltools)
library(sf)
library(tidymodels)

source(here("r/func_pretty_truth_table.R"))

water_body_classifications <- read_csv(here("data/NYDEC_water_classifications.csv"),col_types = "fcc")
water_body_classifications
SAFE = 34

# Get Water Quality Data from BOP ----------------------------------

googlesheets4::gs4_deauth()
wq_url <-
  "https://docs.google.com/spreadsheets/d/1813b2nagaxZ80xRfyMZNNKySZOitro5Nt7W4E9WNQDA/edit?usp=sharing"


# DOWNLOAD meta data worksheet
wq_meta <- gs4_get(wq_url)
# take 400 rows of metadata to accomodate future growth in testing stations (up to 400 <grin>)
# assumes row 10 is column names, column A is site ID which is duplicated in column D
wq_meta_raw <- read_sheet(wq_url,"Information",range = "B10:BA400")

# Clean Meta Station Data ----------------------------------

wq_meta <- wq_meta_raw |>
  # remove empty columns by selecting only columns where names starts with a letter
  select(!matches("^\\.")) |>
  janitor::clean_names() |>
  rename("site" = 2) |>
  # remove empty rows
  filter(!is.na(site)) |>
  mutate(site_id = as.factor(site_id)) |>
  # why this comes in as a list of 1 is beyond me and one value is NULL
  # this is some complicated dplyr-fu.
  mutate(district_council_number =  as.factor(unlist(map(district_council_number,~ifelse(is.null(.x),NA,.x))))) |>
  # FIXED as of Aug 2024  some longitudes are erroneously positive
  mutate(longitude = if_else(longitude >0,-longitude,longitude)) |>
  mutate(currently_testing = as.logical(if_else(is.na(currently_testing),0,1))) |>
  rename_with(~"nyc_dep_wrrf_or_sewershed",starts_with("associated")) |>
  # make NA entries in character columns actual NA so there is only one kind of NA
  mutate(across(where(is.character),\(x) if_else(x == "N/A",NA,x))) |>
  # some columns are NA because they are in NJ. Make "NJ" the value
  mutate(district_council_number = if_else(district_council_number == "N/A","NJ",district_council_number)) |>
  mutate(nyc_dep_wrrf_or_sewershed = if_else(is.na(nyc_dep_wrrf_or_sewershed),"NJ",nyc_dep_wrrf_or_sewershed)) |>
  mutate(nys_dec_water_body_classification = if_else(is.na(nys_dec_water_body_classification),"NJ",nys_dec_water_body_classification))

# good practice to make complete factors of columns that are factors
# add all levels of water body classifications to the meta data
wq_meta$nys_dec_water_body_classification <-
  fct_expand(wq_meta$nys_dec_water_body_classification,levels(water_body_classifications$water_body_class))
# reorder factor levels
wq_meta$nys_dec_water_body_classification <-
  fct_relevel(wq_meta$nys_dec_water_body_classification,levels(water_body_classifications$water_body_class))

wq_meta

# get all levels of lab_analysis columns
labs <- wq_meta |> select(contains("lab_analysis")) |>
  pivot_longer(cols = everything(),names_to = "lab_analysis",values_to = "value") |>
  pull(value) |>
  unique()

wq_meta <- wq_meta |>
  mutate(across(contains("lab_analysis"),
                \(x) fct_expand(x,levels(labs),after=0)))

# convert all N/As in factor columns to a "missing" level
# This will let us keep rows with NA in the models but all missing will have the same value
# good or bad?
wq_meta <- wq_meta |>
  mutate(across(where(is.factor), ~ fct_na_value_to_level(.x,level = "missing")))

wq_meta


# convert wq_meta into a simple features object
wq_meta_sf <- wq_meta |>
  left_join(water_body_classifications,
            by = c("nys_dec_water_body_classification" = "water_body_class")) %>%
  st_as_sf(coords = c("longitude","latitude"),crs = 4326)


# display a leaflet map --------------------------------------------------------
map_labels <- glue::glue("<strong>{wq_meta_sf$site}</strong><br/>
                          Currently Testing? </strong>{wq_meta_sf$currently_testing}<br/>
                          Sewershed: {wq_meta_sf$nyc_dep_wrrf_or_sewershed}<br/>
                         Use: {wq_meta_sf$best_uses}") %>%
  lapply(htmltools::HTML)

wq_meta_sf %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = 5,
                            label = ~map_labels,
                            color = ~ifelse(currently_testing,"green","red"))



# DOWNLOAD water quality data worksheet ---------------------------------------------

# download and clean wq if parquet file does not already exist
if (!file.exists("data/wq_data.parquet")) {

data_names <- c("site","site_id","date","year","month","high_tide","sample_time","bacteria",
                  "precip_t0","precip_t1","precip_t2","precip_t3","precip_t4",
                  "precip_t5","precip_t6","notes")
wq_data_raw <- read_sheet(wq_url,"Data")

data_names <- c("site","site_id","date","year","month","high_tide","sample_time","bacteria",
                "precip_t0","precip_t1","precip_t2","precip_t3","precip_t4",
                "precip_t5","precip_t6","notes")



# get average sample time and use that for NA sample times
sample_time_avg <- wq_raw$`Sample Time` |>
  mean(na.rm = TRUE) |>
  as.POSIXct()

# function to extract day of week from date
day_of_week <- function(x) {
  x |>
    lubridate::wday(week_start = 7) |>
    factor(levels = 1:7,labels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
}


# clean up data
wq_data <- wq_data_raw |>
  set_names(data_names) |>
  mutate(date = as_date(date)) |>
  # change NA sample times to average of all sample times. Good idea?
  mutate(sample_time = if_else(is.na(sample_time),sample_time_avg,sample_time)) |>
  mutate(sample_time = hms::as_hms(sample_time)) |>
  mutate(sample_day = day_of_week(date),.before = bacteria) |>
  mutate(high_tide = hms::as_hms(high_tide)) |>
  # add date to sample time
  mutate(sample_time = ymd_hms(paste(date,sample_time),tz= "America/New_York")) |>
  mutate(high_tide = ymd_hms(paste(date,high_tide),tz= "America/New_York")) |>
  mutate(across(where(is.list), as.character)) |>
  # We chose to make "Trace" and "<10" into zero.
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "<10", "0"))) |>
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "Trace", "0"))) |>
  # > 24196 test limit?  This value is so far out of the range of the other values
  # that it might as well be infinity.
  mutate(across(where(is.character), .fns = ~ str_replace(.x, ">", ""))) |>
  # get rid of snow inches next to precip as water
  mutate(across(where(is.character), .fns = ~ str_replace(.x, "\\(.+\\)", ""))) |>
  mutate(across(where(is.character), .fns = ~ na_if(.x, "N/A"))) |>
  mutate(across(contains("precip"), as.numeric)) |>
  mutate(bacteria = as.numeric(bacteria)) |>
  mutate(notes = replace_na(notes, "")) |>
  # fix some typos
  mutate(site = str_replace(site, "Daylighted Section", "daylighted section")) |>
  mutate(site = str_replace(site, "Govenors", "Governors")) |>
  # classify bacteria levels according to NY DEP standards
  mutate(site = as.factor(site)) |>
  mutate(site_id = as.factor(site_id))
} else {
  # load file
  wq_data <- arrow::read_parquet("data/wq_data.parquet") %>%
    select(-precip_48)

}

# feature engineering
wq_data <- wq_data %>%
  # use of dplyr pipe neede
  # make 2-day precip column since 48-hour precip is a DEP standard
  # since observation time is typically in the morning don't include the current day's precip
  # since we don't know if it came before, during or after collection
  mutate(precip_week = rowSums(select(., starts_with("precip")), na.rm = TRUE),.after="bacteria") %>%
  mutate(precip_48 = rowSums(select(., precip_t1,precip_t2), na.rm = TRUE),.after="bacteria") %>%
  mutate(precip_earlier = rowSums(select(., precip_t3,precip_t4,precip_t5,,precip_t6), na.rm = TRUE),.after="precip_48") %>%
  select(-precip_t1,-precip_t2,-precip_t3,-precip_t4,-precip_t5,-precip_t6) %>%
  # categorize bacteria levels as quality levels
  mutate(quality = as_factor(cut(
    bacteria,
    breaks = c(-1, 34, 104, Inf),
    labels = c("Safe", "Caution", "Unsafe")
  ))) %>%
  # compute time between sample time and high tide
  mutate(time_since_high_tide = as.numeric(difftime(sample_time,high_tide,units = "hours")),
         .after = "sample_time") %>%
  # we won't be doing any month math so we can make it a factor
  mutate(month = as.factor(month)) %>%
  as_tibble()
# add some meta data that might be interesting in prediction
# add water body type,  water body and sewershed from metadata
wq_data <- wq_data %>%
  left_join(by = "site_id",select(wq_meta,
                   site_id,
                   water_body,
                   nys_dec_water_body_classification,
                   nyc_dep_wrrf_or_sewershed)
            ) |>
  # change all character columns to factors
  mutate(across(where(is.character), as.factor)) |>
  # rename columns
  rename(water_class = nys_dec_water_body_classification,
         sewershed = nyc_dep_wrrf_or_sewershed)

#  EDA -------------------------------------------------------------------------
wq_data %>%
  # select numeric columns
  select(where(is.numeric)) %>%
  skimr::skim()


# ggplot histogram of bacteria levels
wq_data %>%
  ggplot(aes(x = bacteria)) +
  geom_histogram(bins = 30,fill = "lightblue") +
  labs(title = "Histogram of Bacteria Levels",
       x = "Bacteria Levels",
       y = "Count") +
  theme_minimal()

# ggplot histogram of quality levels
wq_data %>%
  ggplot(aes(x = quality)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Histogram of Bacteria Quality Levels",
       x = "Quality Levels",
       y = "Count") +
  theme_minimal()

# histogram of months
wq_data |>
  ggplot(aes(x = as.factor(month))) +
  geom_bar(fill = "lightblue") +
  labs(title = "Histogram of Months",
       x = "Month",
       y = "Count") +
  theme_minimal()

# distribution of numeric features
wq_data |>
  ungroup() |>
  select(-year,-bacteria) |>
  select(is.numeric) |>
  gather() |>
  ggplot(aes(x = value)) +
  geom_histogram(bins=20,fill = "lightblue") +
  facet_wrap(~key, scales = "free") +
  labs(title = "Distribution of All Numeric Variables",
       y = "Count",x="") +
  theme_minimal()

# show that reporting stations have increased over time
wq_data |>
  mutate(year = year(date)) |>
  group_by(year) |>
  summarise(n = n_distinct(site_id)) |>
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "lightblue") +
  labs(title = "Reporting Stations Have Steadily Increased",
       x = "Year",
       y = "Number of Stations") +
  theme_minimal()

# get average daily rainfall by year
annl_rain <- wq_data %>%
  group_by(year) %>%
  summarise(annual_rainfall = mean(precip_week,na.rm = TRUE)*52)


# plot median bacteria levels over time only for sites that have been reporting for the all time periods
wq_10 <- wq_data |>
  # filter out stations that have been reporting for less than 10 years
  group_by(site) |>
  summarise(n_years = n_distinct(year)) |>
  filter(n_years > 9) |>
  select(site) |>
  inner_join(wq_data, by = "site") |>
  ungroup() |>
  filter(year(date) > year(Sys.Date())-11)

last_obs <- wq_data$date |> max()
first_obs <- year(last_obs)-10
rain_axis <-1

wq_10 |>
  ungroup() |>
  filter(year > 2011) |>
  summarise(.by = year, median_bacteria = median(bacteria)) %>%
  left_join(annl_rain) %>%
  arrange(year) |>
  ggplot(aes(x = year, y = median_bacteria)) + geom_col(fill = "lightblue") +
  geom_line(aes(x = year, y = annual_rainfall), color = "blue") +
  geom_hline(yintercept = 34, color = "red") +
  annotate("text", x = 2016, y = 34, label = '"Safe" Level = 34 Colonies', vjust = -1) +
  # label the y-axes
  scale_x_continuous(breaks = seq(2000,2024,1)) +
  # put totat_rainfall on secondary y-axis
  scale_y_continuous(sec.axis = sec_axis(~./rain_axis, name = "Annual Rainfall (Blue Line")) +
  labs(y = "Median Bacteria Concentration (Blue Bar)",
       title= str_to_title("water is not getting cleaner over time"),
       subtitle = glue::glue("Sites in NY Harbor Reporting Continuously from {first_obs} to {last_obs}")
  ) +
  theme_minimal()

# show boxplot of cleanest and dirtiest ----------------------------------------
site_boxplots <- function(wq_data, label = "Cleanest") {
  median_all <- median(wq_data$bacteria)
  # show a boxplot of bacteria concentration by site
  selected_sites <- wq_data |>
    # avoid Inf log values
    # mutate(bacteria = bacteria + .001) |>
    group_by(site) |>
    nest() |>
    rowwise() |>
    mutate(n_obs = nrow(data)) |>
    filter(n_obs > 100) |>
    mutate(median_bacteria = median(data$bacteria)) |>
    ungroup()

  if (label == "Cleanest") {
    selected_sites <- slice_min(selected_sites, order_by = median_bacteria, n = 10)
  } else {
    selected_sites <- slice_max(selected_sites, order_by = median_bacteria, n = 10)
  }

  selected_sites |>
    unnest(data) |>
    ggplot(aes(x = reorder(factor(site), median_bacteria), y = bacteria)) +
    scale_y_log10(oob = scales::squish_infinite) +
    geom_boxplot(fill = "lightblue") +
    # geom_violin(draw_quantiles = .5) +
    # geom_jitter(width = .1) +
    # annotate("text", x = log(SAFE)-2, y = 1500, label = "Safe Levels", color = "darkgreen") +

    labs(
      title = glue::glue("{label} Sites"),
      subtitle = "Based on Median Bacteria Count",
      x = "Site",
      y = "Boxplot of Enterococci Concentration\n(Log Scale)"
    ) +
    coord_flip() +
    geom_hline(yintercept = SAFE,
               color = "red",
               linewidth = 2) +
    annotate(
      "label",
      x = 9,
      y = SAFE,
      label = "Safe\nLevel",
      color = "red"
    ) +
    theme_minimal()
}

site_boxplots(wq_data, "Cleanest")
site_boxplots(wq_data, "Dirtiest")


# Model ------------------------------------------------------------------------
# select only variables for model
wq_subset <- wq_data %>%
  select(
    quality,
    site_id,
    year,
    month,
    time_since_high_tide,
    precip_t0,
    precip_48,
    precip_earlier,
    water_body,
    water_class,
    sewershed
  ) %>%
  mutate(year = as.factor(year), month = as.factor(month)) |>
  # remove rows with missing values
  drop_na()


# split data into training and testing sets
# set.seed(123)
wq_split <- initial_split(wq_subset, prop = 0.75, strata = quality)
wq_train <- training(wq_split)
wq_test <- testing(wq_split)

# create a recipe
wq_recipe <- recipe(quality ~ ., data = wq_train) |>
  step_normalize(all_numeric_predictors())

wq_rf <- rand_forest() |>
  set_engine("ranger",importance = "impurity") |>
  set_mode("classification")

wq_wf <- workflow() |>
  add_model(wq_rf) |>
  add_recipe(wq_recipe)

# fit with training data
wq_fit<- wq_wf |>
  fit(data = wq_train)

# show variable importance
wq_fit |>
  extract_fit_parsnip() |>
  vip::vip(aesthetics = list(fill = "blue")) +
  labs(title = "Water Quality Variable Importance",
       subtitle = 'Random Forest Classifier to Predict "Safe", "Caution" or "Unsafe ') +
  theme_minimal()

wq_pred <- wq_fit |>
  augment(new_data = wq_test)

# plot prediction confidence
wq_pred |>
  ggplot(aes(x = .pred_Safe, y = .pred_Unsafe, color = quality)) +
  facet_wrap(~quality) +
  geom_point() +
  geom_abline() +
  scale_color_manual(values = c("green", "orange","red")) +
  labs(title = "Prediction Confidence",
       subtitle = "Many predictions are Confidently Wrong",
       x = "Probablility Actual Is Safe",
       y = "Probablility Actual Is Unsafe")

# show a confusion matrix
xt <- wq_pred |>
  conf_mat(truth = quality, estimate = .pred_class) |>
  # return just the confusion matrix
  pluck("table") |>
  as_tibble() |>
  group_by(Truth) |>
  mutate(.prop = n/sum(n)) |>
  ungroup()

xt_count <- xt |>
  select(-.prop) |>
  pivot_wider(names_from = Prediction,values_from = n) |>
  rowwise() |>
  mutate(Total = sum(c(Safe,Caution,Unsafe)))

gt_domain <- xt_count |> select(-Total,-Truth) |> max()

xt_prop <- xt |>
  select(-n) |>
  pivot_wider(names_from = Prediction,values_from = .prop) |>
  rowwise() |>
  mutate(Total = sum(c(Safe,Caution,Unsafe)))


truth_table(xt_prop,"prop")

# plot a ROC curve
wq_pred_cl |>
  roc_curve(truth = quality, .pred_Safe,.pred_Caution,.pred_Unsafe) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity,color=.level)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

wq_pred_cl |>
  roc_auc(truth = quality, .pred_Safe,.pred_Caution,.pred_Unsafe)

wq_pred_cl |>
  roc_auc(truth = quality, .pred_Safe,.pred_Caution,.pred_Unsafe,estimator = "macro_weighted")

wq_pred_cl |>
  metrics(truth = quality, .pred_Safe,.pred_Caution,.pred_Unsafe,estimate = .pred_class)

wq_subset |> ggplot(aes(quality,time_since_high_tide)) + geom_boxplot()

