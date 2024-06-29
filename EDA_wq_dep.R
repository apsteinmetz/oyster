# NYC DEP EDA
library(tidyverse)
library(lubridate)
library(arrow)

# Load data dep data
wq_data_raw <- read_parquet("data/wq_data_dep_subset.parquet")

# plot nutrients over time
wq_data_raw %>%
  mutate(year = year(sample_datetime)) |>
  select(year,starts_with("nutrient"),oxygen_ml_l) %>%
  mutate(oxygen_ml_l = oxygen_ml_l/100) %>%
  pivot_longer(cols = -year, names_to = "nutrient", values_to = "value") %>%
  mutate(nutrient = str_remove(nutrient, "nutrient_")) |>
  mutate(nutrient = str_remove(nutrient, "_m(g|l)_l")) |>
  drop_na() |>
  group_by(year, nutrient) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = value, color = nutrient)) +
  geom_line() +
  #geom_smooth(span = .01,method = "loess",se = FALSE) +
  #scale_x_date(date_breaks = "2 years",date_labels = "%y") +
  labs(title = "Nutrient Concentrations Over Time",
       source = "NYC DEP Water Quality Data",
       subtitle = "Annual Average",
       x = "Date",
       y = "Concentration (mg/L)",
       color = "Nutrient") +
  theme_minimal()

# get station count
station_count <- wq_data_raw %>%
  mutate(year = year(sample_datetime)) |>
  select(year,sampling_location,contains("bacteria")) |>
  distinct(year, sampling_location) |>
  # get count of sampling locations and average annual bacteria
  group_by(year) %>%
  summarise(station_count = n()) |>
  arrange(year)


# plot bacteria over time
# it looks like 2018 is the start of credible data
wq_data_raw %>%
  mutate(year = year(sample_datetime)) |>
  select(year,sampling_location,contains("bacteria")) %>%
  pivot_longer(cols = -c(year,sampling_location), names_to = "bacteria", values_to = "count") %>%
  mutate(bacteria = str_remove(bacteria, "bacteria_")) |>
  drop_na() |>
  group_by(year,bacteria) %>%
  summarise(bacteria_count = mean(count, na.rm = TRUE)) |>
  left_join(station_count, by = "year") %>%
  ggplot() +
  geom_col(aes(x = year, y = bacteria_count,fill = bacteria)) +
  geom_line(aes(x = year, y = station_count*100),color = "black") +
  # put station count on secondary axis
   scale_y_continuous(sec.axis = sec_axis(~./100,name = "Reporting Stations (black line)")) +
  #geom_smooth(span = .01,method = "loess",se = FALSE) +
  #scale_x_date(date_breaks = "2 years",date_labels = "%y") +
  labs(title = "Bacteria Concentrations Over Time",
       source = "NYC DEP Water Quality Data",
       subtitle = "Annual Average",
       x = "Year",
       y = "Count/100mL)") +
  theme_minimal()

# load meta data
stations_metadata <- read_parquet("data/wq_meta_dep.parquet")

wq_data <- wq_data_raw %>%
  left_join(select(stations_metadata,sampling_location,Area), by = "sampling_location") %>%
  mutate(Area = if_else(is.na(Area), "Unknown", Area)) %>%
  filter(year(sample_datetime) >= 2018) |>
  mutate(across(where(is.character), as.factor)) |>
  select(-sample_datetime) |>
  drop_na() |>
  select(Area,everything())

# only 6287 rows of full data
skimr::skim(wq_data)
wq_data$bacteria_enterococci_100ml |> hist()
wq_data$bacteria_fecal_coliform_100ml |> hist()

# probably need to scale bacteria by interquartile range

