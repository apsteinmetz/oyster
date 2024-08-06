# look at weather data on sample days

weather <- arrow::read_parquet("data/wq_model_data.parquet") |>
  select(date,site,ghcn_precip_in) |>
  mutate(month = lubridate::month(date)) |>
  mutate(year = lubridate::year(date))

weather %>% glimpse()

monthly_weather <- weather |>
  group_by(year, month,site) |>
  summarise(avg_rainfall = mean(ghcn_precip_in, na.rm = TRUE),
            n = n())

yearly_weather <- weather |>
  group_by(year,site) |>
  summarise(total_rainfall = mean(ghcn_precip_in*365, na.rm = TRUE),
            n = n())

yearly_weather$n |> hist()
# plot annual total rainfall
yearly_weather |>
  ggplot(aes(x = factor(year), y = total_rainfall)) +
  geom_boxplot() +
  labs(title = "Total Rainfall by Year",
       x = "Year",
       y = "Total Rainfall (inches)") +
  theme_minimal()


# use full weather history
precip_year <-arrow::read_parquet("data/weather_ghcn.parquet") |>
  select(date,ghcn_station_name,precip_in) |>
  mutate(year = lubridate::year(date)) |>
  summarise(.by = c(year,ghcn_station_name),
            n = n(),
            precip = mean(precip_in, na.rm = TRUE)*365) |>
  na.omit() |>
  # fewer observations make for less useful generalizations
  filter(year == 2023, n > 200)

precip_year |>
  ggplot(aes(x = factor(ghcn_station_name), y = precip,group = ghcn_station_name)) +
  geom_boxplot() +
  labs(title = "There Is Significant Variation In Annual Rainfall Across Stations",
       subtitle = "Total Rainfall in 2023",
       x = "NOAA Stations With At Least 200 Observations in 2023",
       y = "Annualized Total Rainfall (inches)") +
  theme_minimal() +
  coord_flip()
