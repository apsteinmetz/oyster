# NYC DEP EDA
library(tidyverse)
library(lubridate)
library(arrow)
library(tidymodels)
library(rpart.plot)
library(vip)

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

# no feature engineering
wq_data_subset <- wq_data |> select(-contains("fecal"))
wq_recipe <- recipe(bacteria_enterococci_100ml ~ ., data = wq_data_subset)

# so many of the predictors and responses are skewed  so a tree-based model
# is a good choice
# build a tidymodels workflow
show_engines("rand_forest")
set.seed(123)
rf_mod <- rand_forest() %>%
  set_engine("ranger",importance = "impurity") %>%
  set_mode("regression")

wq_workflow <- workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(wq_recipe)

rf_fit <- wq_workflow |>
  fit(data = wq_data)

wq_fold <- vfold_cv(wq_data, v = 5)
rf_tunings <-
  wq_workflow |>
  tune_grid(resamples = wq_fold, grid = 5)


rf_best_model <- rf_tunings |>
  select_best(metric = "rmse")

final_wf <- wq_workflow |>
  finalize_workflow(rf_best_model)

final_fit <- final_wf |>
  fit(data = wq_data)


# rpart ------------------------------------------------------------------------
#wq_split <- initial_split(wq_data_subset, prop = 0.75, strata = bacteria_enterococci_100ml)
#wq_train <- training(wq_split)
#wq_test  <- testing(wq_split)

wq_folds <- vfold_cv(wq_data_subset, v = 5,strata = bacteria_enterococci_100ml,breaks = 4)

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)
dt_mod <-
  decision_tree() %>%
  set_engine("rpart",model = TRUE) %>%
  set_mode("regression")

dt_tune_mod <-
  decision_tree(cost_complexity = tune(),
                tree_depth = tune()) %>%
  set_engine("rpart",model = TRUE) %>%
  set_mode("regression")

tree_wf <- workflow() |>
  add_model(dt_mod) |>
  add_recipe(wq_recipe)

tree_res <- tree_wf |>
  fit(data = wq_data_subset)


tree_res <-
  tree_wf %>%
  tune_grid(
    resamples = wq_folds,
    grid = tree_grid
  )

tree_res |> select_best()
# plot tree
tree_rpart <- tree_res |>
  pull_workflow_fit() |>
  extract_fit_engine()

tree_rpart|>
  rpart.plot::rpart.plot()

tree_rpart |>
  vip::vip()

tree_rpart |> rpart::meanvar()


wq_data_subset_fit <- tree_res |> predict(new_data = wq_data_subset) |> bind_cols(wq_data_subset)
wq_data_subset_fit |> ggplot(aes(x = bacteria_enterococci_100ml, y = .pred)) +
  geom_point() +
  geom_abline() +
  labs(title = "Predicted vs Actual",
       x = "Actual",
       y = "Predicted")

tree_res



