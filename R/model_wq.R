# analyse wq data
library(tidyverse)
library(duckplyr)
library(skimr)
library(tidymodels)
library(vip)


# read in the data
wq <- df_from_parquet("data/wq_model_data.parquet") |>
  # remove rows with missing data
  remove_missing() |>
  as_tibble() |>
  group_by(site)

wq |> pivot_longer(bacteria) |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  labs(title = "Histogram of Bacteria Concentration",
       x = "Bacteria Concentration",
       y = "Frequency")

wq |> pivot_longer(temperature_noaa) |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  labs(title = "Histogram of Temperature",
       x = "Degrees Fahrenheit",
       y = "Frequency")


wq_adj <- wq |>
  ungroup() |>
  filter(temperature_noaa > 50)

# show a violin plot of bacteria concentration by site
wq_adj |>
  group_by(site) |>
  nest() |>
  ungroup() |>
  mutate(n_obs = map_int(data, nrow)) |>
  # get median of bacteria concentrations for each site
  mutate(median_bacteria = map_dbl(data, ~median(.x$bacteria))) |>
  filter(n_obs > 100) |>
  slice_max(median_bacteria,n = 10)|>
  unnest(data) |>
  ggplot(aes(x = factor(site), y = bacteria)) +
  geom_violin(draw_quantiles = .5) +
  geom_jitter(width = .1) +
  labs(title = "Worst Sites for Bacteria Count",
       x = "Site",
       y = "Log Bacteria Concentration") +
  coord_flip()

#best sites
wq_adj |>
  group_by(site) |>
  nest() |>
  ungroup() |>
  mutate(n_obs = map_int(data, nrow)) |>
  # get mean of bacteria concentrations for each site
  mutate(median_bacteria = map_dbl(data, ~median(.x$bacteria))) |>
  filter(n_obs > 100) |>
  slice_min(median_bacteria,n = 10)|>
  unnest(data) |>
  ggplot(aes(x = factor(site), y = bacteria)) +
  geom_violin(draw_quantiles = .5) +
  geom_jitter(width = .1) +
  labs(title = "Cleanest Sites by Bacteria Count",
       x = "Site",
       y = "Log Bacteria Concentration") +
  coord_flip()


# show median bacteria trends over time ----------------------------------------
wq_data_4 <- df_from_parquet("data/wq_data_4.parquet") |>
  as_tibble()


wq_data_4 |>
  filter(year > 2011) |>
  group_by(month) |>
  summarise(median_bacteria = median(bacteria)) |>
  ggplot(aes(x = month, y = median_bacteria)) + geom_col()

rain_axis = 2.5
wq_data_4 |>
  filter(year(date) > 2011) |>
  filter(month(date) %in% c(5,6,7,8,9)) |>
  # mutate(bacteria = if_else(bacteria>1500, 1500,bacteria)) |>
  summarise(.by = year, median_bacteria = median(bacteria),
            median_temp = median(temperature_noaa),
            "total_rainfall" = mean(precip_wk)*20*rain_axis) |>
  ggplot(aes(x = year, y = median_bacteria)) + geom_col() +
  # geom_smooth(aes(x = year, y = median_bacteria),color = "black",se = FALSE) +
  geom_line(aes(x = year, y = median_temp), color = "red") +
  geom_line(aes(x = year, y = total_rainfall), color = "blue") +
  # label the y-axes
  scale_x_continuous(breaks = seq(2000,2024,1)) +
  # put totat_rainfall on secondary y-axis
  scale_y_continuous(sec.axis = sec_axis(~./rain_axis, name = "Total Rainfall (Blue Line")) +
  labs(y = "Median Bacteria Concentration and Temperature (Red Line)",
       title= str_to_title("water is not getting cleaner over time"),
       subtitle = "May-September, 2012-2024")



glimpse(wq_adj)
skim(wq_adj)

# plot histograms of all variables using ggplot
wq_adj |>
  ungroup() |>
  select(-site) |>
  gather() |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~key, scales = "free")

# show pairwise correlation plots


wq_adj |> select(-site) |>
  gather() |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~key, scales = "free")

wq_adj |> select(-site) |>
  cor() |>
  as_tibble(rownames = "variable") |>
  select(variable,bacteria) |>
  gt::gt()

skim(wq_adj)
# show pairwise correlation plots
wq_adj |> select(-site) |> cor() |> corrplot::corrplot()

wq_adj_prep <- wq_adj |>
  ungroup() |>
  filter(temperature_noaa > 50) |>
  filter(bacteria >0 ) |>
  filter(bacteria < 5000 ) |>
  select(-site) |>
  select(-precip_noaa) |>
  # mutate(site = as_factor(site)) |>
  mutate(bacteria = log(bacteria + .01)) |>
  drop_na()

wq_lm <- lm(bacteria ~ ., data = wq_adj_prep)
broom::tidy(wq_lm)
summary(wq_lm)
broom::glance(wq_lm)

wq_lm_pred <-wq_lm |>
  predict() |>
  enframe(name = NULL, value = ".pred") |>
  bind_cols(wq_adj_prep) |>
  select(bacteria,.pred,everything())

# plot model output
wq_lm_pred |>
  ggplot(aes(x = bacteria, y= .pred)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# model each station separately
wq_adj_lm_indiv <- wq_adj |>
  group_by(site) |>
  nest() |>
  mutate(model = map(data, ~lm(bacteria ~ ., data = .))) |>
  mutate(tidy = map(model, broom::glance)) |>
  unnest(tidy) |>
  # remove rows with NaN
  filter(!r.squared == 1.0) |>
  filter(nobs > 100)


wq_adj_lm_indiv |>
  # filter(r.squared  > .1) |>
  filter(nobs > 100) |>
  ggplot(aes(site,r.squared)) +
  geom_col() +
  # do not show x-axis labels
  theme(axis.text.x = element_blank())

best_model_data <- wq_adj_lm_indiv |>
  ungroup() |>
  filter(nobs > 100) |>
  slice_max(r.squared, n = 1)
  # filter(r.squared  > .24)

best_model_data |> select(data) |>
  unnest(data) |>
  cor() |>
  corrplot::corrplot()

# try tree-based models --------------------------------------------------------
# tree-based models are better at handling non-linear relationships and
# large outliers

set.seed(123)
wq_rf_prep <- wq |>
  ungroup() |>
  filter(temperature_noaa > 50) |>
  filter(bacteria >0 ) |>
  filter(bacteria < 5000 ) |>
  # select(-site) |>
  select(-precip_noaa) |>
  # mutate(site = as_factor(site)) |>
  mutate(bacteria = log(bacteria + .01)) |>
  # dimension reduction
  separate(site,into = "body",remove = FALSE,extra = "drop")
      drop_na()

skim(wq_rf_prep)

wq_recipe <- recipe(bacteria ~ ., data = wq_rf_prep) |>
  step_log(precip_wk,offset = .01) |>
  step_normalize(all_numeric_predictors())
# wq_adj_prep <- wq_recipe |> bake(new_data = NULL)

# wq_adj_prep |> skim()

wq_rf_model <- rand_forest() |>
  set_engine("ranger",importance = "impurity") |>
  set_mode("regression")

wq_rf2_model <- rand_forest() |>
  set_engine("randomForest",corr.bias = TRUE)  |>
  set_mode("regression")

wq_wf <- workflow() |>
  add_model(wq_rf2_model) |>
  add_recipe(wq_recipe)

wq_fit <- wq_wf |>
  fit(data = wq_rf_prep)

# show variable importance
wq_fit |>
  pull_workflow_fit() |>
  vip::vip()

wq_adj_pred <- wq_fit |>
  predict(new_data = wq_rf_prep) |>
  bind_cols(wq_rf_prep) |>
  select(bacteria,.pred,everything())

# dummy adjustment
# wq_adj_pred <- wq_adj_pred |>
#  mutate(.pred = .pred*2-5)


wq_pred <- wq_adj_pred |>
  mutate(across(c(1,2),exp))


wq_adj_pred |>
  metrics(truth = bacteria, estimate = .pred)
wq_pred |>
  metrics(truth = bacteria, estimate = .pred)

# plot truth vs predicted
wq_adj_pred |>
  # filter(.pred - bacteria > -5000) |>
  ggplot(aes(x = bacteria, y = .pred)) +
  geom_point() +
  geom_abline() +
  labs(title = "Measured vs Predicted Bacteria Concentration",
       x = "Measured",
       y = "Predicted")

# plot truth vs predicted
wq_pred |>
  filter(.pred < 10000) |>
  ggplot(aes(x = bacteria, y = .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth() +
  labs(title = "Measured vs Predicted Bacteria Concentration",
       x = "Measured",
       y = "Predicted")

# plot residuals
wq_adj_pred |>
  # filter(.pred - bacteria > -5000) |>
  ggplot(aes(x = bacteria, y = .pred - bacteria)) +
  geom_point() +
  # geom_hline(yintercept = -10000) +
  labs(title = "Measured Bacteria Concentration vs Model Residuals",
       x = "Measured",
       y = "Residual")

wq_pred |>
  filter(.pred - bacteria < 5000) |>
  ggplot(aes(x = .pred - bacteria)) +
  geom_histogram() +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency")

# use rpart
