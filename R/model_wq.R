# analyse wq data
library(tidyverse)
library(duckplyr)
library(skimr)
library(tidymodels)
library(vip)
library(gt)


# read in the data -------------------------------------------------------------
wq <- df_from_parquet("data/wq_model_data.parquet") |>
  # remove rows with missing data
  remove_missing() |>
  as_tibble() |>
  group_by(site)

# Exploratory Data Analysis -----------------------------------------------------

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


# show median bacteria trends over time
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

# run a linear regression to predict bacteria concentration -------------------------
wq_adj_prep <- wq_adj |>
  ungroup() |>
  filter(temperature_noaa > 50) |>
  filter(bacteria >0 ) |>
  filter(bacteria < 5000 ) |>
  select(-precip_noaa) |>
  mutate(site = as_factor(site)) |>
  mutate(bacteria = log(bacteria + .01)) |>
  select(-site) |>
  # select(-temperature_noaa) |>
  # select(-precip_wk) |>
  # select(-tide_level) |>
  # select(-hours_since_last) |>
  # select(-current) |>
  drop_na()

wq_lm <- lm(bacteria ~ 0 + ., data = wq_adj_prep)
broom::tidy(wq_lm)
summary(wq_lm)
broom::glance(wq_lm)
crPlots(wq_lm)


wq_lm_pred <-wq_lm |>
  augment()

# plot model output
wq_lm_pred |>
  ggplot(aes(x = bacteria, y= .fitted)) +
  geom_point() +
  geom_abline() +
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

best_model_data |> select(data) |>
  unnest(data) |>
  cor() |>
  corrplot::corrplot()

# try tree-based models --------------------------------------------------------
# tree-based models are better at handling non-linear relationships and
# large outliers

# not needed since we separated the site and into a separate column
# dep_station_names <- read_csv("data/dep_station_names.csv")

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
  separate(site,into = "body",remove = FALSE,extra = "drop") |>
  select(-site) |>
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

# try a classifier model -------------------------------------------------------

wq_rf_prep_cl <- wq |>
  ungroup() |>
  # create a 3 level factor
  mutate(bacteria_category = cut(bacteria, breaks = c(0, 35,104, Inf), labels = c("SAFE", "CAUTION", "UNSAFE"))) |>
  select(-bacteria) |>
  select(-precip_noaa) |>
  select(-precip_wk) |>
  separate(site,into = "body",remove = FALSE,extra = "drop") |>
  # convert all char columns to factors
  mutate_if(is.character,as.factor) |>
  # select(-site) |>
  select(-body) |>
  drop_na()

skim(wq_rf_prep_cl)

wq_recipe_cl <- recipe(bacteria_category ~ ., data = wq_rf_prep_cl) |>
  step_normalize(all_numeric_predictors())

wq_recipe_cl |> prep() |> bake(new_data = NULL) |>
  skim()

wq_rf_model_cl <- rand_forest() |>
  set_engine("ranger",importance = "impurity") |>
  set_mode("classification")

wq_wf <- workflow() |>
  add_model(wq_rf_model_cl) |>
  add_recipe(wq_recipe_cl)

wq_fit_cl <- wq_wf |>
  fit(data = wq_rf_prep_cl)

# show variable importance
wq_fit_cl |>
  pull_workflow_fit() |>
  vip::vip()

wq_pred_cl <- wq_fit_cl |>
  augment(new_data = wq_rf_prep_cl)

# show a confusion matrix
wq_pred_cl |>
  conf_mat(truth = bacteria_category, estimate = .pred_class)

# plot a ROC curve
wq_pred_cl |>
  roc_curve(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity,color=.level)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()



# separate wq_rf_prep_cl into training and testing sets
set.seed(123)
wq_split <- initial_split(wq_rf_prep_cl, prop = 0.75, strata = bacteria_category)
wq_train <- training(wq_split)
wq_test <- testing(wq_split)

# conform strata of bacteria_category in the test set to the training set
skim(wq_train)
skim(wq_test)
ggplot(wq_train,aes(x = bacteria_category)) + geom_bar()
ggplot(wq_test,aes(x = bacteria_category)) + geom_bar()

# new recipe with just the training data
wq_recipe_cl <- recipe(bacteria_category ~ ., data = wq_train) |>
  step_normalize(all_numeric_predictors())

wq_rf_model_cl <- rand_forest() |>
  set_engine("ranger",importance = "impurity") |>
  set_mode("classification")

#wq_rf2_model <- rand_forest() |>
#  # since bacteria_category is a factor, a classifier model is assumed
#  set_engine("randomForest",corr.bias = TRUE)

wq_wf <- workflow() |>
  add_model(wq_rf_model_cl) |>
  add_recipe(wq_recipe_cl)

# fit with training data
wq_fit_cl <- wq_wf |>
  fit(data = wq_train)

# show variable importance
wq_fit_cl |>
  pull_workflow_fit() |>
  vip::vip(aesthetics = list(fill = "blue")) +
  labs(title = "Water Quality Variable Importance",
       subtitle = 'Random Forest Classifier to Predict "SAFE", "CAUTION" or "UNSAFE ') +
  theme_minimal()

wq_pred_cl <- wq_fit_cl |>
  augment(new_data = wq_test)

# plot prediction confidence
wq_pred_cl |>
  ggplot(aes(x = .pred_SAFE, y = .pred_UNSAFE, color = bacteria_category)) +
  geom_point() +
  geom_abline() +
  scale_color_manual(values = c("green", "orange","red")) +
  labs(title = "Prediction Confidence",
       x = "Probablility Actual Is SAFE",
       y = "Probablility Actual Is UNSAFE")

# show a confusion matrix
xt <- wq_pred_cl |>
  conf_mat(truth = bacteria_category, estimate = .pred_class) |>
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
  mutate(Total = sum(c(SAFE,CAUTION,UNSAFE)))

gt_domain <- xt_count |> select(-Total,-Truth) |> max()

xt_prop <- xt |>
  select(-n) |>
  pivot_wider(names_from = Prediction,values_from = .prop) |>
  rowwise() |>
  mutate(Total = sum(c(SAFE,CAUTION,UNSAFE)))

# MAKE A PRETTY TABLE ----------------------------------------------------------

truth_table <- function(xt,type = c("count")){
  gt_xt <- xt |>
  gt(rowname_col = "Truth") |>
  tab_header(title = "Truth Table") |>
  tab_spanner(label = "Prediction", columns = where(is.numeric)) |>
  # add stub header label
  tab_stubhead(label = "Truth")

  if(type == "prop"){
    gt_xt <- gt_xt |> fmt_percent(columns = where(is.numeric),decimals = 0)
  } else {
    gt_xt <- gt_xt |> fmt_number(columns = where(is.numeric),decimals = 0) |>
      grand_summary_rows(
        fns = list(id="Total",label = "Total") ~ sum(.)

      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = list(cells_stub_grand_summary(rows = "Total"))
      )

  }
  # fmt_number(columns = where(is.numeric),decimals = 0) |>
  # fmt_percent(columns = where(is.numeric),decimals = 0) |>
  # color the cells with a heat map
  gt_xt <- gt_xt |>
  data_color(columns = 2:4,
             direction = c("row"),
             domain = c(0,if_else(type == "count",gt_domain,1)),
             method = "numeric",
             palette = "Blues") |>
  # color prediction labels
  tab_style(
    style = list(cell_fill(color = "green"),cell_text(color = "black")),
    locations = list(cells_column_labels("SAFE"),
                     cells_body(column = 1,row = 1))
  ) |>
  tab_style(
    style = list(cell_fill(color = "yellow"),cell_text(color = "blaCk")),
    locations = list(cells_column_labels("CAUTION"),
                     cells_body(column = 1,row = 2))
  ) |>
  tab_style(
    style = list(cell_fill(color = "red"),cell_text(color = "white")),
    locations = list(cells_column_labels("UNSAFE"),
                     cells_body(column = 1,row = 3))
  ) |>
  # color Truth labels
  tab_style(
    style = list(cell_fill(color = "green"),cell_text(color = "black")),
    locations = cells_stub("SAFE")
  ) |>
  tab_style(
    style = list(cell_fill(color = "yellow"),cell_text(color = "black")),
    locations = cells_stub("CAUTION")
  ) |>
  tab_style(
    style = list(cell_fill(color = "red"),cell_text(color = "white")),
    locations = cells_stub("UNSAFE")
  )  |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(cells_body(columns = 1),
                     cells_column_labels(),
                     cells_column_spanners(),
                     cells_title(),
                     cells_stub(),
                     cells_stubhead())
  )
  return(gt_xt)
}

truth_table(xt_prop,"prop")

# plot a ROC curve
wq_pred_cl |>
  roc_curve(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity,color=.level)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

wq_pred_cl |>
  roc_auc(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE)

wq_pred_cl |>
  roc_auc(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE,estimator = "macro_weighted")

wq_pred_cl |>
  metrics(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE,estimate = .pred_class)
