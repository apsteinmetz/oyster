# analyse wq data
library(tidyverse)
library(duckplyr)
library(skimr)
library(tidymodels)
library(yardstick)
library(vip)
library(gt)
library(rayshader)
library(here)
library(car)

source(here("r/pretty_truth_table.R"))
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

# get proportion of UNSAFE rows grouped by site
temp <- wq_rf_prep_cl |>
  group_by(site) |>
  summarise(UNSAFE = sum(bacteria_category == "UNSAFE")/n()) |>
  arrange(desc(UNSAFE))
# show disbribution of bacteria categories
wq_rf_prep_cl |>
  group_by(bacteria_category) |>
  summarise(n = n()) |>
  ggplot(aes(x = bacteria_category, y = n)) +
  geom_col(fill = "blue") +
  labs(title = "Distribution of Bacteria Categories",
       x = "Bacteria Category",
       y = "Frequency")

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
  extract_fit_parsnip() |>
  vip::vip()

wq_pred_cl <- wq_fit_cl |>
  augment(new_data = wq_rf_prep_cl)

save(wq_pred_cl, file = "data/wq_overfit_cl.rdata")

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

# plot prediction confidence
wq_pred_cl |>
  ggplot(aes(x = .pred_SAFE, y = .pred_UNSAFE, color = bacteria_category)) +
  facet_wrap(~bacteria_category) +
  geom_point() +
  geom_abline() +
  scale_color_manual(values = c("green", "orange","red")) +
  labs(title = "Prediction Confidence",
       subtitle = "Using Full Data Set - Causing Overfitting",
       x = "Probablility Actual Is SAFE",
       y = "Probablility Actual Is UNSAFE")



# separate wq_rf_prep_cl into training and testing sets ------------------------
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
  facet_wrap(~bacteria_category) +
  geom_point() +
  geom_abline() +
  scale_color_manual(values = c("green", "orange","red")) +
  labs(title = "Prediction Confidence",
       subtitle = "Using Full Data Set - Causing Overfitting",
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


# Now create a tuned model -----------------------------------------------------

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()) |>
  set_engine("ranger",importance = "impurity") |>
  set_mode("classification")

tune_wf <- workflow() %>%
  add_recipe(wq_recipe_cl) %>%
  add_model(tune_spec)

set.seed(234)
wq_folds <- vfold_cv(wq_train)

doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = wq_folds,
  grid = 20
)

tune_res

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

rf_grid <- grid_regular(
  mtry(range = c(3, 5)),
  min_n(range = c(20, 26)),
  levels = 5
)

rf_grid

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = wq_folds,
  grid = rf_grid
)

regular_res

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.5) +
  geom_point() +
  labs(y = "AUC")

# chose best model
best_auc <- select_best(tune_res, "roc_auc")


final_rf <- finalize_model(
  tune_spec,
  best_auc
)


wq_wf_final <- workflow() |>
  add_model(final_rf) |>
  add_recipe(wq_recipe_cl)

wq_fit_final <- wq_wf_final |>
  fit(data = wq_train)


var_imp <- vip::vi(wq_fit_final)
save(var_imp, file = "data/var_imp.rdata")

# show variable importance
wq_fit_final |>
  extract_fit_parsnip() |>
  vip::vip(aesthetics = list(fill = "blue")) +
  labs(title = "Water Quality Variable Importance",
       subtitle = 'Random Forest Classifier to Predict "SAFE", "CAUTION" or "UNSAFE ') +
  theme_minimal()

wq_pred_final <- wq_fit_final |>
  augment(new_data = wq_test)

# plot prediction confidence
wq_pred_final |>
  ggplot(aes(x = .pred_SAFE, y = .pred_UNSAFE, color = bacteria_category)) +
  facet_wrap(~bacteria_category) +
  geom_point() +
  geom_abline() +
  scale_color_manual(values = c("green", "orange","red")) +
  labs(title = "Prediction Confidence",
       x = "Probablility Actual Is SAFE",
       y = "Probablility Actual Is UNSAFE")

# show a confusion matrix
xt <- wq_pred_final |>
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

truth_table(xt_prop,"prop")
truth_table(xt_count,"count")

wq_pred_final |>
  roc_curve(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity,color=.level)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  labs(title = "Is The Model Better Than Random Chance?",
       subtitle = "ROC Curve for Random Forest Classifier",
       x = "1 - Specificity",
       y = "Sensitivity",
       color = "Prediction") +
  scale_color_manual(values = c("orange", "green","red")) +
  # annotate to label better and worse than chance
  annotate("text",label = "Random Chance",
           x = .5, y = .5,
           color = "black",
           angle = 45, hjust = .5, vjust = 1.5) +
  theme_bw()

wq_pred_final |>
  metrics(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE,estimate = .pred_class)

# TRY LEAVING OUT THE "SITE" INPUT ---------------------------------------------

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
  select(-site) |>
  select(-body) |>
  drop_na()

skim(wq_rf_prep_cl)

# separate wq_rf_prep_cl into training and testing sets ------------------------
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
  facet_wrap(~bacteria_category) +
  geom_point() +
  geom_abline() +
  scale_color_manual(values = c("green", "orange","red")) +
  labs(title = "Prediction Confidence",
       subtitle = "Using Full Data Set - Causing Overfitting",
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

# plot a precision-recall curve
wq_pred_cl |>
  pr_curve(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE) |>
  ggplot(aes(x = recall, y = precision,color=.level)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

# Now create a tuned model -----------------------------------------------------

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()) |>
  set_engine("ranger",importance = "impurity") |>
  set_mode("classification")

tune_wf <- workflow() %>%
  add_recipe(wq_recipe_cl) %>%
  add_model(tune_spec)

set.seed(234)
wq_folds <- vfold_cv(wq_train)

doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = wq_folds,
  grid = 20
)

tune_res

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

# chose tune range optically from plot above
rf_grid <- grid_regular(
  mtry(range = c(1, 2)),
  min_n(range = c(32, 35)),
  levels = 5
)

rf_grid

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = wq_folds,
  grid = rf_grid
)

# unused CV folds
regular_res <- regular_res[1:7,]


regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.5) +
  geom_point() +
  labs(y = "AUC")

# chose best model
best_auc <- select_best(tune_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

wq_wf_final <- workflow() |>
  add_model(final_rf) |>
  add_recipe(wq_recipe_cl)

wq_fit_final <- wq_wf_final |>
  fit(data = wq_train)


var_imp <- vip::vi(wq_fit_final)
save(var_imp, file = "data/var_imp.rdata")

# show variable importance
wq_fit_final |>
  extract_fit_parsnip() |>
  vip::vip(aesthetics = list(fill = "blue")) +
  labs(title = "Water Quality Variable Importance",
       subtitle = 'Random Forest Classifier to Predict "SAFE", "CAUTION" or "UNSAFE ') +
  theme_minimal()

wq_pred_final <- wq_fit_final |>
  augment(new_data = wq_test)

# plot prediction confidence
wq_pred_final |>
  ggplot(aes(x = .pred_SAFE, y = .pred_UNSAFE, color = bacteria_category)) +
  facet_wrap(bacteria_category ~ .) +
  geom_point() +
  geom_abline() +
  scale_color_manual(values = c("green", "orange","red")) +
  labs(title = "Prediction Confidence",
       x = "Probablility Actual Is SAFE",
       y = "Probablility Actual Is UNSAFE")

# show a confusion matrix
xt <- wq_pred_final |>
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

truth_table(xt_prop,"prop")
truth_table(xt_count,"count")

wq_pred_final |>
  roc_curve(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity,color=.level)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  labs(title = "Is The Model Better Than Random Chance?",
       subtitle = "ROC Curve for Random Forest Classifier",
       x = "1 - Specificity",
       y = "Sensitivity",
       color = "Prediction") +
  scale_color_manual(values = c("orange", "green","red")) +
  # annotate to label better and worse than chance
  annotate("text",label = "Random Chance",
           x = .5, y = .5,
           color = "black",
           angle = 45, hjust = .5, vjust = 1.5) +
  theme_bw()

wq_pred_final |>
  metrics(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE,estimate = .pred_class)



# pretty 3d heat map
gg <- xt %>%
  mutate(Truth = factor(Truth, levels = c("SAFE", "CAUTION", "UNSAFE"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("SAFE", "CAUTION", "UNSAFE"))) %>%
  ggplot(aes(Truth,Prediction,fill=.prop)) + geom_tile(color = "black") +
  labs(title = "Water Quality Predictions",
       x = "True Quality Level ",
       y= "Predicted Quality Level",
       caption = "Billion Oyster Project") +
  scale_fill_gradient(low = "lightblue",high = "blue") +
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")


gg

render_confusion <-  function(theta = 45, sunangle = 315) {
    print(sunangle)
    shadow_matrix <-
      plot_gg(
        gg,
        width = 7,
        height = 7,
        multicore = TRUE,
        sunangle = sunangle,
        scale = 200,
        zoom = .75,
        phi = 65,
        theta = theta,
        windowsize = c(800, 800)
      )
    return(shadow_matrix)
  }

orig_shadow = render_confusion(20)
render_snapshot()
render_snapshot(file = "img/3d_confusion.png")
