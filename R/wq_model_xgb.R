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


wq_prep <- wq |>
  ungroup() |>
  # create a 3 level factor
  mutate(bacteria_category = cut(bacteria, breaks = c(0, 35,104, Inf), labels = c("SAFE", "CAUTION", "UNSAFE"))) |>
  select(-bacteria) |>
  select(-precip_noaa) |>
  select(-precip_wk) |>
  separate(site,into = "body",remove = FALSE,extra = "drop") |>
  # convert all char columns to factors
  mutate_if(is.character,as.factor) |>
  # xgboost can't handle categorical data
  select(-site) |>
  select(-body) |>
  drop_na()

skim(wq_prep)

# separate wq_rf_prep_cl into training and testing sets ------------------------
set.seed(123)
wq_split <- initial_split(wq_prep, strata = bacteria_category)
wq_train <- training(wq_split)
wq_test <- testing(wq_split)

# conform strata of bacteria_category in the test set to the training set
skim(wq_train)
skim(wq_test)
ggplot(wq_train,aes(x = bacteria_category)) + geom_bar()
ggplot(wq_test,aes(x = bacteria_category)) + geom_bar()

# new recipe with just the training data
wq_recipe <- recipe(bacteria_category ~ ., data = wq_train)

wq_xgb_model <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification")

wq_xgb_model

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), wq_train),
  learn_rate(),
  size = 30
)

xgb_grid

wq_wf <- workflow() |>
  add_model(wq_xgb_model) |>
  add_recipe(wq_recipe)

wq_fit <- wq_wf |>
  fit(data = wq_train)

wq_pred <- augment(wq_fit,new_data = wq_test)

# show variable importance
wq_fit |>
  pull_workflow_fit() |>
  vip::vip(aesthetics = list(fill = "blue")) +
  labs(title = "Water Quality Variable Importance",
       subtitle = 'Random Forest Classifier to Predict "SAFE", "CAUTION" or "UNSAFE ') +
  theme_minimal()

# plot prediction confidence
wq_pred |>
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
xt <- wq_pred |>
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


truth_table(xt_count,"count")

# plot a ROC curve
wq_pred |>
  roc_curve(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity,color=.level)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

wq_pred |>
  roc_auc(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE)

wq_pred |>
  roc_auc(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE,estimator = "macro_weighted")

wq_pred |>
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

# tune model -------------------------------------------------------------------

wq_xgb_model_tuneable <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

wq_wf <- workflow() |>
  add_model(wq_xgb_model_tuneable) |>
  add_recipe(wq_recipe)


wq_wf

set.seed(123)
wq_folds <- vfold_cv(wq_train, strata = bacteria_category)

wq_folds
doParallel::registerDoParallel()

set.seed(234)
xgb_res <- tune_grid(
  wq_wf,
  resamples = wq_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)


xgb_res

collect_metrics(xgb_res)

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

show_best(xgb_res, "roc_auc")

best_auc <- select_best(xgb_res, "roc_auc")
best_auc

final_xgb <- finalize_workflow(
  wq_wf,
  best_auc
)

final_xgb

final_fit <- final_xgb %>%
  fit(data = wq_train)

final_fit %>%
  pull_workflow_fit() %>%
  vip()

final_pred <- final_fit %>%
  augment(wq_test)

# plot prediction confidence
final_pred |>
  ggplot(aes(x = .pred_SAFE, y = .pred_UNSAFE, color = bacteria_category)) +
  facet_wrap(~bacteria_category) +
  geom_point() +
  geom_abline() +
  scale_color_manual(values = c("green", "orange","red")) +
  labs(title = "Prediction Confidence",
       subtitle = "Using Full Data Set - Causing Overfitting",
       x = "Probablility Actual Is SAFE",
       y = "Probablility Actual Is UNSAFE")


xt <- final_pred |>
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


truth_table(xt_count,"count")

# plot a ROC curve
wq_pred |>
  roc_curve(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity,color=.level)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

wq_pred |>
  roc_auc(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE)

wq_pred |>
  roc_auc(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE,estimator = "macro_weighted")

wq_pred |>
  metrics(truth = bacteria_category, .pred_SAFE,.pred_CAUTION,.pred_UNSAFE,estimate = .pred_class)
