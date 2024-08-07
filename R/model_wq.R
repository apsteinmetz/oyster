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

source(here("r/func_pretty_truth_table.R"))
# read in the data -------------------------------------------------------------
wq <- df_from_parquet("data/wq_model_data.parquet") |>
  # remove rows with missing data
  remove_missing() |>
  as_tibble() |>
  mutate(across(where(is.character),as.factor)) |>
  # re-order quality factor levels
  mutate(quality = fct_relevel(quality, "Safe", "Caution", "Unsafe")) |>
  # use our weather, not BOP's
  select(-starts_with("bop"))

# Exploratory Data Analysis -----------------------------------------------------

wq |> pivot_longer(bacteria) |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  labs(title = "Histogram of Bacteria Concentration",
       x = "Bacteria Concentration",
       y = "Frequency")

wq |> pivot_longer(temperature_f) |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  labs(title = "Histogram of Temperature",
       x = "Degrees Fahrenheit",
       y = "Frequency")


wq_adj <- wq |>
  ungroup() |>
  filter(temperature_f > 50)
wq_numeric <- wq_adj |> select(where(is.numeric))

# show a violin plot of bacteria concentration by site
median_all <- median(wq$bacteria)
# show a boxplot of bacteria concentration by site
wq |>
  # avoid Inf log values
  # mutate(bacteria = bacteria + .001) |>
  group_by(site) |>
  nest() |>
  rowwise() |>
  mutate(n_obs = nrow(data)) |>
  filter(n_obs > 100) |>
  mutate(median_bacteria = median(data$bacteria)) |>
  ungroup() |>
  slice_max(median_bacteria,n = 10) |>
  unnest(data) |>
  ggplot(aes(x = reorder(factor(site),median_bacteria), y = bacteria)) +
  scale_y_log10(oob = scales::squish_infinite) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Dirtiest Sites",
       subtitle = "Based on Median Bacteria Count",
       x = "Site",
       y = "Boxplot of Enterococci Concentration\n(Log Scale)") +
  coord_flip() +
  geom_hline(yintercept = SAFE,color="red",linewidth=2) +
  annotate("label", x = 9, y = SAFE, label = "Safe\nLevel", color = "red") +
  theme_minimal()

#best sites
wq |>
  # avoid Inf log values
  # mutate(bacteria = bacteria + .001) |>
  group_by(site) |>
  nest() |>
  rowwise() |>
  mutate(n_obs = nrow(data)) |>
  filter(n_obs > 100) |>
  mutate(median_bacteria = median(data$bacteria)) |>
  ungroup() |>
  slice_min(median_bacteria,n = 10) |>
  unnest(data) |>
  ggplot(aes(x = reorder(factor(site),median_bacteria), y = bacteria)) +
  scale_y_log10(oob = scales::squish_infinite) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Cleanest Sites",
       subtitle = "Based on Median Bacteria Count",
       x = "Site",
       y = "Boxplot of Enterococci Concentration\n(Log Scale)") +
  coord_flip() +
  geom_hline(yintercept = SAFE,color="red",linewidth=2) +
  annotate("label", x = 9, y = SAFE, label = "Safe\nLevel", color = "red") +
  theme_minimal()


# show median bacteria trends over time
wq_plus <- wq |>
  ungroup() |>
  mutate(year = year(date),month = month(date))


rain_axis = 5
wq_plus |>
  filter(year(date) > 2011) |>
  filter(month(date) %in% c(5,6,7,8,9)) |>
  # mutate(bacteria = if_else(bacteria>1500, 1500,bacteria)) |>
  summarise(.by = year, median_bacteria = median(bacteria),
            median_temp = median(temperature_f),
            "total_rainfall" = mean(ghcn_precip_in)*52*rain_axis) |>
  ggplot(aes(x = year, y = median_bacteria)) + geom_col() +
  # geom_smooth(aes(x = year, y = median_bacteria),color = "black",se = FALSE) +
  geom_line(aes(x = year, y = median_temp), color = "red",linewidth =1) +
  geom_line(aes(x = year, y = total_rainfall), color = "blue",linewidth=1) +
  # label the y-axes
  scale_x_continuous(breaks = seq(2000,2024,1)) +
  # put totat_rainfall on secondary y-axis
  scale_y_continuous(sec.axis = sec_axis(~./rain_axis, name = "Total Rainfall (Blue Line")) +
  labs(y = "Median Bacteria Concentration and Temperature (Red Line)",
       title= str_to_title("water is not getting cleaner over time"),
       subtitle = "May-September, 2012-2024")



skim(wq_adj)

# plot histograms of all variables using ggplot
wq_numeric |>
  select(where(is.numeric)) |>
  gather() |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~key, scales = "free")

wq_numeric |>
  cor() |>
  as_tibble(rownames = "variable") |>
  select(variable,bacteria) |>
  gt::gt()

skim(wq_numeric)
# show pairwise correlation plots
wq_numeric |> cor() |> corrplot::corrplot()



# try a classifier model -------------------------------------------------------

wq_rf_prep_cl <- wq |>
  ungroup() |>
  select(-date,-bacteria) |>
  select(-site_id) |>
  # mutate(site = as_factor(site)) |>
  # dimension reduction
  drop_na()

skim(wq_rf_prep_cl)



# get proportion of Unsafe rows grouped by site
temp <- wq_rf_prep_cl |>
  group_by(site) |>
  summarise(Unsafe = sum(quality == "Unsafe")/n()) |>
  arrange(desc(Unsafe))

# show disbribution of bacteria categories
wq_rf_prep_cl |>
  group_by(quality) |>
  summarise(n = n()) |>
  ggplot(aes(x = quality, y = n)) +
  geom_col(fill = "blue") +
  labs(title = "Distribution of Bacteria Categories",
       x = "Bacteria Category",
       y = "Frequency")

wq_recipe_cl <- recipe(quality ~ ., data = wq_rf_prep_cl) |>
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
  conf_mat(truth = quality, estimate = .pred_class)

# plot a ROC curve
wq_pred_cl |>
  roc_curve(truth = quality, .pred_Safe,.pred_Caution,.pred_Unsafe) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity,color=.level)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

# plot prediction confidence
wq_pred_cl |>
  ggplot(aes(x = .pred_Safe, y = .pred_Unsafe, color = quality)) +
  facet_wrap(~quality) +
  geom_point() +
  geom_abline() +
  scale_color_manual(values = c("green", "orange","red")) +
  labs(title = "Prediction Confidence",
       subtitle = "Using Full Data Set - Causing Overfitting",
       x = "Probablility Actual Is Safe",
       y = "Probablility Actual Is Unsafe")



# separate wq_rf_prep_cl into training and testing sets ------------------------
set.seed(123)
wq_split <- initial_split(wq_rf_prep_cl, prop = 0.75, strata = quality)
wq_train <- training(wq_split)
wq_test <- testing(wq_split)

# conform strata of quality in the test set to the training set
skim(wq_train)
skim(wq_test)
ggplot(wq_train,aes(x = quality)) + geom_bar()
ggplot(wq_test,aes(x = quality)) + geom_bar()

# new recipe with just the training data
wq_recipe_cl <- recipe(quality ~ ., data = wq_train) |>
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
       subtitle = 'Random Forest Classifier to Predict "Safe", "Caution" or "Unsafe ') +
  theme_minimal()

wq_pred_cl <- wq_fit_cl |>
  augment(new_data = wq_test)

# plot prediction confidence
wq_pred_cl |>
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
xt <- wq_pred_cl |>
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

##  Fine tune if desired
# rf_grid <- grid_regular(
#   mtry(range = c(3, 5)),
#   min_n(range = c(20, 26)),
#   levels = 5
# )
#rf_grid

# set.seed(456)
# regular_res <- tune_grid(
#   tune_wf,
#   resamples = wq_folds,
#   grid = rf_grid
# )
#
# regular_res
#
# regular_res %>%
#   collect_metrics() %>%
#   filter(.metric == "roc_auc") %>%
#   mutate(min_n = factor(min_n)) %>%
#   ggplot(aes(mtry, mean, color = min_n)) +
#   geom_line(alpha = 0.5, linewidth = 1.5) +
#   geom_point() +
#   labs(y = "AUC")

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
       subtitle = 'Random Forest Classifier to Predict "Safe", "Caution" or "Unsafe ') +
  theme_minimal()

wq_pred_final <- wq_fit_final |>
  augment(new_data = wq_test)

save(wq_pred_final,file=here("data/wq_pred_final.rdata"))


# plot prediction confidence
wq_pred_final |>
  ggplot(aes(x = .pred_Safe, y = .pred_Unsafe, color = quality)) +
  facet_wrap(~quality) +
  geom_point() +
  geom_abline() +
  scale_color_manual(values = c("green", "orange","red")) +
  labs(title = "Prediction Confidence",
       x = "Probablility Actual Is Safe",
       y = "Probablility Actual Is Unsafe")

# show a confusion matrix
xt <- wq_pred_final |>
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
truth_table(xt_count,"count")

wq_pred_final |>
  roc_curve(truth = quality, .pred_Safe,.pred_Caution,.pred_Unsafe) |>
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
  metrics(truth = quality, .pred_Safe,.pred_Caution,.pred_Unsafe,estimate = .pred_class)

# pretty 3d heat map
gg <- xt %>%
  mutate(Truth = factor(Truth, levels = c("Safe", "Caution", "Unsafe"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("Safe", "Caution", "Unsafe"))) %>%
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
