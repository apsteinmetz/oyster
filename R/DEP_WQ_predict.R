# predict water quality with NY DEP data and NOAA weather data
library(tidyverse)
library(here)
library(googlesheets4)
library(rvest)
library(duckplyr)
library(leaflet)
library(htmltools)
library(sf)
library(tidymodels)
library(arrow)
library(gt)


# enterococci thresholds
SAFE_E = 34
CAUTION_E = 104

# fecal coliform thresholds
SAFE_C = 200
CAUTION_C = 1000

# DATA PREP ---------------------------------------------------
# read the data
wq_data <- read_parquet(here("data", "wq_data_dep_subset.parquet")) |>
  mutate(.before = sample_datetime,date = date(sample_datetime))
#save parquet  preciptation data
nyc_precip <- read_parquet(here("data", "precip_nyc.parquet"))
# add column to show  rainfall from previous two days
nyc_precip <- nyc_precip %>%
  arrange(date) %>%
  mutate(rain_2d = lag(PRCP, 2)+ lag(PRCP, 1)) |>
  filter(!is.na(rain_2d))

# add rain features
wq_data <- wq_data %>%
  left_join(nyc_precip, by = "date")
# add seasonality feature where tags are relative warmth of typical month
# so we can treat seasonality as a continuous variable
wq_data <- wq_data %>%
  # mutate(month = factor(month(date), levels = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
  mutate(season = as.numeric(factor(month(date), levels = 1:12, labels = c(0,1,2,3,4,5,6,5,4,3,2,1))))
 # mutate character columns to factor
wq_data <- wq_data %>%
  mutate(across(where(is.character), as.factor))

wq_data <- wq_data |> mutate(quality_fecal = as_factor(cut(
  bacteria_fecal_coliform_100ml,
  breaks = c(-1,SAFE_C, CAUTION_C, Inf),
  labels = c("Safe", "Caution", "Unsafe"))))

wq_data <- wq_data |> mutate(quality_cocci = as_factor(cut(
  bacteria_enterococci_100ml,
  breaks = c(-1,SAFE_E, CAUTION_E, Inf),
  labels = c("Safe", "Caution", "Unsafe"))))

skimr::skim(wq_data)

wq_subset <- wq_data %>%
  drop_na()

# EDA ---------------------------------------------------
# plot the data
wq_data %>%
  ggplot(aes(x = sample_datetime,
             y = bacteria_fecal_coliform_100ml,
             color = quality_fecal)) +
  geom_point() +
  labs(title = "Fecal Coliform Bacteria",
       subtitle = "NYC DEP Water Quality Data",
       x = "Date",
       y = "Fecal Coliform Bacteria (100ml)",
       color = "Quality") +
  scale_color_manual(values = c("green", "orange", "red")) +
  theme_minimal()

wq_data %>%
  ggplot(aes(x = sample_datetime,
             y = bacteria_enterococci_100ml,
             color = quality_cocci)) +
  geom_point() +
  labs(title = "Enterococci Bacteria",
       subtitle = "NYC DEP Water Quality Data",
       x = "Date",
       y = "Enterococci Bacteria (100ml)",
       color = "Quality") +
  scale_color_manual(values = c("green", "orange", "red")) +
  theme_minimal()

wq_subset %>%
  ggplot(aes(x = bacteria_fecal_coliform_100ml,
             y = bacteria_enterococci_100ml,
             color = quality_fecal)) +
  geom_point() +
  labs(title = "Fecal Coliform Bacteria",
       subtitle = "NYC DEP Water Quality Data",
       x = "fecal coliform",
       y = "enterococci",
       color = "Quality") +
  scale_color_manual(values = c("green", "orange", "red")) +
  scale_x_log10() +
  scale_y_log10() +
  geom_vline(xintercept = SAFE_C) +
  geom_vline(xintercept = CAUTION_C) +
  geom_hline(yintercept = SAFE_E) +
  geom_hline(yintercept = CAUTION_E) +
  theme_minimal()

# build the model ---------------------------------------------------

# split data into training and testing sets
# predict enterococci quality level
wq_data_for_model <- wq_subset %>%
  select(-bacteria_fecal_coliform_100ml,
         -bacteria_enterococci_100ml,
         -quality_fecal,
         -sample_datetime,
         -date)
# set.seed(123)
wq_split <- initial_split(wq_data_for_model, prop = 0.75, strata = quality_cocci)
wq_train <- training(wq_split)
wq_test <- testing(wq_split)

# create a recipe
wq_recipe <- recipe(quality_cocci ~ ., data = wq_train) |>
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

# plot prediction confidence
wq_pred <- wq_fit |>
  augment(new_data = wq_test)

wq_pred |>
  ggplot(aes(x = .pred_Safe, y = .pred_Unsafe, color = quality_cocci)) +
  facet_wrap(~quality_cocci) +
  geom_point() +
  geom_abline() +
  scale_color_manual(values = c("green", "orange","red")) +
  labs(title = "Prediction Confidence",
       subtitle = "Many predictions are Confidently Wrong",
       x = "Probablility Actual Is Safe",
       y = "Probablility Actual Is Unsafe")
# show a confusion matrix
xt <- wq_pred |>
  conf_mat(truth = quality_cocci, estimate = .pred_class) |>
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
      locations = list(cells_column_labels("Safe"),
                       cells_body(column = 1,row = 1))
    ) |>
    tab_style(
      style = list(cell_fill(color = "yellow"),cell_text(color = "blaCk")),
      locations = list(cells_column_labels("Caution"),
                       cells_body(column = 1,row = 2))
    ) |>
    tab_style(
      style = list(cell_fill(color = "red"),cell_text(color = "white")),
      locations = list(cells_column_labels("Unsafe"),
                       cells_body(column = 1,row = 3))
    ) |>
    # color Truth labels
    tab_style(
      style = list(cell_fill(color = "green"),cell_text(color = "black")),
      locations = cells_stub("Safe")
    ) |>
    tab_style(
      style = list(cell_fill(color = "yellow"),cell_text(color = "black")),
      locations = cells_stub("Caution")
    ) |>
    tab_style(
      style = list(cell_fill(color = "red"),cell_text(color = "white")),
      locations = cells_stub("Unsafe")
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
wq_pred |>
  roc_curve(truth = quality_cocci, .pred_Safe,.pred_Caution,.pred_Unsafe) |>
  mutate(.level = as_factor(.level)) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity,color=.level)) +
  scale_color_manual(values = c("green", "orange","red")) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

wq_pred |>
  roc_auc(truth = quality_cocci, .pred_Safe,.pred_Caution,.pred_Unsafe)
wq_pred |>
  roc_auc(truth = quality_cocci, .pred_Safe,.pred_Caution,.pred_Unsafe,estimator = "macro_weighted")
wq_pred |>
  metrics(truth = quality_cocci, .pred_Safe,.pred_Caution,.pred_Unsafe,estimate = .pred_class)
wq_subset |> ggplot(aes(quality_cocci,rain_2d)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal()

