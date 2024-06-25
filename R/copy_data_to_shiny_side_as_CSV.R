# copy data files to shiny app www directory as csv
library(tidyverse)
library(arrow)
datasets <- c("wq_data", "wq_meta", "weather")

copy_to_shiny <- function(name) {
  dframe <- arrow::read_parquet(paste0("data/",name,".parquet"))
  # dframe = eval(parse(text = name))
  fname = paste0("NY_Harbor_WQ/www/", name, ".csv")
  arrow::write_csv_arrow(dframe, fname)
}

walk(datasets, copy_to_shiny)
