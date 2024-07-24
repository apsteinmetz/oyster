# Function to MAKE A PRETTY GT TABLE ----------------------------------------------------------
library(gt)

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
