# get tide data from usharbors.com
  make_tide_year_mo <- function(year,month){
    return(paste0(year,"-",str_pad(month,2,pad="0")))
  }


  make_datetime <- function(date,time){
    return(as_datetime(paste(date,time),format = "%Y-%m-%d %I:%M %p",tz="America/New_York"))
  }

  make_date <- function(year_mo,day){
    return(as.Date(paste(year_mo,day,sep = "-"),format = "%Y-%m-%d"))
  }

  tide_year_mos <- NULL
  for (i in 2020:2024){
    for(j in 1:12){
      tide_year_mos <- c(tide_year_mos,make_tide_year_mo(i,j))
    }
  }
  tide_year_mos



  get_tide_data <- function(tide_year_mo = "2021-04") {
    # tide_year_mo <- make_tide_year_mo(tide_year, tide_mo)
    cat(tide_year_mo)
    tides_html <- rvest::read_html(
      paste0(
        "https://www.usharbors.com/harbor/new-york/new-york-the-battery-ny/tides/?tide=",
        tide_year_mo,
        "#monthly-tide-chart"
      )
    )

    #extract monthly tide chart
    tides <- tides_html %>%
      rvest::html_nodes(xpath = '//*[@id="monthly-tide-chart"]') %>%
      rvest::html_table() |>
      pluck(1)
    if (length(tides) == 0) {
      cat(" No Data\n")
      return(NULL)
    }

    cat("\n")
    tides <- tides  |>
      janitor::clean_names() |>
      select(1:10) |>
      # remove first row with units
      slice(-1) |>
      set_names(
        c(
          "daynum",
          "day_of_week",
          "high_tide_am_time",
          "high_tide_am_ft",
          "high_tide_pm_time",
          "high_tide_pm_ft",
          "low_tide_am_time",
          "low_tide_am_ft",
          "low_tide_pm_time",
          "low_tide_pm_ft"
        )
      ) |>
      mutate(date = make_date(tide_year_mo, daynum),
             .before = "daynum") |>
      filter(!is.na(date)) |>
      mutate(across(contains("ft"), as.numeric)) |>
      mutate(across(contains("am_time"),  ~ paste(.x, "AM"))) |>
      mutate(across(contains("pm_time"),  ~ paste(.x, "PM"))) |>
      mutate(across(contains("time"),  ~ make_datetime(date, .x))) |>
      select(-daynum, -day_of_week)
    return(tides)
  }



  tides <- map(tide_year_mos,get_tide_data) |> bind_rows()

  save(tides,file="data/tides.rdata")
