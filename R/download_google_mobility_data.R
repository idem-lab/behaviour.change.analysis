#' @title Download Google COVID-19 Community mobility reports
#' @description Download, formats, writes data to disc, and returns it. Full
#' data set is large, so saves only Australian national and state level data by
#' default.
#'
#' @details Download target is
#' <https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv>
#'
#' @param filename `character`. File name and path to write out data.
#' @param overwrite `logical`. Overwrite if `filename` already exists?
#' @param aus_only `logical`. Write and return only Australian national and
#'   state level data?
#'
#'
#' @source <https://www.google.com/covid19/mobility/index.html>
#'
#' @returns `tbl-df` i.e., tibble.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' download_google_mobility_data()
#' }
#'
download_google_mobility_data <- function(
  filename = "data/mobility/Global_Mobility_Report_AUS.csv",
  overwrite = FALSE,
  aus_only = TRUE
  ){

  warn_and_return_csv(
    filename = filename,
    overwrite= overwrite
  )

  # download archived google mobility data:
  # details https://www.google.com/covid19/mobility/index.html
  url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

  raw_mobility_data <- readr::read_csv(
    url,
    col_types = cols(
      country_region_code = col_character(),
      country_region = col_character(),
      sub_region_1 = col_character(),
      sub_region_2 = col_character(),
      date = col_date(format = "%Y-%m-%d"),
      retail_and_recreation_percent_change_from_baseline = col_double(),
      grocery_and_pharmacy_percent_change_from_baseline = col_double(),
      parks_percent_change_from_baseline = col_double(),
      transit_stations_percent_change_from_baseline = col_double(),
      workplaces_percent_change_from_baseline = col_double(),
      residential_percent_change_from_baseline = col_double(),
      census_fips_code = col_character()
    )
  )

  if(aus_only){
    raw_mobility_data <- raw_mobility_data |>
      dplyr::filter(
        country_region == "Australia" & is.na(sub_region_2)
      )

  }

  readr::write_csv(
    x = raw_mobility_data,
    file = filename
  )

  raw_mobility_data

}
