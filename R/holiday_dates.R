#' @title Australian holiday dates
#' @description Dates of Australian public holidays in 2020—2023 by state, from:
#' <https://data.gov.au/dataset/ds-dga-b1bc6077-dadd-4f61-9f8c-002ab2cdff10/details?q=>
#'
#' @param holiday_file `character`
#'
#' @returns
#' @export
#'
#' @examples
#'
#' holiday_dates(holiday_file = tempfile())
#'
holiday_dates <- function(holiday_file = "data/holidays/public_holidays.csv") {

  if (!file.exists(holiday_file)) {
    download_holiday_dates(holiday_file)
  }

  readr::read_csv(
    holiday_file,
    col_types = readr::cols(
      state = readr::col_character(),
      date = readr::col_date(format = ""),
      name = readr::col_character()
    )
  )

}
