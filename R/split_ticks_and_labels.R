#' @title Split ticks and labels
#' @description Independently split frequency of plotting labels and ticks
#'
#'
#' @param dat
#' @param tick_freq
#' @param label_freq
#' @param label_format
#' @param label_shift `logical` sometimes necessary or desirable to switch which
#'   ticks are labelled (e.g. to align with start of a year etc.)
#' @param start_date
#' @param end_date
#'
#' @returns `list` of tick dates, labels, and tick colours
#' @export
#'
#' @examples
#' split_ticks_and_labels(
#' tick_freq = "3 month",
#' label_freq = "6 months",
#' label_format = "%b %y"
#' start_date = as.Date("2020-01-01"),
#' end_date = as.Date("2022-12-31"),
#' )
#'
split_ticks_and_labels <- function(
    # data can be vector of dates or dataframe/tibble with date column
  dat = NULL,
  tick_freq = "1 month",
  label_freq = "2 months",
  label_format = "%b %y",
  label_shift = TRUE,
  start_date,
  end_date
){

  if(!is.null(dat)){
    if(is.tbl(dat)){
      dates <- dat$date
    } else if (is.Date(dat)){
      dates <- dat
    } else {
      stop ("Data must be data frame with `date` column or vector of dates")
    }

    start_date <- min(dates)
    end_date <- max(dates)
  }



  # Create date objects for ticks/labels (e.g., show ticks every n.week.ticks, but label every n.week.labels.panel)
  ticks <- seq.Date(
    from = start_date,
    to = end_date,
    by = tick_freq
  )

  labs_short <- seq.Date(
    from = start_date,
    to = end_date,
    by = label_freq
  ) %>%
    format(label_format) %>%
    as.character

  labs <- ticks %>%
    format(label_format) %>%
    as.character

  label_shift <- ifelse(label_shift, 1, 0)

  labs[!(labs %in% labs_short) - label_shift] <- ""

  tick.cols <- ifelse(labs == "", "grey70", "black")

  return(
    list(
      ticks = ticks,
      labels = labs,
      tick.cols = tick.cols
    )
  )
}
