split_ticks_and_labels <- function(
    # data can be vector of dates or dataframe/tibble with date column
  dat = NULL,
  tick_freq = "1 month",
  label_freq = "2 months",
  label_format = "%b %y",
  label_last = TRUE,
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

  label_shift <- ifelse(label_last, 1, 0)

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
