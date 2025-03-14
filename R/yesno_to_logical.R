yesno_to_logical <- function(x) {
  dplyr::case_when(
    x == "Yes" ~ TRUE,
    x == "No" ~ FALSE,
    TRUE ~ NA
  )
}
