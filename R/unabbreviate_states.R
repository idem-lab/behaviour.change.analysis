#' Unabbreviate states
#'
#' @param state_names `character` of state names in full or abbreviated form
#'
#' @returns `character` of state names in full
#' @export
#'
#' @examples
#' unabbreviate_states(
#'   c(
#'     "Victoria",
#'     "Tasmania",
#'     "VIC"
#'   )
#' )
#'
unabbreviate_states <- function(state_names) {
  dplyr::case_when(
    state_names %in% c("Australian Capital Territory", "ACT") ~ "Australian Capital Territory",
    state_names %in% c("New South Wales", "NSW") ~ "New South Wales",
    state_names %in% c("Northern Territory", "NT") ~ "Northern Territory",
    state_names %in% c("Queensland", "QLD") ~ "Queensland",
    state_names %in% c("South Australia", "SA") ~ "South Australia",
    state_names %in% c("Tasmania", "TAS") ~ "Tasmania",
    state_names %in% c("Victoria", "VIC") ~ "Victoria",
    state_names %in% c("Western Australia", "WA") ~ "Western Australia"
  )
}
