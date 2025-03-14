#' Abbreviate states
#'
#' @param state_names `character` of state names in full or abbreviated form
#'
#' @returns `character` of state names in abbreviated forms
#' @export
#'
#' @examples
#' abbreviate_states(
#'   c(
#'     "Victoria",
#'     "Tasmania",
#'     "VIC"
#'   )
#' )
#'
abbreviate_states <- function(state_names) {
  dplyr::case_when(
    state_names %in% c("Australian Capital Territory", "ACT") ~ "ACT",
    state_names %in% c("New South Wales", "NSW") ~ "NSW",
    state_names %in% c("Northern Territory", "NT") ~ "NT",
    state_names %in% c("Queensland", "QLD") ~ "QLD",
    state_names %in% c("South Australia", "SA") ~ "SA",
    state_names %in% c("Tasmania", "TAS") ~ "TAS",
    state_names %in% c("Victoria", "VIC") ~ "VIC",
    state_names %in% c("Western Australia", "WA") ~ "WA"
  )
}
