#' @title Save ggplot
#' @details
#' Saves a ggplot into a specified dir with a specified aspect ratio
#'
#'
#' @param filename
#' @param dir
#' @param subdir
#' @param multi
#' @param width
#' @param height
#' @param scale
#' @param dpi
#'
#' @returns
#' @export
#'
#' @examples
save_ggplot <- function (
    filename,
    dir = "outputs",
    subdir = "figures",
    multi = TRUE,
    width = 11.69 / 2,
    height = 8.27 / 3,
    scale = 1,
    dpi = 150
) {

  # david does 8.27 x 11.69 (landscape A4) for 3x2 panels
  # aspect ratio of 0.707:1 h:w
  # want A4 *portrait* width (8.27) with same aspect ratio

  if (multi) {

    # work out dimensions for 4x2 panels for reports
    ratio <- height / width
    mfrow <- c(4, 2)
    width <- height * 3
    height <- (width / mfrow[2]) * ratio * mfrow[1] * 1.2
    scale <- 0.95 * scale

  } else {

    height <- height * 1.25

  }

  path <- file.path(dir, subdir, filename)

  ggplot2::ggsave(
    filename = path,
    scale = scale,
    width = width,
    height = height,
    dpi = dpi,
    bg = 'white'
  )

}
