% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_google_mobility_data.R
\name{download_google_mobility_data}
\alias{download_google_mobility_data}
\title{Download Google COVID-19 Community mobility reports}
\source{
\url{https://www.google.com/covid19/mobility/index.html}
}
\usage{
download_google_mobility_data(
  filename = "data/mobility/Global_Mobility_Report_AUS.csv",
  overwrite = FALSE,
  aus_only = TRUE
)
}
\arguments{
\item{filename}{\code{character}. File name and path to write out data.}

\item{overwrite}{\code{logical}. Overwrite if \code{filename} already exists?}

\item{aus_only}{\code{logical}. Write and return only Australian national and
state level data?}
}
\value{
\code{tbl-df} i.e., tibble.
}
\description{
Download, formats, writes data to disc, and returns it. Full
data set is large, so saves only Australian national and state level data by
default.
}
\details{
Download target is
\url{https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv}
}
\examples{

\dontrun{
download_google_mobility_data()
}

}
