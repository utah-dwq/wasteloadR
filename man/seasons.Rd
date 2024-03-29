% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seasons.R
\name{seasons}
\alias{seasons}
\title{Generate low flow statistics (nQy) for wasteloads}
\usage{
seasons(
  dates,
  seasons = list(winter = c(1, 2, 3), spring = c(4, 5, 6), summer = c(7, 8, 9), fall =
    c(10, 11, 12))
)
}
\arguments{
\item{dates}{Input date vector. Must of class "Date".}

\item{seasons}{Named list of seasons.}
}
\value{
Returns a vector of season names associated with each date.
}
\description{
Generate low flow statistics (nQy) for wasteloads
}
\examples{
dates=as.Date(c("2022-08-29","2022-09-30","2022-10-01","2022-10-02","2022-10-03"))
seasons(dates)
}
