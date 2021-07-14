#' Run the wasteloadR Dashboard

#' Runs the wasteloadR dashboard app embedded in UT wasteloadR package.

#' @import shiny
#' @import shinyBS
#' @import leaflet
#' @importFrom jsonlite fromJSON
#' @import plotly


#' @export
wasteloadR=function(){
	rmarkdown::run(system.file('dashboard/wasteloadR.Rmd', package='wasteloadR'))
}
