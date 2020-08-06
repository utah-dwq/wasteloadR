#' Permit selector
#'
#' Find and select a WQ permit ID
#' @importFrom wqTools baseMap
#' @importFrom wqTools addMapResetButton
#' @importFrom shinyWidgets pickerInput
#' @import miniUI
#' @import shiny
#' @import leaflet
#' @export
selectPermit=function(){
	permits_coords=wasteloadR::permits_coords
	ui=miniPage(
		gadgetTitleBar("Select a permit"),
		miniContentPanel(
			fluidRow(
			column(6, 
				shinyWidgets::pickerInput('pid_picker', 'Permit ID:', choices=append(" ",paste(permits_coords$permit_id, permits_coords$permit_name)), selected = " ", options = list(`live-search` = TRUE), width="85%")
			),
			column(6, 
				leaflet::leafletOutput("map", height='600px', width="100%")
			)
			)
		)
	)
	
	server=function(input, output, session){
	
		output$map=leaflet::renderLeaflet({
			wqTools::baseMap() %>% wqTools::addMapResetButton()
		})
		
		pid=reactive({gsub(" .*$", "", input$pid_picker)})	
	
	
		observeEvent(pid(), ignoreInit=T, {
			if(pid() !=" "){
				permit_loc=subset(permits_coords, permit_id==pid())
				leaflet::leafletProxy("map") %>% leaflet::clearMarkers() %>% 
					leaflet::addCircleMarkers(data=permit_loc, lat=~LatitudeMeasure, lng=~LongitudeMeasure, options = pathOptions(pane = "markers"),
						label = ~permit_name) %>% 
					leaflet::flyTo(permit_loc$LongitudeMeasure, permit_loc$LatitudeMeasure, zoom=12)
			}
		})
	
		observeEvent(input$done, {
			stopApp(pid())
		})
	}

	runGadget(ui, server)
}

#selectPermit()



