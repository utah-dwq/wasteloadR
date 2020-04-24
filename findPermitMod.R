# Find permit module

findPermitModUI <- function(id){
	ns <- NS(id)
	tagList(		
		tags$style(type = "text/css", "html, body {width:100%;height:100%}",
			".leaflet .legend i{
			border-radius: 50%;
			width: 10px;
			height: 10px;
			margin-top: 4px;
			}"
		),
		fillRow(
			column(6,
			#fillCol(width="50%", 
				uiOutput(ns("pid_picker")),
				dateRangeInput(ns('ec_date_range'), 'Date range:', end=Sys.Date(), start=Sys.Date()-365*10, width="100%"),
				actionButton(ns('read_ec'), 'Read effluent data', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('cloud-download-alt'))
			),
			fillCol(width="100%", leaflet::leafletOutput(ns("map"), height='100%', width="100%"))
		),
	)
}

findPermitMod <- function(input, output, session){
	
	#permits=readECHO_fac(p_st="ut", p_act="y")	
	#permits$properties$prefix=substr(permits$properties$SourceID, 1, 3)
	#permits=subset(permits, properties$prefix %in% c('UT0','UTL'))
	#permits_coords=do.call(rbind.data.frame,permits$geometry$coordinates)
	#names(permits_coords)=c("dec_long","dec_lat")
	#permits_coords=data.frame(permits$properties[,c("SourceID","CWPName","CWPFacilityTypeIndicator")], (permits_coords))
	#names(permits_coords)[names(permits_coords)=="SourceID"]="locationID"
	#names(permits_coords)[names(permits_coords)=="CWPName"]="locationName"
	#names(permits_coords)[names(permits_coords)=="CWPFacilityTypeIndicator"]="locationType"
	#names(permits_coords)[names(permits_coords)=="dec_long"]="LongitudeMeasure"
	#names(permits_coords)[names(permits_coords)=="dec_lat"]="LatitudeMeasure"
	permits_coords=read.csv(file="permits_coords.csv")
		
	# Permit ID picker input
	output$pid_picker=renderUI({
		ns <- session$ns
		shinyWidgets::pickerInput(ns('pid_picker'), 'Permit ID:', choices=append(" ",paste(permits_coords$locationID, permits_coords$locationName)), selected = " ",
			options = list(`live-search` = TRUE)
		)
	})	

	# Map output
	output$map <- leaflet::renderLeaflet({
		buildMap(plot_polys=F) %>% addMapPane("markers", zIndex = 420)
	})
	
	# Extract permit ID
	pid=reactive({gsub(" .*$", "", input$pid_picker)})

	# Update map with selected permit_id
	observeEvent(pid(), ignoreInit=T, {
		if(pid() !=" "){
			leaflet::leafletProxy("map") %>% leaflet::clearMarkers() %>% 
				leaflet::addCircleMarkers(data=subset(permits_coords, locationID==pid()), lat=~LatitudeMeasure, lng=~LongitudeMeasure) %>%
				leaflet::flyTo(subset(permits_coords, locationID==pid())$LongitudeMeasure, subset(permits_coords, locationID==pid())$LatitudeMeasure, zoom=9)
		}
	})	
	
	# Read EC data
	ec_data=eventReactive(input$read_ec, {
		req(pid(), input$ec_date_range)
			if(pid()!=""){
				show_modal_spinner(spin = "double-bounce", color = "#112446", text = "Querying effluent data...", session = shiny::getDefaultReactiveDomain())
				data=subset(wqTools::readECHO_ec(p_id=pid(), start_date=as.character(format(as.Date(input$ec_date_range)[1], "%m/%d/%Y")), end_date=as.character(format(as.Date(input$ec_date_range)[2], "%m/%d/%Y")),
					print=F, progress=F), !is.na(dmr_value_standard_units))
				levels(data$standard_unit_desc)=append(levels(data$standard_unit_desc), "None")
				data$standard_unit_desc[is.na(data$standard_unit_desc) | data$standard_unit_desc==""]="None"
				data
			}
	})
	
	observeEvent(ec_data(), {
		if(dim(ec_data())[1]==0){
			remove_modal_spinner()
			showModal(modalDialog(easyClose=T, title='No effluent data', 'No effluent data is associated with this permit. Please select another permit'))
		}else{
			remove_modal_spinner()
		}
	})

	return(list(
		pid=pid,
		ec_data=ec_data
	))
}
