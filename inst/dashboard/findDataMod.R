# Find data module


findDataModUI <- function(id){
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
		#add_busy_spinner(spin = "fading-circle", position="full-page", timeout=200),
		fillRow(
			#column(6,
			wellPanel(style = "overflow-y:scroll; max-height: 100%; min-height: 100%;",
			fillCol(width="95%", 
				shinyBS::bsCollapse(multiple=F, open=1,
					
					## Find permit
					shinyBS::bsCollapsePanel(list(icon('plus-circle'),"Find effluent data"), value=1,
						uiOutput(ns("permit_ui")), 
						fluidRow(
							column(6, actionButton(ns('read_ec'), 'Read effluent data', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('file-import'), width="100%")),
							column(6, uiOutput(ns("dwnloadec_button")))
						)
					),
					
					## Find WQ sites
					shinyBS::bsCollapsePanel(list(icon('plus-circle'),"Find water quality sites"), value=2,
						uiOutput(ns('query_sites')),
						br(),
						fluidRow(
							column(6, uiOutput(ns('site_types'))),
							column(6, uiOutput(ns('orgs')))
						),
						fluidRow(
							column(6, uiOutput(ns('visit_count_slider')))
						)
						
						# ? Select sites for download (map click  & multiInput)
						# Query WQ data actionButton
					),
					shinyBS::bsCollapsePanel(list(icon('plus-circle'), "Query water quality data"), value=3,
						fluidRow(column(12, uiOutput(ns("sel_sites_input")))),
						fluidRow(column(6, uiOutput(ns("readwq")), uiOutput(ns("dwnloadwq_button"))))
					)
				)
			)),
			fillCol(width="95%", leaflet::leafletOutput(ns("map"), height='100%', width="100%"))
		)
	)
}

findDataMod <- function(input, output, session, permits_coords){

	robs=reactiveValues()
	
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

	## Map output
	output$map <- leaflet::renderLeaflet({
		show_modal_spinner(spin = "double-bounce", color = "#112446", text = "Initializing...", session = shiny::getDefaultReactiveDomain())	
		map=buildMap(plot_polys=T) %>% addMapPane("markers", zIndex = 420) %>% addMapPane("sites", zIndex = 419) %>% addMapPane("highlight", zIndex = 418)
		#remove_modal_spinner()
		map
	})

	
	# Find permit
	## Permit ID picker input
	output$permit_ui=renderUI({
		ns <- session$ns
		tagList(
			shinyWidgets::pickerInput(ns('pid_picker'), 'Permit ID:', choices=append(" ",paste(permits_coords$locationID, permits_coords$locationName)), selected = " ",
				options = list(`live-search` = TRUE)
			),
			dateRangeInput(ns('ec_date_range'), 'Date range:', end=Sys.Date(), start=Sys.Date()-365*10, width="100%")
		)
	})	

	
	## Extract permit ID
	pid=reactive({gsub(" .*$", "", input$pid_picker)})	
	## Update map with selected permit_id
	observeEvent(pid(), ignoreInit=T, {
		if(pid() !=" "){
			permit_loc=subset(permits_coords, locationID==pid())
			leaflet::leafletProxy("map") %>% leaflet::clearMarkers() %>% 
				leaflet::addCircleMarkers(data=permit_loc, lat=~LatitudeMeasure, lng=~LongitudeMeasure, options = pathOptions(pane = "markers"),
					label = HTML(permit_loc$lab)) %>% 
				leaflet::flyTo(permit_loc$LongitudeMeasure, permit_loc$LatitudeMeasure, zoom=12)
		}
		remove_modal_spinner()
	})	
	
	## Read EC data
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

	### Download EC data actionButton
	output$dwnloadec_button=renderUI({
		req(ec_data())
		ns <- session$ns
		downloadButton(ns('dwnloadec'), 'Download effluent data', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', style = "width:100%;")
	})


	### Download EC data
	output$dwnloadec <- downloadHandler(
		filename=paste0('effluent-export-', Sys.Date(),'.xlsx'),
		content = function(file) {writexl::write_xlsx(
			list(
				'effluent-data'=ec_data()
			),
			path = file, format_headers=F, col_names=T)}
	)



	# Find water quality data
	## Query sites UI
	### Button
	output$query_sites=renderUI({
		req(pid())
		ns <- session$ns
		actionButton(ns('query_sites'), 'Query sites in view area', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('cloud-download-alt'))
	})
	
	### Site types
	output$site_types=renderUI({
		req(robs$sites)
		ns <- session$ns
		types=unique(robs$sites$MonitoringLocationTypeName)
		types=types[order(types)]
		shinyWidgets::multiInput(ns('site_types'), 'Site types:', choices=types, selected=types, width="100%")	
	})
		
	### Organizations
	output$orgs=renderUI({
		req(robs$sites)
		ns <- session$ns
		orgs=unique(robs$sites$OrganizationIdentifier)
		orgs=orgs[order(orgs)]
		shinyWidgets::multiInput(ns('orgs'), "Organizations:", choices=orgs,
			selected=c("UTAHDWQ_WQX"), width="100%")	
	})

	### Visit count slider
	output$visit_count_slider=renderUI({
		req(robs$visit_counts)
		ns <- session$ns
		min_visit=min(robs$visit_counts$count)
		max_visit=max(robs$visit_counts$count)
		sliderInput(ns("visit_count_slider"), "Min visit count:", min=min_visit, max=max_visit, value=min_visit)	
	})	
	
	
	## Query sites, activities, get visit counts
	observeEvent(input$query_sites, ignoreInit=T, {
		req(input$map_bounds)
		show_modal_spinner(spin = "double-bounce", color = "#112446", text = "Querying...", session = shiny::getDefaultReactiveDomain())	
		map_box=input$map_bounds
		bbox=paste(map_box[4], map_box[3], map_box[2], map_box[1], sep='%2C')
		robs$sites=wqTools::readWQP(type='sites', bBox=bbox, siteType=c("Lake, Reservoir, Impoundment","Stream","Spring","Facility"))
		act=wqTools::readWQP(type='activity', bBox=bbox, siteType=c("Lake, Reservoir, Impoundment","Stream","Spring","Facility"))
		robs$activities=subset(act, MonitoringLocationIdentifier %in% robs$sites$MonitoringLocationIdentifier)
		visits=unique(robs$activities[,c("MonitoringLocationIdentifier","ActivityStartDate")])
		visit_counts=aggregate(ActivityStartDate~MonitoringLocationIdentifier, visits, FUN='length')
		names(visit_counts)[names(visit_counts)=='ActivityStartDate']="count"
		robs$visit_counts=visit_counts
		sites_counts=merge(robs$sites, robs$visit_counts)
		robs$sites_counts=within(sites_counts, {
			lab=paste0(
				'<p>',
				"Organization: ", OrganizationIdentifier,
				'<br />', "MLID: ", MonitoringLocationIdentifier,
				'<br />', "ML name: ", MonitoringLocationName,
				'<br />', "Site type: ", MonitoringLocationTypeName,
				'<br />', "Visit count: ", count)
		})		
		remove_modal_spinner()
	})
		
	## Select sites from UI inputs
	map_sites=reactive({
		req(input$visit_count_slider, input$orgs, input$site_types, robs$sites_counts)
		subset(robs$sites_counts, count>=input$visit_count_slider & OrganizationIdentifier %in% input$orgs & MonitoringLocationTypeName %in% input$site_types)
	})
		
	### Update map
	observeEvent(map_sites(), {
		req(map_sites())
		leafletProxy("map") %>% clearGroup("sites") %>%
			leaflet::addCircleMarkers(data=map_sites(), lat=~LatitudeMeasure, lng=~LongitudeMeasure, options = pathOptions(pane = "sites"), group="sites",
				color = 'purple', opacity=0.8, layerId=~MonitoringLocationIdentifier,
				label = lapply(map_sites()$lab, HTML) # crashes if only one site is selected for lapply
			)
	})

	## Query WQ data UI
	### Select sites for WQ data query
	output$sel_sites_input=renderUI({
		req(map_sites())
		ns <- session$ns
		shinyWidgets::multiInput(ns("sel_sites_input"), "Select sites (or map click):", choices=map_sites()$MonitoringLocationIdentifier, selected=robs$sel_sites, width="100%")	
	})

	### Select sites on map click
	observeEvent(input$map_marker_click, {
		mlid = input$map_marker_click$id
		if(!is.null(mlid)){
			if(mlid %in% robs$sel_sites){
				robs$sel_sites=robs$sel_sites[!robs$sel_sites %in% mlid]
			}else{
				robs$sel_sites=append(robs$sel_sites, mlid)
			}
		}
	})
	
	### Update map site highlight
	observeEvent(robs$sel_sites, ignoreNULL = F, ignoreInit=T, {
		leafletProxy("map") %>%
			clearGroup(group='highlight') %>%
			addCircleMarkers(data=subset(map_sites(), MonitoringLocationIdentifier %in% robs$sel_sites), lat=~LatitudeMeasure, lng=~LongitudeMeasure,
				group='highlight', options = pathOptions(pane = "highlight"), radius = 20, color='chartreuse', opacity = 0.75, fillOpacity = 0.4)
	})

	### Update sel_sites w/ multiInput
	observeEvent(input$sel_sites_input, ignoreNULL=F, {
		robs$sel_sites=input$sel_sites_input
	})
	
	### Read WQ actionButton
	output$readwq=renderUI({
		req(robs$sel_sites)
		ns <- session$ns
		actionButton(ns('readwq'), 'Read water quality data', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('file-import'), width="100%")
	})

	### Read wq data
	wq_data=eventReactive(input$readwq, {
		req(robs$sel_sites)
		show_modal_spinner(spin = "double-bounce", color = "#112446", text = "Querying water quality data...", session = shiny::getDefaultReactiveDomain())	
		wq_data=wqTools::readWQP(type='result', siteid=robs$sel_sites)
		wq_data=merge(wq_data, subset(map_sites(), MonitoringLocationIdentifier %in% robs$sel_sites))
		wq_data$value=wqTools::facToNum(wq_data$ResultMeasureValue)
		wq_data$ResultSampleFractionText[is.na(wq_data$ResultSampleFractionText)]="None"
		#wq_data=wqTools::assignAUs(wq_data)
		remove_modal_spinner()
		wq_data
	})

	### Download WQ actionButton
	output$dwnloadwq_button=renderUI({
		req(wq_data())
		ns <- session$ns
		downloadButton(ns('dwnloadwq'), 'Download water quality data', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', style = "width:100%;")
	})
	
	### Download WQ
	output$dwnloadwq <- downloadHandler(
	filename=paste0('data-export-', Sys.Date(),'.xlsx'),
	content = function(file) {writexl::write_xlsx(
		list(
			'data-export'=wq_data()[, !names(wq_data()) %in% c("count","lab","value")]
		),
		path = file, format_headers=F, col_names=T)}
	)

	
	return(list(
		pid=pid,
		ec_data=ec_data,
		wq_data=wq_data
	))
}
