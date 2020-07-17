# Find sites module

findSitesModUI <- function(id){
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
				#fluidRow(shinyWidgets::radioGroupButtons(ns('search_type'), 'Search by:', choices=c('Assessment unit','Bounding box'), selected='Bounding box')),
				fluidRow(uiOutput(ns('query_sites')))
			),
			fillCol(width="100%", leaflet::leafletOutput(ns("map"), height='100%', width="100%"))
		),
	)
}

findSitesMod <- function(input, output, session, pid, permits_coords){
	
	# Map output
	output$map <- leaflet::renderLeaflet({
		req(pid())
		basemap=
			buildMap(plot_polys=F) %>% addMapPane("markers", zIndex = 420) %>% addMapPane("sites", zIndex = 419) %>% 
			leaflet::addCircleMarkers(data=subset(permits_coords, locationID==pid()), lat=~LatitudeMeasure, lng=~LongitudeMeasure, options = pathOptions(pane = "markers")) %>%
			leaflet::setView(subset(permits_coords, locationID==pid())$LongitudeMeasure, subset(permits_coords, locationID==pid())$LatitudeMeasure, zoom=9)
		basemap
		
		#if(input$search_type %in% c('Assessment unit')){
		#	poly=wqTools::au_poly
		#	poly=within(poly, {
		#		label=lapply(paste0(
		#					'<p>',
		#					"AU name: ", poly$AU_NAME,
		#					'<br />', "AU ID: ", poly$ASSESS_ID,
		#					'<br />', "AU type: ", poly$AU_Type
		#				), HTML)
		#		id=ASSESS_ID
		#	})
		#	reactive_objects$poly=poly
		#	map=basemap %>% 
		#		addPolygons(data=poly, group="Polygons",smoothFactor=2,fillOpacity = 0.1, layerId=~polyID,weight=3,color="orange", options = pathOptions(pane = "au_poly"),
		#			label=~label
		#		) %>% 
		#		addMapPane("highlight", zIndex = 414) %>%
		#		removeLayersControl() %>% 
		#		leaflet::addLayersControl(
		#					position ="topleft",
		#					baseGroups = c("Topo","Satellite"),overlayGroups = c("Polygons"),
		#					options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))
		#}
        #
		#
		#if(input$search_type == 'Bounding box'){
		#	map=basemap %>%
		#		removeLayersControl() %>% 
		#		addLayersControl(
		#			position ="topleft",
		#			baseGroups = c("Topo","Satellite"),
		#			options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE)) %>%
		#		leaflet.extras::addDrawToolbar(singleFeature=T, polylineOptions=F, polygonOptions=F, circleOptions=F, circleMarkerOptions=F, markerOptions=F)		
		#}
        #
		#map

	})
	
	
	# Query sites
	## Button
	#output$query_sites=renderUI({
	#	ns <- session$ns
	#	actionButton(ns('query_sies'), 'Query sites', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('cloud-download-alt'))
	#})

	## Read WQP
	sites=eventReactive(input$map_bounds, ignoreInit=T, {
		map_box=input$map_bounds
		bbox=paste(map_box[4], map_box[3], map_box[2], map_box[1], sep='%2C')
		wqTools::readWQP(type='sites', bBox=bbox)
	})
	observe({print(head(sites()))})
	
	# Update map
	observeEvent(sites(), {
		req(sites())
		leafletProxy("map") %>% 
			leaflet::addCircleMarkers(data=sites(), lat=~LatitudeMeasure, lng=~LongitudeMeasure, options = pathOptions(pane = "sites"))
	})
	
	
	
	
	# Selected AUs
	#reactive_objects=reactiveValues()		
	#reactive_objects$sel_polys=NULL
	#
	#observeEvent(input$map_shape_click, ignoreInit=T, {
	#	click = input$map_shape_click$id
	#	poly=reactive_objects$poly
	#	if(!is.null(click)){
	#		sel_id=as.character(unique(poly$id[poly$polyID==click]))
	#	}
	#	if(sel_id %in% reactive_objects$sel_polys){
	#		reactive_objects$sel_polys=reactive_objects$sel_polys[!reactive_objects$sel_polys %in% sel_id]
	#	}else{
	#		reactive_objects$sel_polys=append(reactive_objects$sel_polys, sel_id)
	#	}
	#})
	#
	#observe({
	#	print(reactive_objects$sel_polys)
	#})
	#
	#
	## Update map highlight
	#observeEvent(reactive_objects$sel_polys, ignoreNULL = F, ignoreInit=T, {
	#	leafletProxy("map") %>%
	#		clearGroup(group='highlight') %>%
	#		addPolygons(data=reactive_objects$poly[reactive_objects$poly$id %in% reactive_objects$sel_polys,],
	#			group='highlight', options = pathOptions(pane = "highlight"), color='chartreuse', opacity = 0.75, fillOpacity = 0.4, weight = 5)
	#})
    #
	## Extract bbox
	#bbox=eventReactive(input$map_draw_new_feature, ignoreInit=T, {
	#	draw=input$map_draw_new_feature
	#	c(
	#		draw$geometry$coordinates[[1]][[1]][[1]], #W
	#		draw$geometry$coordinates[[1]][[1]][[2]], #S
	#		draw$geometry$coordinates[[1]][[3]][[1]], #E
	#		draw$geometry$coordinates[[1]][[3]][[2]]  #N
	#	)
	#})
	#observe({
	#	print(bbox())
	#})
    #
    #
	## Query sites button
	#output$query_sites=renderUI({
	#	ns <- session$ns
	#	if(input$search_type=="Assessment unit"){
	#		req(reactive_objects$sel_polys)
	#		actionButton(ns('read_ec_au'), 'Query sites', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('cloud-download-alt'))
	#	}else{
	#		req(input$map_draw_new_feature)		
	#		actionButton(ns('read_ec_bb'), 'Query sites', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('cloud-download-alt'))
	#	}
	#})
	#
	## Query sites by au
	#
	#
	## Query sites by bb
	#sites=eventReactive(input$read_ec_bb,{
	#		bbox=paste(bbox()[1], bbox()[2], bbox()[3], bbox()[4], sep='%2C')
	#		wqTools::readWQP(type='sites', bBox=bbox)
	#})
	#observe({print(head(sites()))})
	
	
	
	
	
	
	

	
}
