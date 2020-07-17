# Figures module

figuresModUI <- function(id){
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
		
		fluidRow(
			column(3, uiOutput(ns('sel_param1')), uiOutput(ns('sel_units1')), uiOutput(ns('sel_frac1'))),
			column(9, tabsetPanel(
				tabPanel("Time series", plotlyOutput(ns('multi_site_ts'), height='600px')),
				tabPanel("Boxplot", plotlyOutput(ns('multi_site_bp'), height='600px')),
				tabPanel("Concentration map", style = "height:600px;", fillRow(fillCol(width="95%", leaflet::leafletOutput(ns("conc_map"), height='100%', width="100%"))))
			))
		)
	)
}


figuresMod <- function(input, output, session, wq_data){
	
	
	
	# Note - currently dropping all NA values in data for figures. Need to add ability to fill w/ detection limts
	
	# Empty reactive objects
	reactive_objects=reactiveValues()

	# Get data & format
	#observe({
	#	req(subset(wq_data(), !is.na(value)))
	#	wq_data=subset(wq_data(), !is.na(value))
	#	wq_data$ActivityStartDate=as.Date(wq_data$ActivityStartDate)
	#	wq_data$ResultMeasure.MeasureUnitCode=toupper(wq_data$ResultMeasure.MeasureUnitCode)
	#	subset(wq_data(), !is.na(value))=wq_data
	#})
	
	# Select param 1
	output$sel_param1 <- renderUI({
		ns <- session$ns
		req(subset(wq_data(), !is.na(value))$CharacteristicName)
		selectInput(ns("sel_param1"),"Select parameter 1", choices = unique(subset(wq_data(), !is.na(value))$CharacteristicName[order(subset(wq_data(), !is.na(value))$CharacteristicName)]))
	})
	
	observe({
		req(input$sel_param1)
		reactive_objects$param1_sub=subset(wq_data(), !is.na(value))[subset(wq_data(), !is.na(value))$CharacteristicName == input$sel_param1,]
	})
	
	observe({
		req(reactive_objects$param1_sub)
		reactive_objects$units1=unique(reactive_objects$param1_sub$ResultMeasure.MeasureUnitCode)
		reactive_objects$fractions1=unique(reactive_objects$param1_sub$ResultSampleFractionText)
	})
	
	# Select units 1
	output$sel_units1 <- renderUI({
		req(reactive_objects$units1)
		ns <- session$ns
		units=reactive_objects$units1
		selectInput(ns("sel_units1"),"Select units 1", choices = units, selected="")
	})
	
    
	observe({
		req(subset(wq_data(), !is.na(value)))
		ns <- session$ns
		updateSelectInput(session, ns('sel_units1'), selected="")
	})
    
	# Select fraction 1
	output$sel_frac1 <- renderUI({
		req(reactive_objects$fractions1)
		ns <- session$ns
		selectInput(ns("sel_frac1"),"Select fraction 1", choices = reactive_objects$fractions1, selected="")
	})
    
	observe({
		req(subset(wq_data(), !is.na(value)))
		ns <- session$ns
		updateSelectInput(session, ns('sel_frac1'), selected="")
	})
    
	# Generate parameter 1 data
	observe({
		req(input$sel_param1, input$sel_units1, input$sel_frac1)
			## Data
			param1=subset(subset(wq_data(), !is.na(value)), CharacteristicName == input$sel_param1 & ResultSampleFractionText==input$sel_frac1)
			if(dim(param1)[1]>0){
				param1$target_unit=input$sel_units1
				param1=wqTools::convertUnits(param1, input_units='ResultMeasure.MeasureUnitCode', target_units = "target_unit", value_var='value', conv_val_col='plot_value')
				param1=param1[order(param1$ActivityStartDate),]
				reactive_objects$param1=droplevels(unique(param1[,c('MonitoringLocationIdentifier','ActivityStartDate','LatitudeMeasure','LongitudeMeasure','CharacteristicName','plot_value','target_unit','MonitoringLocationName','ResultSampleFractionText')]))
			}
	})
    
	
	# Multi-site figure labels & visibilities
	observe({
		req(reactive_objects$param1)
		reactive_objects$title = input$sel_param1
		reactive_objects$ylab = paste0(input$sel_param1,' (', input$sel_units1,')')
		reactive_objects$ylab2 = paste0(input$sel_param2,' (', input$sel_units2,')')
	})
    
	# Multi-site time series
	multi_site_ts=reactive({
		req(reactive_objects$param1, input$sel_units1)				
		#if(all(!is.na(reactive_objects$param1$plot_value))){
			plot_ly(source="a") %>%
				add_trace(data=reactive_objects$param1, type = 'scatter', mode = 'lines+markers', x=~as.Date(ActivityStartDate), y=~plot_value, color = ~MonitoringLocationIdentifier, marker = list(size=10), visible=T, text=~MonitoringLocationIdentifier) %>%
				layout(
					title = reactive_objects$title,
					xaxis = list(title = "Date"),
					yaxis = list(title = reactive_objects$ylab)
				) %>% 
				config(displaylogo = FALSE,
					modeBarButtonsToRemove = c(
						'sendDataToCloud',
						'lasso2d'
					)
				)
	})

	output$multi_site_ts=renderPlotly({
		multi_site_ts()
	})
    	
	# Multi site boxplot
	multi_site_bp=reactive({
		req(reactive_objects$param1, input$sel_units1, reactive_objects$title, reactive_objects$ylab)
		plot_ly(data=reactive_objects$param1, type = 'box', y = ~plot_value, color = ~MonitoringLocationIdentifier, visible=T) %>%
			layout(
				title = reactive_objects$title,
				xaxis = list(title = "Monitoring location ID"),
				xaxis2 = list(overlaying = "x", zeroline=F, showticklabels = FALSE, showgrid = FALSE),
				yaxis = list(title = reactive_objects$ylab)
			) %>%
			config(displaylogo = FALSE,
				modeBarButtonsToRemove = c(
					'sendDataToCloud',
					'select2d',
					'lasso2d'
				)
			)
	})

	output$multi_site_bp=renderPlotly({
		multi_site_bp()
	})
    
	
	## Concentration map		
	conc_map = wqTools::buildMap(plot_polys=F, search="")
	conc_map=conc_map%>%clearGroup('Sites') %>% clearControls()
	conc_map = leaflet::addLayersControl(conc_map,
		position ="topleft",
		baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites"),
		options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=TRUE))
	conc_map=addMapPane(conc_map,"site_markers", zIndex = 450)
	conc_map=hideGroup(conc_map, "Assessment units")
	conc_map=hideGroup(conc_map, "Site-specific standards")
	conc_map=hideGroup(conc_map, "Beneficial uses")
	conc_map=removeMeasure(conc_map)
    
	output$conc_map <- leaflet::renderLeaflet({
		isolate({
			req(reactive_objects$param1)
			if(!is.null(reactive_objects$param1) & dim(reactive_objects$param1)[1]>0){
				sites=reactive_objects$param1
				sites=sites[!is.na(sites$plot_value),]
				count=aggregate(plot_value~MonitoringLocationIdentifier+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='length')
				names(count)[names(count)=='plot_value'] = 'count'
				sites=aggregate(plot_value~MonitoringLocationIdentifier+MonitoringLocationName+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='mean', na.rm=TRUE)
				sites=merge(sites,count,all.x=T)
				sites$radius=scales::rescale(sites$plot_value, c(5,35))
				min_lat=min(sites$LatitudeMeasure)*0.999
				min_lng=min(sites$LongitudeMeasure)*0.999
				max_lat=max(sites$LatitudeMeasure)*1.001
				max_lng=max(sites$LongitudeMeasure)*1.001
				leg_labs=c(signif(quantile(sites$plot_value, 0.10),3), signif(median(sites$plot_value),3), signif(quantile(sites$plot_value, 0.90),3))
				leg_sizes=c(quantile(sites$radius, 0.10), median(sites$radius), quantile(sites$radius, 0.90))*2
				conc_map = conc_map %>% fitBounds(min_lng,min_lat,max_lng,max_lat) %>%	
					addCircleMarkers(data = sites, lat=~LatitudeMeasure, lng=~LongitudeMeasure, group="Sites", layerId=~MonitoringLocationIdentifier, color='blue', stroke=F, fillOpacity=0.5,
						radius = ~radius, options = pathOptions(pane = "site_markers"),
						popup = paste0(
							"MLID: ", sites$MonitoringLocationIdentifier,
							"<br> ML name: ", sites$MonitoringLocationName,
							"<br> Average Parameter Value: ", sites$plot_value,
							"<br> Sample Count: ", sites$count)
					) %>%
				addLegendCustom(colors = c("blue", "blue", "blue"), labels = leg_labs, sizes = leg_sizes, title=reactive_objects$ylab)
			}
		})
        
		conc_map
	})
		
    conc_map_output=reactive({
		conc_map_output = wqTools::buildMap(plot_polys=FALSE, search="")
		conc_map_output=conc_map_output%>%clearGroup('Sites') %>% clearControls()
		conc_map_output = leaflet::addLayersControl(conc_map_output,
			position ="topleft",
			baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites"),
			options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=TRUE))
		conc_map_output=addMapPane(conc_map_output,"site_markers", zIndex = 450)
		conc_map_output=hideGroup(conc_map_output, "Assessment units")
		conc_map_output=hideGroup(conc_map_output, "Site-specific standards")
		conc_map_output=hideGroup(conc_map_output, "Beneficial uses")
		conc_map_output=removeMeasure(conc_map_output)
			if(!is.null(reactive_objects$param1) & dim(reactive_objects$param1)[1]>0){
				sites=reactive_objects$param1
				sites=sites[!is.na(sites$plot_value),]
				count=aggregate(plot_value~MonitoringLocationIdentifier+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='length')
				names(count)[names(count)=='plot_value'] = 'count'
				sites=aggregate(plot_value~MonitoringLocationIdentifier+MonitoringLocationName+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='mean', na.rm=TRUE)
				sites=merge(sites,count,all.x=T)
				sites$radius=scales::rescale(sites$plot_value, c(5,35))
				min_lat=min(sites$LatitudeMeasure)*0.999
				min_lng=min(sites$LongitudeMeasure)*0.999
				max_lat=max(sites$LatitudeMeasure)*1.001
				max_lng=max(sites$LongitudeMeasure)*1.001
				leg_labs=c(signif(quantile(sites$plot_value, 0.10),3), signif(median(sites$plot_value),3), signif(quantile(sites$plot_value, 0.90),3))
				leg_sizes=c(quantile(sites$radius, 0.10), median(sites$radius), quantile(sites$radius, 0.90))*2
				conc_map_output = conc_map_output %>% flyToBounds(min_lng,min_lat,max_lng,max_lat) %>%	
					addCircleMarkers(data = sites, lat=~LatitudeMeasure, lng=~LongitudeMeasure, group="Sites", layerId=~MonitoringLocationIdentifier, color='blue', stroke=F, fillOpacity=0.5,
						radius = ~radius, options = pathOptions(pane = "site_markers"),
						popup = paste0(
							"MLID: ", sites$MonitoringLocationIdentifier,
							"<br> ML name: ", sites$MonitoringLocationName,
							"<br> Average Parameter Value: ", sites$plot_value,
							"<br> Sample Count: ", sites$count)
					) %>%
				addLegendCustom(colors = c("blue", "blue", "blue"), labels = leg_labs, sizes = leg_sizes, title=reactive_objects$ylab)
			}
		conc_map_output
	})
	
	# Map proxy
	conc_proxy = leaflet::leafletProxy("conc_map")
   
	# Custom leaflet legend (re-size circles)
    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, title = NULL){
		colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
		labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
		return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = title))
	}
	
    
	# Update concentration map via proxy on param1 change
	observeEvent(reactive_objects$param1, {
		#req(reactive_objects$param1, reactive_objects$ylab)
		conc_proxy%>%clearGroup('Sites') %>% clearControls()
		if(exists('sites', inherits=FALSE)){rm(sites)}
		if(any(!is.na(reactive_objects$param1$plot_value))){
			sites=reactive_objects$param1
			sites=sites[!is.na(sites$plot_value),]
			count=aggregate(plot_value~MonitoringLocationIdentifier+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='length')
			names(count)[names(count)=='plot_value'] = 'count'
			sites=aggregate(plot_value~MonitoringLocationIdentifier+MonitoringLocationName+LatitudeMeasure+LongitudeMeasure+target_unit, sites, FUN='mean')
			sites=merge(sites,count,all.x=T)
			sites$radius=scales::rescale(sites$plot_value, c(5,35))
			min_lat=min(sites$LatitudeMeasure)*0.999
			min_lng=min(sites$LongitudeMeasure)*0.999
			max_lat=max(sites$LatitudeMeasure)*1.001
			max_lng=max(sites$LongitudeMeasure)*1.001
			leg_labs=c(signif(quantile(sites$plot_value, 0.10),3), signif(median(sites$plot_value),3), signif(quantile(sites$plot_value, 0.90),3))
			leg_sizes=c(quantile(sites$radius, 0.10), median(sites$radius), quantile(sites$radius, 0.90))*2
			conc_proxy %>% flyToBounds(min_lng,min_lat,max_lng,max_lat) %>%	
				addCircleMarkers(data = sites, lat=~LatitudeMeasure, lng=~LongitudeMeasure, group="Sites", layerId=~MonitoringLocationIdentifier, color='blue', stroke=F, fillOpacity=0.5,
					radius = ~radius, options = pathOptions(pane = "site_markers"),
					popup = paste0(
						"MLID: ", sites$MonitoringLocationIdentifier,
						"<br> ML name: ", sites$MonitoringLocationName,
						"<br> Average Parameter Value: ", sites$plot_value,
						"<br> Sample Count: ", sites$count)
				) %>%
			addLegendCustom(colors = c("blue", "blue", "blue"), labels = leg_labs, sizes = leg_sizes, title=reactive_objects$ylab)
		}
	})
	
	return(list(
		select_data=reactive({event_data("plotly_selected", source="a")}),
		param1=reactive({input$sel_param1}),
		param_choices=reactive({
				req(subset(wq_data(), !is.na(value)))
				unique(subset(wq_data(), !is.na(value))$CharacteristicName[order(subset(wq_data(), !is.na(value))$CharacteristicName)])
			}),
		multi_site_ts=multi_site_ts,
		multi_site_bp=multi_site_bp#,
		#conc_map=conc_map_output
	))
	
}
