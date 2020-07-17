# Permit gauge module

permitGaugeModUI <- function(id){
	ns <- NS(id)
	tagList(		
		fluidRow(
			column(3, 
				uiOutput(ns("feature_picker")),
				uiOutput(ns("loc_picker"))
			),
			column(3, 
				uiOutput(ns("param_picker")),
				uiOutput(ns("stat_picker"))
			),
			column(3, 
				uiOutput(ns("unit_picker"))
			),
			column(3, flexdashboard::gaugeOutput(ns("gauge")))
		),
		fluidRow(plotlyOutput(ns("time_series")))
	)
}


permitGaugeMod <- function(input, output, session, ec_data){
	
	# Colors
	cols=list(
	list('rgba(200, 30, 30, 0.5)','rgb(200, 30, 30)'),      #red 1
	list('rgba(245, 121, 58, 0.5)','rgb(245, 121, 58)'),    #orange 2
	list('rgba(230, 200, 50, 0.5)','rgb(230, 200, 50)'),    #yellow 3
	list('rgba(60, 230, 70, 0.5)','rgb(60, 230, 70)'),      #green 4
	list('rgba(15, 32, 128, 0.5)','rgb(15, 32, 128)'),      #blue 5
	list('rgba(169, 90, 161, 0.5)','rgb(169, 90, 161)')    #purple 6
	)


	gauge_cols=c(cols[[4]][[2]],cols[[2]][[2]],cols[[1]][[2]])

	
	# Pickers
	## Features
	output$feature_picker=renderUI({
		ns <- session$ns
		shinyWidgets::pickerInput(ns('feature_picker'), 'Feature ID:', choices=unique(as.factor(ec_data()$perm_feature_nmbr)))
	})
	
	output$loc_picker=renderUI({
		ns <- session$ns
		data=subset(ec_data(), perm_feature_nmbr %in% input$feature_picker)
		shinyWidgets::pickerInput(ns('loc_picker'), 'Location:', choices=unique(as.character(data$monitoring_location_desc)))
	})	
	
	## Parameter
	output$param_picker=renderUI({
		ns <- session$ns
		data=subset(ec_data(), perm_feature_nmbr %in% input$feature_picker & monitoring_location_desc %in% input$loc_picker)
		params=unique(as.character(data$parameter_desc))
		params=params[order(params)]
		shinyWidgets::pickerInput(ns('param_picker'), 'Parameter:', choices=params, options = list(`live-search` = TRUE))
	})	
	
	## Stat
	output$stat_picker=renderUI({
		ns <- session$ns
		data=subset(ec_data(), perm_feature_nmbr %in% input$feature_picker & monitoring_location_desc %in% input$loc_picker & parameter_desc %in% input$param_picker)
		shinyWidgets::pickerInput(ns('stat_picker'), 'Statistic:', choices=unique(as.character(data$statistical_base_short_desc)))
	})	
	
	## Unit
	output$unit_picker=renderUI({
		ns <- session$ns
		data=subset(ec_data(), perm_feature_nmbr %in% input$feature_picker & monitoring_location_desc %in% input$loc_picker & parameter_desc %in% input$param_picker & statistical_base_short_desc %in% input$stat_picker)
		shinyWidgets::pickerInput(ns('unit_picker'), 'Unit:', choices=unique(as.character(data$standard_unit_desc)))
	})	
	
	## Subset data
	subset_data=reactive({
		req(input$feature_picker, input$param_picker, input$stat_picker, input$unit_picker)
			subset(ec_data(), perm_feature_nmbr %in% input$feature_picker & monitoring_location_desc %in% input$loc_picker & parameter_desc %in% input$param_picker & 
				statistical_base_short_desc %in% input$stat_picker & standard_unit_desc %in% input$unit_picker & !is.na(dmr_value_standard_units))
	})
	
	
	## Generate plots
	
	
	
	### Gauge
	output$gauge=flexdashboard::renderGauge({
		#ec_data=wqTools::readECHO_ec(p_id="UT0023639", start_date="01/01/2010", end_date="03/24/2020")
		#subset_data=function(){
		#	return(subset(ec_data, perm_feature_nmbr == 1 & parameter_desc == "Nitrogen, ammonia total [as N]" & statistical_base_short_desc=="DAILY MX"  & monitoring_location_desc=="Effluent Gross"& !is.na(dmr_value_standard_units)))
		#	#return(subset(ec_data, perm_feature_nmbr == 1 & parameter_desc == "BOD, 5-day, percent removal" & monitoring_location_desc=="Effluent Gross" & statistical_base_short_desc=="MINIMUM" & !is.na(dmr_value_standard_units)))
		#}
		#head(subset_data())
		if(dim(subset_data())[1]>0){
			current_val=with(subset_data(), {dmr_value_standard_units[as.Date(monitoring_period_end_date, format="%m/%d/%Y")==max(as.Date(monitoring_period_end_date, format="%m/%d/%Y"))]})
			limit=with(subset_data(), {limit_value_standard_units[as.Date(monitoring_period_end_date, format="%m/%d/%Y")==max(as.Date(monitoring_period_end_date, format="%m/%d/%Y"))]})
			max_val=max(max(subset_data()$dmr_value_standard_units, na.rm=T), limit, na.rm=T)
			#if(is.na(limit)){limit=max_val*1.25}
			min_val=min(subset_data()$dmr_value_standard_units, na.rm=T)
			unit=unique(subset_data()$standard_unit_desc)
			if(round(max_val,2)==0){round_val=3}else{round_val=2}
			
			if(unique(subset_data()$statistical_base_type_code)=="MIN"){
				#print('min')
				#print(c(current_val, limit, max_val, min_val))
				if(is.na(limit)){limit=min_val}
				flexdashboard::gauge(round(current_val,round_val), min=0, max=round(max_val,round_val), symbol=paste0(" ", unit),
					sectors=flexdashboard::gaugeSectors(
						colors=gauge_cols,
						success=c(limit*1.25, max_val),
						warning=c(limit*1.05, limit*1.25),
						danger=c(limit, limit*1.05)
					)
				)
			}else{
				#print('max')
				#print(c(current_val, limit, max_val, min_val))
				if(is.na(limit)){limit=max_val}
				flexdashboard::gauge(round(current_val,round_val), min=0, max=round(limit,round_val), symbol=paste0(" ", unit),
					sectors=flexdashboard::gaugeSectors(
							colors=gauge_cols,
							success=c(0,limit*0.75),
							warning=c(limit*0.75,limit*0.95),
							danger=c(limit*0.95, limit)
					)
				)
			}
		}
	})

	
    ### Time series
	output$time_series=renderPlotly({
		unit=unique(subset_data()$standard_unit_desc)
		label=unique(subset_data()$parameter_desc)
		plot_ly(data=subset_data(), x=~as.Date(monitoring_period_end_date, format="%m/%d/%Y")) %>%
			add_markers(y=~dmr_value_standard_units, name="Reported values") %>% 
			add_lines(y=~limit_value_standard_units, name="Permit limit") %>% 
			layout(
				xaxis = list(title = ""),
				yaxis = list(side = 'left', title=paste0(label, " (", unit, ")")),
				legend = list(x = 0.01, y = 0.9)
			) %>% 
			config(displaylogo = FALSE,
				modeBarButtonsToRemove = c(
					'sendDataToCloud',
					'hoverClosestCartesian',
					'hoverCompareCartesian',
					'lasso2d',
					'select2d'
				)
			)
		#if(!is.na(limit())){
		#	time_series=time_series %>% add_lines(y=~limit_value_standard_units, name="Permit limit")
		#}
		
		#time_series

	})

}


