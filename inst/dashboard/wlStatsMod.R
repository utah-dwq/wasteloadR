# wlStatsMod

wlStatsModUI <- function(id){
	ns <- NS(id)
	tagList(
		shinyBS::bsCollapse(multiple=F, open=1,
			shinyBS::bsCollapsePanel(list(icon('plus-circle'),"Input parameters"), value=1,
				column(3,
					h4("Non-detect option"),
					shinyWidgets::radioGroupButtons(
						inputId = ns("nd_opt"),
						label = "",
						choices = c("Detection limit", "1/2 detection limit", "NA"),
						selected="1/2 detection limit",
						checkIcon = list(yes = icon("check"))
					)
				),
				
				column(4, 
					h4("Select seasons"),
					checkboxGroupButtons(
						inputId = ns("winter"),
						label = "Winter: ",
						choices = seq(1,12,1),
						selected=c(12,1,2)
					),
					checkboxGroupButtons(
						inputId = ns("spring"),
						label = "Spring: ",
						choices = seq(1,12,1),
						selected=c(3,4,5)
					),
					checkboxGroupButtons(
						inputId = ns("summer"),
						label = "Summer: ",
						choices = seq(1,12,1),
						selected=c(6,7,8)
					),
					checkboxGroupButtons(
						inputId = ns("fall"),
						label = "Fall: ",
						choices = seq(1,12,1),
						selected=c(9,10,11)
					),
					checkboxGroupButtons(
						inputId = ns("noseason"),
						label = "Exclude: ",
						choices = seq(1,12,1)
					)
				)
			),
			shinyBS::bsCollapsePanel(list(icon('plus-circle'),"Wasteload statistics"), value=2,
				wellPanel(
					fluidRow(
						column(4, 
							uiOutput(ns("site_picker")),
							verbatimTextOutput(ns("site_name"))
						),
						column(4,uiOutput(ns("param_picker")))
					)
				),
				fluidRow(
					DT::dataTableOutput(ns("pivot"))
				)
			)
		)
	)
}

wlStatsMod <- function(input, output, session, wq_data){

	observeEvent(input$winter, ignoreInit=T, ignoreNULL=F, {
		updateCheckboxGroupButtons(session=session, inputId="spring", selected=input$spring[!input$spring %in% input$winter])
		updateCheckboxGroupButtons(session=session, inputId="summer", selected=input$summer[!input$summer %in% input$winter])
		updateCheckboxGroupButtons(session=session, inputId="fall", selected=input$fall[!input$fall %in% input$winter])
		updateCheckboxGroupButtons(session=session, inputId="noseason", selected=as.character(seq(1,12,1)[!seq(1,12,1) %in% as.numeric(c(input$winter, input$spring, input$summer, input$fall))]))
		#print(seq(1,12,1)[!seq(1,12,1) %in% as.numeric(c(input$winter, input$spring, input$summer, input$fall))])
		#print(as.numeric(c(input$winter, input$spring, input$summer, input$fall)))
	})
	observeEvent(input$spring, ignoreInit=T, ignoreNULL=F, {
		updateCheckboxGroupButtons(session=session, inputId="winter", selected=input$winter[!input$winter %in% input$spring])
		updateCheckboxGroupButtons(session=session, inputId="summer", selected=input$summer[!input$summer %in% input$spring])
		updateCheckboxGroupButtons(session=session, inputId="fall", selected=input$fall[!input$fall %in% input$spring])
		updateCheckboxGroupButtons(session=session, inputId="noseason", selected=as.character(seq(1,12,1)[!seq(1,12,1) %in% as.numeric(c(input$winter, input$spring, input$summer, input$fall))]))
	})
	observeEvent(input$summer, ignoreInit=T, ignoreNULL=F, {
		updateCheckboxGroupButtons(session=session, inputId="spring", selected=input$spring[!input$spring %in% input$summer])
		updateCheckboxGroupButtons(session=session, inputId="winter", selected=input$winter[!input$winter %in% input$summer])
		updateCheckboxGroupButtons(session=session, inputId="fall", selected=input$fall[!input$fall %in% input$summer])
		updateCheckboxGroupButtons(session=session, inputId="noseason", selected=as.character(seq(1,12,1)[!seq(1,12,1) %in% as.numeric(c(input$winter, input$spring, input$summer, input$fall))]))
	})
	observeEvent(input$fall, ignoreInit=T, ignoreNULL=F, {
		updateCheckboxGroupButtons(session=session, inputId="spring", selected=input$spring[!input$spring %in% input$fall])
		updateCheckboxGroupButtons(session=session, inputId="summer", selected=input$summer[!input$summer %in% input$fall])
		updateCheckboxGroupButtons(session=session, inputId="winter", selected=input$winter[!input$winter %in% input$fall])
		updateCheckboxGroupButtons(session=session, inputId="noseason", selected=as.character(seq(1,12,1)[!seq(1,12,1) %in% as.numeric(c(input$winter, input$spring, input$summer, input$fall))]))
	})
	observeEvent(input$noseason, ignoreInit=T, ignoreNULL=F, {
		updateCheckboxGroupButtons(session=session, inputId="spring", selected=input$spring[!input$spring %in% input$noseason])
		updateCheckboxGroupButtons(session=session, inputId="summer", selected=input$summer[!input$summer %in% input$noseason])
		updateCheckboxGroupButtons(session=session, inputId="winter", selected=input$winter[!input$winter %in% input$noseason])
		updateCheckboxGroupButtons(session=session, inputId="fall", selected=input$fall[!input$fall %in% input$noseason])
	})


	## Map output
	#output$map <- leaflet::renderLeaflet({
	#	#show_modal_spinner(spin = "double-bounce", color = "#112446", text = "Initializing...", session = shiny::getDefaultReactiveDomain())	
	#	buildMap(plot_polys=F) %>% addMapPane("markers", zIndex = 420) %>% addMapPane("sites", zIndex = 419) %>% addMapPane("highlight", zIndex = 418)
	#	#remove_modal_spinner()
	#	#map
	#})



	## Site & parameter selections
	output$site_picker=renderUI({
		req(wq_data())
		ns <- session$ns
		sites=unique(wq_data()$MonitoringLocationIdentifier)
		shinyWidgets::pickerInput(ns('site_picker'), 'Site:', choices=sites)
	})
	output$site_name=renderText({
		req(input$site_picker, wq_data())
		unique(wq_data()$MonitoringLocationName[wq_data()$MonitoringLocationIdentifier == input$site_picker])	
	})
	output$param_picker=renderUI({
		req(wq_data(),input$site_picker)
		ns <- session$ns
		params=unique(subset(wq_data(), MonitoringLocationIdentifier %in% input$site_picker)$CharacteristicName)
		params=params[order(params)]
		shinyWidgets::pickerInput(ns('param_picker'), 'Parameter:', choices=params)
	})

	## Pivot table output
	wl_stats=reactive({
		req(wq_data(), input$nd_opt)
		if(input$nd_opt == "Detection limit"){nd="det"}
		if(input$nd_opt == "1/2 detection limit"){nd="half"}
		if(input$nd_opt == "NA"){nd="NA"}		
		wasteloadR_stats(wq_data(), seasons=list(spring=input$spring, summer=input$summer, fall=input$fall, winter=input$winter, unassigned=input$noseason), nd="half") # Update for ND selection
	})
	observe({print(head(wl_stats()))})
	
	output$pivot=DT::renderDataTable({
		req(wl_stats(), input$site_picker, input$param_picker)
		table_data=subset(wl_stats(), MonitoringLocationIdentifier %in%input$site_picker & CharacteristicName %in% input$param_picker)
		print(table_data)
		table_data=table_data[,c("ResultSampleFractionText", "ResultMeasure.MeasureUnitCode","SampleCollectionMethod.MethodName","Season", "min","pctile20","median","mean","pctile80","max","samp_count")]
		DT::datatable(data.frame(lapply(table_data, as.factor)),
			selection='none', rownames=FALSE, filter="top",
			options = list(dom = 't', paging = FALSE, scrollX=TRUE, digits = 2)
		) %>%
		DT::formatRound(columns=c("min","pctile20","median","mean","pctile80","max"), digits=2) %>%
		DT::formatRound(columns=c("samp_count"), digits=0)
	})




}

#### Site group selection
#### ND handling
#
#wl_stats=reactive({
#	wasteloadR_stats(wq_data())
#})
#
##observe({print(wl_stats())})
#
#output$wl_stats_table=DT::renderDataTable({
#	req(wl_stats())
#	DT::datatable(data.frame(lapply(wl_stats(),as.factor)),
#		selection='none', rownames=FALSE, filter="top",
#		options = list(scrollY = '600px', paging = TRUE, scrollX=TRUE)
#	)	
#})
#
#tabsetPanel(
#	tabPanel("Pivot select"),
#	tabPanel("Full table", DT::DTOutput("wl_stats_table"))
#)
