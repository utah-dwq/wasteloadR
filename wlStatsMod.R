# wlStatsMod

wlStatsModUI <- function(id){
	ns <- NS(id)
	tagList(
		fillRow(
			wellPanel(style = "overflow-y:scroll; max-height: 100%; min-height: 100%;",
				(h4("Non-detect option")),
				(shinyWidgets::radioGroupButtons(
					inputId = ns("nd_opt"),
					label = "",
					choices = c("Detection limit", "1/2 detection limit", "NA"),
					selected="1/2 detection limit",
					checkIcon = list(yes = icon("check"))
				)),
				
				(h4("Select seasons")),
				(checkboxGroupButtons(
					inputId = ns("winter"),
					label = "Winter: ",
					choices = seq(1,12,1),
					selected=c(12,1,2)
				)),
				(checkboxGroupButtons(
					inputId = ns("spring"),
					label = "Spring: ",
					choices = seq(1,12,1),
					selected=c(3,4,5)
				)),
				(checkboxGroupButtons(
					inputId = ns("summer"),
					label = "Summer: ",
					choices = seq(1,12,1),
					selected=c(6,7,8)
				)),
				(checkboxGroupButtons(
					inputId = ns("fall"),
					label = "Fall: ",
					choices = seq(1,12,1),
					selected=c(9,10,11)
				)),
				(checkboxGroupButtons(
					inputId = ns("noseason"),
					label = "Exclude: ",
					choices = seq(1,12,1)
				))#,
				
				#h4("Classify sites"),
				#shinyWidgets::radioGroupButtons(
				#	inputId = ns("site_grps"),
				#	label = "",
				#	choices = c("Upstream", "Downstream", "Effluent"),
				#	selected="Upstream",
				#	checkIcon = list(yes = icon("check"))
				#)
			),
			#fillCol(width="95%", 
			#	leaflet::leafletOutput(ns("map"))
			#)
			wellPanel()
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
