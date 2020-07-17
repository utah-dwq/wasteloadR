# wasteloadr app

library(shiny)
library(wqTools)
library(leaflet)
#library(waiter)
library(shinybusy)
library(plotly)


## Modules
source('findPermitMod.R')
source('permitGaugeMod.R')

# Spinner specs
#spinner <- tagList(
#  tags$img(
#  	src="dwq_logo_small.png", 
#  	height=100, 
#  	id = "myImage" # set id
#  ),
#  spin_chasing_dots(),
#  span("Querying permits...", style="color:white;")
#)

## User interface
ui <-fluidPage(
	# Waiter setup
	#add_busy_bar(color = "#FF0000"),
	use_waiter(),
	waiter_show_on_load(spinner),
	
	## Header
	headerPanel(
		title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo_draft.png', height = 125, width = 100*2.85*1.75), target="_blank"),
		tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="wasteloadr")
	),
	
	## UI
	column(12,
		shinyBS::bsCollapse(multiple=F, open=1,
			shinyBS::bsCollapsePanel(list(icon('plus-circle'),"Find effluent data", icon('search-location')), value=1,
				findPermitModUI('find_permit')
			),		
			shinyBS::bsCollapsePanel(list(icon('plus-circle'),"Analyze effluent data", icon('tachometer-alt')), value=2,
				permitGaugeModUI('permit_gauge1')
			)
		)
	)
)


## Server
server <- function(input, output, session){
	
	### Select permit
	#show_modal_spinner(spin = "double-bounce", color = "#112446", text = "Querying permit locations...", session = shiny::getDefaultReactiveDomain())
	find_permit=callModule(module=findPermitMod, id='find_permit')
	#remove_modal_spinner()	
	
	### Extract permit ID from findPermitMod
	pid=reactive({gsub(" .*$", "", find_permit$pid())})
	observe({
		print(pid())
	})

	### Extract effluent_data from findPermitMod
	effluent_data=reactive({
		req(find_permit$ec_data())
		find_permit$ec_data()
	})
	
	observe({
		print(dim(effluent_data()))
	})
	
	### Permit gauge module
	permit_gauge1=callModule(module=permitGaugeMod, id='permit_gauge1', effluent_data)
	
	### Select WQ sites module
	
	
	
	
	
}

shinyApp(ui = ui, server = server)

