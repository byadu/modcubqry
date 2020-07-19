#' @title modqry
#' @description A 'shiny' Module for Creating SQL queries via a front end
#' @import shiny
#' @import shinyWidgets
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyBS
#' @import modchart
#' @import libcubmeta
#' @import libcubolap
#' @import modcubfilt
#' @export
#' @title xr
#' @description reactive values for this module ('xplorer')
xr<- reactiveValues(dimgrp=NULL, measgrp=NULL, mselids=NULL, dselids=NULL, mselnames=NULL, dselnames=NULL, fdimsel=NULL, gtype='dt', xdilist=NULL)
#' @export 
#' @title f
#' @description reactive values for row selection from DT / plotly
f<- reactiveValues(rows_selected=NULL)
#' @export 
#' @title rg
#' @description reactive values for the 'graph': containing data from SQL query and chart properties
rg<- reactiveValues(g=NULL)

adhocinit<- 1

#' @export
#' @title analyze
#' @description the query module
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param id is the caller id
#' @param M is the meta data connection structure
#' @param D is the data connection structure
#' @param uid is the logged in user id

analyze<- function(input, output, session, M, D, uid) {
	ns<- session$ns

	observeEvent(input$id1,ignoreInit=T,  ignoreNULL=F, {
		if(is.null(input$id1)) {
			xr$mselids<- NULL
			xr$mselnames<- NULL
			xr$xdilist<- M$mt$dilist
			}
		else {
			xr$mselnames<- xmsel(xr$mselnames, input$id1)
			xr$mselids<- lookupid(M, isolate(xr$mselnames), 'm')
			dimgrp<- xgetdims(M$cfg, isolate(xr$mselids))
			if(!is.null(dimgrp))
				xr$xdilist<- xmakepicklist(M$cfg, dimgrp)
			xr$dimgrp<- dimgrp
			}
		updatePickerInput(session, "id2", choices=xr$xdilist, selected=isolate(input$id2)) 
		})
	output$r1<- renderText({
		paste("Measures:", paste(xr$mselnames, collapse=", "))
		})

	observeEvent(input$id2, ignoreInit=T, ignoreNULL=F, {
		if(is.null(input$id2)) {
			xr$dselids<- NULL
			xr$dselnames<- NULL
			milist<- M$mt$milist
			}
		else {
			xr$dselnames<- xmsel(xr$dselnames, input$id2)
			xr$dselids<- lookupid(M, isolate(xr$dselnames), 'd')
			measgrp<- xgetmeas(M$cfg, isolate(xr$dselids))
			if(!is.null(measgrp))
				milist<- xmakepicklist(M$cfg, measgrp)
			xr$measgrp<- measgrp
			}
		updatePickerInput(session, "id1", choices=milist, selected=isolate(input$id1)) 
		})
	output$r2<- renderText({
		paste("Dimensions:", paste(xr$dselnames, collapse=", "))
		})

	observeEvent(input$go, ignoreInit=T, {
		if(isolate(input$repdata)=='Report')
			fun<- "dograph"
		else
			fun<- "dodata"
		req(isolate(xr$mselids))
		req(isolate(xr$dselids))
		rg$g<-isolate(do.call(fun, list(M, D, xr$mselids, xr$dselids, xfr$currfilters, xr$gtype)))

		})
	output$fd<- renderUI({
		req(xfr$currfilters)
		f<- xfr$currfilters
		ids<- names(f)
		nms<- getmnames(M$mt$dimensions, ids)
		fd<- list()
		for (i in 1:length(nms)) {
			nm<- nms[i]
			fd[[i]]<- HTML(paste(nm, "=", paste(f[[ids[i]]]$fval, collapse=","), br()))
			}
		xr$dselnames<- setdiff(isolate(xr$dselnames), nms)
		xr$dselids<- lookupid(M, isolate(xr$dselnames), 'd')
		updatePickerInput(session, "id2", choices=xr$xdilist, selected=isolate(xr$dselnames)) 
		fluidPage(do.call(fluidRow, fd))
		})

	output$savepopup<- renderUI({
		a<- NULL
		if(!is.null(rg$g)) {
			a<- savereportUI(ns('saver'),M$fold$ft)
			callModule(savereport, 'saver', isolate(rg$g), M, uid, isolate(xfr$currfilters))
			}
		a
		})

	observeEvent(input$clear, {
		clearmenu()
		})
	clearmenu<- function() {
		xr$new<-1
		xr$measgrp<- NULL
		xr$mselids<- NULL
		xr$dimgrp<- NULL
		xr$dselids<- NULL
		xfr$currfilters<- NULL
		xr$dselnames<- NULL
		xr$mselnames<- NULL
		f$rows_selected<- NULL
		cr$dualmode<- F
		cr$chartlib<- 'DT'
		rg$g<- NULL
		isolate(updatePickerInput(session, "id1", choices=M$mt$milist, selected=NULL))
		isolate(updatePickerInput(session, "id2", choices=M$mt$dilist, selected=NULL))
		}

	setdrill<- function(g, rows_selected) {
		if(is.null(g)) return
		
		for(i in 1:g$gp$gfdim) {
			drillid<- as.character(g$gp$dim[i])
			drillval<- unique(as.character(g$dxy[rows_selected,i]))
			addfilter(M, drillid, drillval, FALSE)
			}
		}

	output$rd<- renderUI({
		req(rg$g)
		a<- chartUI(ns('adhoc'), rg$g)
		callModule(chart, 'adhoc', rg$g, setdrill=setdrill)
		a
		})
	}

#' @export
#' @title analyzeUI
#' @description the query module UI
#' @param id is the caller id
#' @param M is the meta data connection structure
analyzeUI<- function(id, M) {
	ns<- NS(id)

	fluidPage(
	boxPlus(title='Query',width=12,closable=F,collapsible=T, collapsed=ifelse(id=='main', F, T),
		fluidRow(column(3, offset=1, textOutput(ns('r1'))), column(3, textOutput(ns('r2')))),
		fluidRow(
			column(1,
				bsButton(ns("save"), label="", block=F, icon=icon("save"), size='small'),
				bsButton(ns("clear"), label="", block=F, icon=icon("bolt"), size='small')
				),
			column(3, 
					pickerInput(inputId=ns("id1"), multiple=T,
						choices=M$mt$milist, selected=isolate(xr$mselnames),
						options = pickerOptions(selectedTextFormat='static', width=250, liveSearch=T, size=10, title="Select Measures")
						)
				),
			column(3, 
					pickerInput(inputId=ns("id2"), multiple=T,
						choices=M$mt$dilist,selected=isolate(xr$dselnames),
						options = pickerOptions(selectedTextFormat='static',width=250, liveSearch=T, size=10, title="Select Dimensions")
							)
				),
			column(2,
				fluidRow(
					column(6, bsButton(ns("go"), label="GO",block=T, icon=icon("caret-square-right"), style='info',size='default')),
					column(6, radioButtons(inputId = ns("repdata"), label = NULL, choices=c('Report', 'Data'), inline=F))
					)
				),
			column(3,
					'Filters:', 
					uiOutput(ns('fd'))
				),
		bsTooltip(ns('save'), 'Save this chart into your folder'),
		bsTooltip(ns('clear'), 'Clear this chart and create a new one'),
		bsModal(ns("savemodal"), "Save Report", trigger=ns("save"), uiOutput(ns('savepopup')))
			)
		),
		fluidRow(
			uiOutput(ns('rd'))
      		)
		)
	}
