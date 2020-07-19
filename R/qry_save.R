saveobservers<-0
#' @export
#' @title savereport
#' @description save the query as a report
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param g is graph structure to be saved as a report in database
#' @param currfilt contains the filters applied for this report
#' @param M is the meta data connection structure
#' @param uid is the logged in user id
savereport<- function(input, output, session, g, M, uid, currfilt) {
	ns<- session$ns

	if(!saveobservers) {
	observeEvent(input$saverep, ignoreInit=T,{
		repname<- isolate(input$repname)
		foldid<- isolate(input$parfold)
		if(is.na(as.numeric(foldid)))
			foldid<- addfolder(foldid, uid, M)
		else
			foldid<- as.numeric(foldid)
		g$gp$gtype<- isolate(cr$gtype)
		gfid<- addgraph(repname, foldid, g, currfilt, M)
		rr$repchanged<- isolate(rr$repchanged)+1
		createAlert(session, ns("saved"), ns("alsaved"), title="", content=paste("Report", strong(repname), "saved in folder", strong(foldername(M$cfg, foldid))))
		})
	saveobservers<<- 1
	}
	}

#' @export
#' @title savereportUI
#' @description UI to save the query as a report
#' @param id is the caller id
#' @param ft is the folder to save the report in
savereportUI<- function(id, ft) {
	ns<- NS(id)

	fluidPage(
		fluidRow(
		column(3, selectizeInput(ns("parfold"), "Folder", ft, selected='', multiple=F, options=list(create=T, placeholder='Select or Add'))),
		column(5, textInput(ns("repname"), "", placeholder="Your report name")),
		column(2, HTML("<br>"), actionButton(ns("saverep"), "Save",icon("save")))
		),
		fluidRow(column(10, offset=1,bsAlert(ns("saved"))))
		)
	}
