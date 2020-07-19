#' @export
#' @title dashboardUI
#' @description UI to display a dashboard
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param dashid is the folder id of reports
#' @param M is the meta data connection structure
dashboard<- function(input, output, session, dashid, M) {
	ns<- session$ns

	print('dash')
	rl<- replist(M$cfg, dashid)
	print(rl)
	for(j in 1:nrow(rl)) {
		local({
			my_j<- j
			repid<- rl[my_j,1]
			output[[repid]]<- renderUI({
				g<- setrepid(M$cfg, repid)
				if(!is.null(g)) {
					callModule(chart, repid, g, noopt=1)
					chartUI(ns(repid), g, noopt=1)
					}
				})
			})
		}
	}

#' @export
#' @title dashboardUI
#' @description UI to display a dashboard
#' @param id is caller id
#' @param dashid is the folder id of reports
#' @param dashname is the name of the folder 
#' @param M is the meta data connection structure
dashboardUI<- function(id, dashid, dashname, M) {
	ns<- NS(id)

	print('dashui')
	rl<- replist(M$cfg, dashid)

	htmlfile<- paste0("templs/", dashname, ".html")
	if(file.exists(htmlfile)) {
		ui<-paste0('htmlTemplate("', htmlfile, '",')
		for(j in 1:nrow(rl)) {
			repid<- rl[j,1]
			a=paste0('R', j, '=', 'uiOutput(ns(', repid, '))')
			if(j > 1)
				ui<- paste(ui, ',')
			ui<- paste(ui,a)
			}
		ui<- paste(ui, ')')
		eval(parse(text=ui))
		}

	else {			# draw nx2 grid 
		p1<- ""
		fr<- 0
		frow<- list()

		rl<- replist(M$cfg, dashid)
		for(j in 1:nrow(rl)) {
			repid<- rl[j,1]
			reptitle<- rl[j,2]
			p<- uiOutput(ns(repid))
	
			if(j %% 2)
				p1<- p
			else {
				fr<- fr+1
				frow[[fr]]<-fluidRow(column(8,p1), column(4, p))
				}
			}
		if(j%%2) {
			fr<- fr+1
			frow[[fr]]<- fluidRow(column(12,p1))
			}

	print('end dashui')
		a<-do.call(fluidPage, frow)
		a
		}
	}

#' @export
#' @title reportscharts
#' @description Display saved reports
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param M is the meta data connection structure
dashboards<- function(input, output, session, M) {
	ns<- session$ns

	repf<- M$fold$repf
	if(!nrow(repf))
		return(NULL)

	for(i in 1:nrow(repf)) {
		local({
			my_i<- i
			id<- as.character(repf[my_i,1])
			name<- repf[my_i,2]
			output[[id]]<- renderUI({
				callModule(dashboard, id, id, M)
				dashboardUI(ns(id), id, name, M)
				})
			})
		}

	ti<- list()
	for(i in 1:nrow(repf)) {
		id<- as.character(repf[i,1])
		name<- repf[i,2]
		ti[[i]]<- tabItem(tabName=id, uiOutput(id))
		}
	ti
	}

#' @export
#' @title dashside
#' @description Display list of reports folders (dashboards)
#' @param M is the meta data connection structure
dashside<- function(M) {
	repf<- M$fold$repf
	if(!nrow(repf))
		return(NULL)
	mi<- list()
	mi[[1]]<- "Dashboards"
	mi[["tabName"]]<- "Dashboards"
	for(i in 1:nrow(repf))
		mi[[2+i]]<- menuSubItem(repf[i,2],tabName=as.character(repf[i,1]))
	mi
	}
