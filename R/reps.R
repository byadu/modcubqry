rr<- reactiveValues(repchanged=0, reportrow=0)
#' @import libcubolap
#' @export
#' @title reportscharts
#' @description Display saved reports
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param M is the meta data connection structure
#' @param D is the data connection structure
#' @param uid is the logged in user id
reportscharts<- function(input, output, M, D, uid) {
	observeEvent(input$del, ignoreInit=T, {
		delreport(M$mycfg, isolate(rg$g))
		rr$reportrow<- NULL
		rr$repchanged<<- isolate(rr$repchanged)+1
		})
	repf<- M$fold$repf
	if(!nrow(repf))
		return(NULL)

	for(i in 1:nrow(repf)) {
		local({
			my_i<- i
			rl<- replist(M$cfg, repf[my_i,1])
			for(j in 1:nrow(rl)) {
				local({
					my_j<- j
					repid<- as.character(rl[my_j,1])
					output[[repid]]<- renderUI({
						cfg<- M$cfg
						g<- setrepid(cfg, repid)
						gp<- g$gp
						rg$g<- g
	xr$dimgrp<- xgetdims(cfg, gp$measures)
	xr$mselids<- gp$measures
	xr$dselids<- gp$dims
	xr$dselnames<- getmnames(M$mt$dimensions, gp$dims)
	xr$mselnames<- getmnames(M$mt$measures, gp$measures)

	f<- g$f
	if(!is.null(f))
	for(i in 1:length(f)) {
		fitem<- f[[i]][[1]]
		fvals<- f[[i]][[2]]
		addfilter(fitem$fh_itemid, fvals, fitem$fh_excl)
		}

						if(!is.null(g)) {
							callModule(analyze, repid, M, D, uid)
							a<-	analyzeUI(repid, M)
							}
						a
						})
					})
				}
			})
		}

	ti<- list()
	k<- 1
	for(i in 1:nrow(repf)) {
		reps<- replist(M$cfg, repf[i,1])
		for(j in 1:nrow(reps)) {
			ti[[k]]<- tabItem(tabName=as.character(reps[j,1]), uiOutput(reps[j,1]))
			k<- k+1
			}
		}
	ti
	}

#' @export
#' @title repside
#' @description Display saved reports list (in sidebar)
#' @param M is the meta data connection structure
repside<- function(M) {
	rr$repchanged

	repf<- M$fold$repf
	if(!nrow(repf))
		return(NULL)
	mi<- list()
	mi[[1]]<- "Reports"
	mi[["tabName"]]<- "Reports"
	for(i in 1:nrow(repf)) {
		smi<- list()
		smi[1]<- repf[i,2]
		smi[["tabName"]]<- as.character(repf[i,1])
		reps<- replist(M$cfg, repf[i,1])
		for(j in 1:nrow(reps)) {
			smi[[j+2]]<- menuSubItem(reps[j,2], tabName=as.character(reps[j,1]))
			}
		mi[[2+i]]<- do.call(menuItem, smi)
		}
	mi
	}
