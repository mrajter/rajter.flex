
#' The most simple frequencies
#'
#' @param vari variable name as string
#' @param data dataset
#' @param deci number of decimals for percentages (default=1)
#' @param heading Which heading should be used for title (default=2)
#'
#' @return Title and table
#'
freq.flex.rmd=function(vari, data, deci=1, heading=2){
  t=freq.flex(vari,data,deci)
  cat("## ", colnames(t$header$dataset)[1], "\n", sep="") #heading level shoul be implemented
  cat("\n")
  t
  cat("\n")
}

freq.flex=function(vari, data, deci=1) {
  t=as.data.frame(table(data[[vari]]))

  if (is.null(sjlabelled::get_label(data[[vari]]))==TRUE) {
    t.name=vari
    } else {
      t.name=sjlabelled::get_label(data[[vari]])
    }

  if (is.null(sjlabelled::get_labels(data[[vari]], values = "as.name"))==FALSE) {
    vl=sjlabelled::get_labels(data[[vari]], values = "as.name")
    t$Var1=lapply(t$Var1, function(x) {x=vl[x]})
  } else {
      t$Var1=lapply(t$Var1,as.character)
  }

  t$pct=lapply(t$Freq, function(x){x=format(round((x/sum(t$Freq, na.rm=TRUE))*100,deci), nsmall=deci)})

  t=rbind(t,c("Total",sum(t$Freq), 100))

  colnames(t)=list(t.name, "f", "%")
  t=freq.to.flex(t)
  return(t)
}

freq.to.flex=function(t){
  t=t %>% flextable::flextable() %>%
    flextable::align(align="center", part="header") %>%
    flextable::align(align="center", part="body") %>%
    flextable::align(j=1,align="left", part="body") %>%
    flextable::hline_top(border=officer::fp_border(color="black", width = 1), part="header") %>%
    flextable::hline_bottom(border=officer::fp_border(color="black", width = 1), part="header") %>%
    flextable::hline_bottom(border=officer::fp_border(color="black", width = 1), part="body")
  pretty_dims=flextable::dim_pretty(t)$widths
  for (i in 1:length(pretty_dims)){
    t=flextable::width(t, j=i,pretty_dims[i])
  }
  return(t)
}

#send to officer document
freq.flex.of=function(vari, data, mydoc, deci=1, heading=2){
  t=freq.flex(vari,data,deci)
  mydoc=officer::body_add(mydoc, colnames(t$header$dataset)[1], style = paste("heading ",heading,sep=""))
  mydoc=officer::body_add_par(mydoc,"")
  mydoc=flextable::body_add_flextable(mydoc, value = t)
  mydoc=officer::body_add_par(mydoc,"")
  mydoc=officer::body_add_par(mydoc,"")
  return(mydoc)
}




