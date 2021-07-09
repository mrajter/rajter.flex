#' Create comperimeter tables
#'
#' Function creates basic comperimeter flextable with optional descriptives. Variables must be labelled.
#'
#' @param form formula specified as 1 ~ var1 + var2 +varX
#' @param data dataset
#' @param param should parametrics (M and SD) be included (default=TRUE)
#' @param m.deci number of decimal points for parametrics (default=1)
#' @param p.deci number of decimal points for percentages (default=1)
#' @param lang language (defaut="hr")
#' @return data.frame with results
#' @export
#'

comp.flex=function(form, data, param=TRUE, m.deci=1, p.deci=1, lang="hr"){

  #collect variable names
  v.names=attr(stats::terms(form), which = "variables")
  v.names=as.character(v.names[3:length(v.names)])

  #collect value labels - the function will work only with labelled data
  if (is.null(sjlabelled::get_labels(data[[v.names[1]]], values = "as.name"))) {
    warning("Value labels should be defined for comperimeter table")} else {
    v.labs=data.frame(labs=names(attributes(data[[v.names[1]]])$labels), vals=unname(attributes(data[[v.names[1]]])$labels))
    }

  #radi? prvo za jednu varijablu pa tek onda za ostale
  #tri varijable - imena, deskriptiva, postotci


  r.names=data.frame(Name=sjlabelled::get_label(data[[v.names[1]]]), N=sum(is.na(data[[v.names[1]]])==FALSE))
  if (param==TRUE) {
    r.desc=data.frame(
      M=mean(data[[v.names[1]]], na.rm=TRUE),
      SD=stats::sd(data[[v.names[1]]], na.rm=TRUE))
  }

  fre.t=c()
  for (i in 1:nrow(v.labs)) {
    fre.t=c(fre.t,length(data[[v.names[1]]][data[[v.names[1]]]==v.labs$vals[i]])*100/r.names$N[1])
  }
  r.fre=data.frame(t(fre.t))
  names(r.fre)=v.labs$labs

  if (length(v.names)>1) {
    #provjeri jesu li labeli isti
    res=1
    for (i in 2:length(v.names)){
      test.labs=v.labs2=data.frame(labs=names(attributes(data[[v.names[1]]])$labels), vals=unname(attributes(data[[v.names[1]]])$labels))
      if (all(v.labs==test.labs)) {res=1} else {res=0}
    }
    if (res==0) {
      warning("Variables in comperimeter tables should have same values and value labels")
    }

    #imena
    for (i in 2:length(v.names)){
    r.names=rbind(r.names, list(sjlabelled::get_label(data[[v.names[i]]]), sum(is.na(data[[v.names[i]]])==FALSE)))

    #parametrija
    if (param==TRUE) {
      r.desc=rbind(r.desc, c(mean(data[[v.names[i]]], na.rm=TRUE),stats::sd(data[[v.names[i]]], na.rm=TRUE)))
    }


    #freqs
    fre.t=c()
    for (j in 1:nrow(v.labs)) {
      fre.t=c(fre.t,length(data[[v.names[i]]][data[[v.names[i]]]==v.labs$vals[j]])*100/r.names$N[i])
    }
    r.fre=rbind(r.fre,fre.t)

    }

  }


  #format cells and create data.frame
  if (param==TRUE) {
    r.desc$M=format(round(r.desc$M,m.deci), nsmall=m.deci)
    r.desc$SD=format(round(r.desc$SD,m.deci+1), nsmall=m.deci+1)
    r.fre=format(round(r.fre,p.deci), nsmall=p.deci)
    res.df=cbind(r.names, r.desc, r.fre)
  } else {
    r.fre=format(round(r.fre,p.deci), nsmall=p.deci)
    res.df=cbind(r.names, r.fre)
  }

  if (lang=="hr") {names(res.df)[1]="Varijabla"} else {names(res.df)[1]="Variable"}

  res.df=comp.to.flex(res.df, param = param)
  return(res.df)
}

comp.to.flex=function(df, param){

  #definicije za stupce
  if (param==TRUE) {
    col.names=c(names(df)[1],names(df)[2],names(df)[3],names(df)[4])
    for (i in 5:ncol(df)) {col.names=c(col.names, "%")}
  } else {
    col.names=c(names(df)[1], names(df)[2])
    for (i in 3:ncol(df)) {col.names=c(col.names, "%")}
  }

  #tablica
  df=df %>% flextable::flextable() %>%
    flextable::hline_top(border=officer::fp_border(color="black", width = 1), part="header") %>%
    flextable::hline_bottom(border=officer::fp_border(color="black", width = 1), part="header") %>%
    flextable::hline_bottom(border=officer::fp_border(color="black", width = 1), part="body") %>%
    flextable::add_header_row(top=FALSE, values=col.names) %>%
    flextable::hline_bottom(border=officer::fp_border(color="black", width = 1), part="header") %>%
    flextable::merge_v(part="header") %>%
    flextable::fix_border_issues() %>%
    flextable::align(align="center", part="header") %>%
    flextable::align(align="center", part="body") %>%
    flextable::align(j=1,align="left", part="body")

  df
}




