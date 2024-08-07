
#' Correlation matrix from data.frame
#'
#' Creates correlation matrix from data.frame based on Hmisc::rcorr
#'
#' @param df  data.frame
#' @param na how will missing values be handled ("listwise" (default), "pairwise")
#' @param type type of correlation ("pearson" (default), "spearman")
#' @param labs type of table:
#' \itemize{
#'   \item "numbered" - numbered variable labels an numbers as column headings
#'   \item "asis" - variable labels in rows and column headings
#'   \item "names" - variable names in rows and column headings
#' }
#' @param diag table looks ("bottom" (default), "top", "all")
#' @param lang language ("hr" (default), "en")
#'
#' @return list with two elements:
#' \itemize{
#'   \item $matrix - flextable object
#'   \item $N - number of participants (range if na="pairwise")
#' }
#' @export
#'

cor.flex=function(df, na="listwise", type="pearson", labs="numbered", diag="bottom", lang="hr"){
  if (na=="listwise") {df=df[stats::complete.cases(df),]}

  q=Hmisc::rcorr(as.matrix(df), type=type)

  q.c=data.frame(q[[1]])
  q.c=format(round(q.c,3), nsmall=3)

  q.p=data.frame(q[[3]])


  q.c[q.c == "1.000"] <- "-"

  #adding stars
  for (i in 1:ncol(q.c)){
    for (j in 1:nrow(q.c)) {
      if (j!=i) {
        if (q.p[j,i]<0.001) {q.c[j,i]=paste(q.c[j,i],"*", sep="")}
        if (q.p[j,i]<0.01) {q.c[j,i]=paste(q.c[j,i],"*", sep="")}
        if (q.p[j,i]<0.05) {q.c[j,i]=paste(q.c[j,i],"*", sep="")}

        #diagonals
        if (diag=="top") {if (j>i) {q.c[j,i]=""}}
        if (diag=="bottom") {if (j<i) {q.c[j,i]=""}}

      }
    }
  }

  #labels
  q.nam=data.frame(names(df))
  q.nam$labs=unlist(lapply(q.nam[,1], FUN=function(x){
    if (is.null(sjlabelled::get_label(df[[x]]))==TRUE){
      return(x)
    } else {
      return(sjlabelled::get_label(df[[x]]))
    }
  }))

  #labels variants
  if (labs=="asis"){
    q.c=cbind(q.nam[[2]],q.c)
    if (lang=="hr") {names(q.c)[1]="Varijabla"} else {names(q.c)[1]="Variable"}
  }

  if (labs=="names"){
    q.c=cbind(q.nam[[1]],q.c)
    if (lang=="hr") {names(q.c)[1]="Varijabla"} else {names(q.c)[1]="Variable"}
  }

  if (labs=="numbered"){
    q.c=cbind(seq(1,nrow(q.c)), q.nam[[2]], q.c)
    if (lang=="hr") {
      names(q.c)[1]="Br."
      names(q.c)[2]="Varijabla"
    } else {
      names(q.c)[1]="No."
      names(q.c)[2]="Variable"
    }
    names(q.c)[3:ncol(q.c)]=seq(1,nrow(q.c))
  }

  # if diagonal is not top then the last column can be removed
  if (diag!="top") {
    q.c=q.c[,1:(ncol(q.c)-1)]
  }

  #flextable

  q.c=q.c %>% flextable::flextable() %>%
    flextable::align(align="center", part="header") %>%
    flextable::align(align="center", part="body")
  if (labs=="numbered") {
    q.c = q.c %>% flextable::align(j=2,align="left", part="body")
  } else {
    q.c = q.c %>% flextable::align(j=1,align="left", part="body")
  }
  q.c = q.c %>%
    flextable::hline_top(border=officer::fp_border(color="black", width = 1), part="header") %>%
    flextable::hline_bottom(border=officer::fp_border(color="black", width = 1), part="header") %>%
    flextable::hline_bottom(border=officer::fp_border(color="black", width = 1), part="body")
  pretty_dims=flextable::dim_pretty(q.c)$widths
  for (i in 1:length(pretty_dims)){
    q.c=flextable::width(q.c, j=i,pretty_dims[i])
  }

  # N
  if (na=="listwise") {n=min(q[2]$n)} else {n=paste(min(q[2]$n), "-", max(q[2]$n))}


  #kako pišeš rezultate!!!!
  res=list(matrix=q.c, N=n )

  return(res)

}


