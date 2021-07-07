#' Provide basic descriptives
#'
#' Simple descriptive statistics
#'
#' @param data data.frame with variables
#' @param vars variables as list as strings
#' @param param_set list of parameters with order in the table (as strings):
#' \itemize{
#'   \item default - "standard"
#'   \item "simple" - N, Min, Max, M, SD
#'   \item "standard" - N, M, SD, Min, Q1, C, Q3, Max, Skew, Kurt, SW, SWp
#'   \item other available statistics - missing valuse(Miss), Kolmogorov-Smirnov (KS, KSp), standard errors for Skewness and Kurtosis (SEskew, SEkurt)
#' }
#' @param by section variable, default=FALSE
#' @param deci number of decimals for M, everything else is done automatically (default=1)
#' @param lang language (default is "hr")
#'
#' @return data.frame with results
#' @export
#'
des.flex<-function(data, vars, param_set="standard", by=FALSE, deci=1, lang="hr"){
  if (missing(vars)) {vars=c(unname(labelled::var_label(data)))}

  #empty list for results
  res.names=c()
  res.N=c()
  res.Miss=c()
  res.min=c()
  res.max=c()
  res.M=c()
  res.SD=c()
  res.C=c()
  res.Q1=c()
  res.Q3=c()
  res.Var=c()
  res.Skew=c()
  res.SE_skew=c()
  res.Kurt=c()
  res.SE_kurt=c()
  res.KS=c()
  res.KSp=c()
  res.SW=c()
  res.SWp=c()

  for (i in vars) {
    # names
    if (is.null(labelled::var_label(data[[i]]))) {
      res.names=append(res.names, i)} #if not labelled use variable name
      else {
        res.names=append(res.names, labelled::var_label(data[[i]]))
      }

    # N
    res.N=append(res.N,length(stats::na.omit(data[[i]])))

    #Miss
    res.Miss=append(res.Miss,sum(is.na(data[[i]])))

    #Min
    res.min=append(res.min, min(data[[i]], na.rm=TRUE))

    #Max
    res.max=append(res.max, max(data[[i]], na.rm=TRUE))

    #M
    res.M=append(res.M, mean(data[[i]], na.rm=TRUE))

    #SD
    res.SD=append(res.SD, stats::sd(data[[i]], na.rm=TRUE))

    #C
    res.C=append(res.C, stats::median(data[[i]], na.rm=TRUE))

    #Q1
    res.Q1=append(res.Q1, unname(stats::quantile(data[[i]], na.rm=TRUE)[2]))

    #Q3
    res.Q3=append(res.Q3, unname(stats::quantile(data[[i]], na.rm=TRUE)[4]))

    #Variance
    res.Var=append(res.Var, stats::var(data[[i]], na.rm=TRUE))

    #skew
    res.Skew=append(res.Skew, r.skewness(data[[i]]))

    #skew SE
    res.SE_skew=append(res.SE_skew, r.skewness.se(data[[i]]))

    #kurt
    res.Kurt=append(res.Kurt, r.kurtosis(data[[i]]))

    #kurt SE
    res.SE_kurt=append(res.SE_kurt, r.kurtosis.se(data[[i]]))

    #KS test
    ks=r.ks(data[[i]])
    res.KS=append(res.KS, unname(ks$statistic))
    res.KSp=append(res.KSp, unname(ks$p.value))

    #SW test
    sw=r.sw(data[[i]])
    res.SW=append(res.SW, unname(sw$statistic))
    res.SWp=append(res.SWp, unname(sw$p.value))
  }


  ##decimals
  #res.min=c()
  #res.max=c()
  res.M=format(round(res.M,deci),nsmall=deci)
  res.SD=format(round(res.SD,(deci+1)),nsmall=(deci+1))
  res.C=format(round(res.C,deci),nsmall=deci)
  res.Q1=format(round(res.Q1,deci),nsmall=deci)
  res.Q3=format(round(res.Q3,deci),nsmall=deci)
  res.Var=format(round(res.Var,(deci+1)),nsmall=(deci+1))
  res.Skew=format(round(res.Skew,2),nsmall=2)
  res.SE_skew=format(round(res.SE_skew,2),nsmall=2)
  res.Kurt=format(round(res.Kurt,2),nsmall=2)
  res.SE_kurt=format(round(res.SE_kurt,2),nsmall=2)
  res.KS=format(round(res.KS,2),nsmall=2)
  res.KSp=format(round(res.KSp,3),nsmall=3)
  res.SW=format(round(res.SW,2),nsmall=2)
  res.SWp=format(round(res.SWp,3),nsmall=3)







  res=data.frame(res.names,res.N, res.Miss, res.min, res.max, res.M, res.SD,
                   res.C, res.Q1, res.Q3, res.Skew, res.SE_skew, res.Kurt, res.SE_kurt,
                   res.KS,res.KSp, res.SW, res.SWp)




  names(res)=c("Variable", "N", "Miss","Min", "Max", "M","SD","C","Q1","Q3",
               "Skew","SE_skew", "Kurt", "SE_kurt","KS", "KSp", "SW", "SWp")
  if (param_set=="simple"){
    res=res %>% dplyr::select("Variable", "N", "Min", "Max", "M", "SD")
  } else if (param_set=="standard") {
    res=res %>% dplyr::select("Variable", "N", "M", "SD", "Min", "Q1", "C","Q3", "Max", "Skew", "Kurt", "SW", "SWp")

  } else if (param_set=="full"){

  } else {
    res=res %>% dplyr::select(append("Variable", param_set))
  }

  if (lang=="hr") {names(res)[1]="Varijabla"} else {names(res)[1]="Variable"}

  return(des.to.flex(res))

}






# Skewness ----------------------------------------------------------------


#' Calculates skewness of the variable
#'
#' uses Kirk (2008) method for adjusted Fisher-Pearson standardized moment coefficient.
#'
#' This is the same method Excel uses when calculating skewness.
#' @param target_variable numeric variable
#' @return variable skewness
#' @export
r.skewness=function(target_variable){
  a=scale(target_variable)
  a=a^3

  n=length(stats::na.omit(a))
  skew=sum(a,na.rm=TRUE)/(n-1)
  skew=skew*(n/(n-2))

  return(skew)

}



# Skewness standard error -------------------------------------------------

#' Calculates standard error for the skewness of the variable
#'
#' uses SPSS book of algorithms
#'
#' @param target_variable numeric variable
#' @return standard error for variable skewness
#' @export

r.skewness.se=function(target_variable) {
  n=length(stats::na.omit(target_variable))
  se.skew=sqrt( (6*n*(n-1) )/( (n-2) * (n+1) * (n+3) ) )
  return(se.skew)
}



# Kurtosis ----------------------------------------------------------------

#' Calculates excess kurtosis of the variable
#'
#' uses SPSS Book of algorithms v20
#'
#' @param target_variable numeric variable
#' @return excess kurtosis
#' @export

r.kurtosis=function(target_variable){
  n=length(stats::na.omit(target_variable))
  a=target_variable-mean(target_variable, na.rm=TRUE)
  a2=a^2
  a4=a^4

  m2=sum(a2, na.rm = TRUE)
  m4=sum(a4, na.rm = TRUE)

  up_div=(n*(n+1)*m4)-(3*m2*m2*(n-1))
  down_div=(n-1)*(n-2)*(n-3)*(stats::sd(target_variable, na.rm=TRUE)^4)

  kurt=up_div/down_div
  return(kurt)
}



# Kurtosis SE -------------------------------------------------------------

#' Calculates standard error for excess kurtosis of the variable
#'
#' uses SPSS Book of algorithms v20
#'
#' @param target_variable numeric variable
#' @return SE for excess kurtosis
#' @export

r.kurtosis.se=function(target_variable){
  n=length(stats::na.omit(target_variable))
  up_div=4*(n^2-1)*r.skewness.se(target_variable)^2
  down_div=(n-3)*(n+5)
  kurt_se=sqrt(up_div/down_div)
  return(kurt_se)
}



# Kolmogorov Smirnov ------------------------------------------------------

#' Calculates sKolmogorov-Smirnov test with Lilliefors correction
#'
#' uses nortest package
#'
#' @param target_variable numeric variable
#' @return list with statistic and p value
#' @export

r.ks=function(target_variable){
  res=nortest::lillie.test(target_variable)
  ks=list(unname(res$statistic), unname(res$p.value))
  names(ks)=c("statistic", "p.value")
  return(ks)
}



# Shapiro Wilk ------------------------------------------------------------

#' Calculates Shapiro-Wilk test
#'
#' The maximum data length is 5000 points by definition. If the sample size is larger than 5000 (without NAs), function selects a random sample with 5000 cases and issues a warning.
#'
#' @param target_variable numeric variable
#' @return list with statistic and p value
#' @export

r.sw=function(target_variable) {
  if (length(stats::na.omit(target_variable))>5000){
    a=sample(target_variable, 5000)
    warning("Number of cases is larger than 5000. Test is calculated based on random sample with 5000 cases")
  } else{
    a=target_variable
  }
  res=stats::shapiro.test(a)
  sw=list(unname(res$statistic), unname(res$p.value))
  names(sw)=c("statistic", "p.value")
  return(sw)

}

des.to.flex=function(res){
  res=res %>% flextable::flextable() %>%
    flextable::hline_top(border=officer::fp_border(color="black", width = 1), part="header") %>%
    flextable::hline_bottom(border=officer::fp_border(color="black", width = 1), part="header") %>%
    flextable::hline_bottom(border=officer::fp_border(color="black", width = 1), part="body") %>%
    flextable::align(align="center", part="header") %>%
    flextable::align(align="center", part="body") %>%
    flextable::align(j=1,align="left", part="body")
}
