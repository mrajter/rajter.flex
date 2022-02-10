
#' Set up everything needed to start an analysis
#'
#' This function will do necessary steps to set up an environment for an analysis
#' Basically, it sets up a working directory, creates options variable in global environment, creates folders and downloads template file for officer package.
#'
#' You should really start with this
#'
#' @return environment setup and variables:
#' \itemize{
#'    \item working directory
#'    \item directories in working directory: Data, Pictures, Templates, Outputs
#'    \item downloaded file template.docx into Templates directory
#'    \item global variable \strong{doc} based on template.docx to be used with officer
#'    \item global variable \strong{r.flex.opts} as named list which is used in functions for options:
#'    \itemize{
#'        \item lang - language - defaults to "hr", can be changed to "en"
#'        \item d.p - decimal point type - defaults to ",", can be changed to "."
#'        \item lead.zero (boolean) - will the leading zero be shown in numbers. e.g. 0.05 or .05. Defaults to TRUE
#'        \item p.type - how do you show p values. Options are
#'        \itemize {
#'            \item "<>" - default - they will be shown as compared to critical points (<0.05, <0.01, <0.001)
#'            \item "exact" - shown as exact values
#'            \item "star" - shown as stars as compared to critical points (<0.05, 0.01, 0.001)
#'        }
#'        \item tab.caption (boolean) - should table captions be put in word file. Defaults to TRUE
#'    }
#' }
#' @export

im_on_fire<- function(){
  #set up working directory
  setwd(utils::choose.dir(default = "", caption = "Select folder for analysis"))

  # Create directories
  dir.create("Data")
  dir.create("Pictures")
  dir.create("Templates")
  dir.create("Outputs")

  #download template
  utils::download.file("https://github.com/mrajter/flex_support_files/raw/main/template.docx", "Templates/template.docx", mode="wb")

  #assign to officer variable
  assign("doc", officer::read_docx("Templates/template.docx"), envir = .GlobalEnv)
  #doc<<-officer::read_docx("Templates/template.docx")

  #create options variable
  assign("r.flex.opts",
         list(lang="hr", d.p=",", lead.zero=TRUE, p.type="<>", tab.caption=TRUE),
         envir = .GlobalEnv)
}



#' Set r.flex.opts variable
#'
#' Used as a direct way to set the options variable used in other functions. The preferred way is by using im_on_fire() function.
#' This should be used when you want to work without other perks of im_on_fire().
#'
#' @return
#' @export
set.r.flex.opts=function(){
  assign("r.flex.opts",
         list(lang="hr", d.p=",", lead.zero=TRUE, p.type="<>", tab.caption=TRUE),
         envir = .GlobalEnv)
}





