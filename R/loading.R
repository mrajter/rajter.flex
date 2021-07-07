#' Load dataset from SPSS
#'
#' Function enables loading from SPSS by using haven's method
#'
#' @param path path to dataset (default = file.choose())
#' @return loaded data.frame
#' @export
#'
load.SPSS<-function(path="file.choose()"){
  if (path=="file.choose()"){
    baza<-haven::read_sav(file.choose())
  } else {
    baza<-haven::read_sav(path)
  }
  return(baza)

}
