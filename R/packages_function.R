#' Shortcut to install packages
#'
#' @param x name of package to be installed/loaded
#' @return Returns nothing.
#' @description Loads package x if it exists, otherwise tries to download x.
#' @keywords install.packages, library
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}