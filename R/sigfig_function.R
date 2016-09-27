#' Only selected number of digits
#'
#' @param x numeric vector.
#' @param digits number of digits to return for each number. 
#' @return returns a vector of same length as x, with number of digits = \code{digits}

sigfig <- function(x, digits = 3){
  return(gsub("\\.$", "", formatC(signif(x,digits=digits), digits=digits, format="fg", flag="#")))
}