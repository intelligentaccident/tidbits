#' Function for generating a character vector from crossing vectors, separated by sep
#'
#' @param ... A set of parameters that can be interpreted as character vectors.
#' @param FUN A function to combine parameters. Defaults to function(a, b) paste0(a, b)
#' @keywords outer, vector, character
#' @examples
#' ovec(c("x", "y", "z"), 5:10)

ovec <- function(..., FUN = function(a, b) paste0(a, b)) {
  args <- list(...)
  larg <- length(args)
  for(narg in 1:larg) args[[narg]] <- as.character(args[[narg]])
  if(larg < 2) return(unlist(args))
  else return(as.vector(outer(args[[1L]], do.call(ovec, c(args = args[-1L], FUN = FUN )), FUN = FUN)))
}
