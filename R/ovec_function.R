#' Crossed character vectors
#'
#' @param ... A set of parameters that can be coerced by as.character.
#' @param FUN A function to combine parameters. Defaults to \code{function(a, b) paste0(a, b)}
#' @return Character vector containing all combinations of provided parameters
#' @description Function which uses \code{\link{outer}} and provided function FUN to generate a character vector of all possible combinations of
#' the provided parameters, each coerced by as.character
#' @keywords outer, vector, character
#' @examples
#' ovec(c("x", "y", "z"), 1:3)
#' # c("x1", "x2", "x3", "y1", "y2", "y3", "z1", "z2", "z3")

ovec <- function(..., FUN = function(a, b) paste0(a, b)) {
  args <- list(...)
  larg <- length(args)
  for(narg in 1:larg) args[[narg]] <- as.character(args[[narg]])
  if(larg < 2) return(unlist(args))
  else return(as.vector(outer(args[[1L]], do.call(ovec, c(args = args[-1L], FUN = FUN )), FUN = FUN)))
}
