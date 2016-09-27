#' Crossed character vectors
#'
#' @param ... A set of parameters that can be coerced by as.character.
#' @param FUN A function to combine parameters. Defaults to \code{function(...) paste(..., collapse = "")}
#' @return Character vector containing all combinations of provided parameters
#' @description Returns a char vector containing all combinations of provided vectors, each coerced by as.character.
#' @keywords outer, vector, character
#' @examples
#' ovec(c("x", "y", "z"), 1:3)
#' # c("x1", "x2", "x3", "y1", "y2", "y3", "z1", "z2", "z3")
#' 

cross_vec <- function(..., FUN = function(...) paste(..., collapse = "")) {
  args <- list(...)
  larg <- length(args)
  for(narg in 1:larg) args[[narg]] <- as.character(args[[narg]])
  apply(do.call(expand.grid, list(...)), MARGIN = 1, FUN = FUN)
}