#' Unlist strsplit
#'
#' @param ... Parameters to be forwarded to \code{\link{strsplit}}
#' @return Unlist strsplit
#' @examples
#' x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
#' #split x on the letter e
#' ulstrsplit(x, "e")
ulstrsplit <- function(...) unlist(strsplit(...))