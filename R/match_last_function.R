#' Match on last element
#'
#' @param x vector or \code{NULL}: the values to be matched. \code{\link{Long vectors}} are supported.
#' @param table vector or \code{NULL}: the values to be matched against. \code{\link{Long vectors}} are not supported.
#' @param nomatch the value to be returned in the case when no match is found. Note that it is coerced to \code{integer}.
#' @param incomparables a vector of values that cannot be matched. Any value in \code{x} matching a value in this vector is assigned the \code{nomatch} value. For historical reasons, \code{FALSE} is equivalent to \code{NULL}.
#' @return Performs a \code{\link{match}}, but on the last element matching, rather than the first.
#' @description \code{\link{match}} returns a vector of the positions of (first) matches of its first argument in its second.
#' \code{\link{match_last}} returns a vector of the positions of (last) matches of its first argument in its second.
#' 
match_last <- function(x, table, nomatch = NA_integer_, incomparables = NULL) (NROW(table)+1)-match(x, table[NROW(table):1], nomatch = nomatch, incomparables = incomparables)