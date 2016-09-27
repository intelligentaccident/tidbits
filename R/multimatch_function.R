#' Match between rows of data.frames
#'
#' @param dfx data.frame with rows to be matched
#' @param dftable data.frame with rows to be matched against
#' @return \code{multimatch} returns a vector of position of (first) matches of its first argument in its second.
#' @return \code{multin} corresponds to \code{\link{\%in\%}} 
#' @return \code{multwhich} corresponds to \code{\link{which}}
#' @usage 
#' @aliases multin, multwhich
#' @description Returns a match performed at the level of rows in data.frames.

multimatch <- function(dfx, dftable) {
  if(ncol(dfx) != ncol(dftable)) stop("Data.frames must have same number of columns.")
  return((match(interaction(as.list(dfx)), interaction(as.list(dftable)))))
}

multin <- function(dfx, dftable) !is.na(multimatch(dfx, dftable))

multwhich <- function(dfx, dftable) which(multin(dfx, dftable))