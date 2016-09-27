as.call <- function(x) UseMethod("as.call")
as.call.default <- .Primitive("as.call")
as.call.character <- function(x) formula(paste0("~", x))[[2]]