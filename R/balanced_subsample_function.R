#' Create random balanced subsamples
#'
#' @param df data.frame
#' @param unique_vector name of vector of unique instances
#' @param existing_groupings vector of column names describing groups over which to balance sampling.
#' @param newgroups vector of values for new groups
#' @param new_group_name name for variable with new groups

balanced_subsample <- function(df, unique_vector, existing_groupings = NULL, newgroups, new_group_name = "new_group") {
  
  tmpgrps <- unique_vector
  if(!is.null(existing_groupings)) tmpgrps <- c(tmpgrps, existing_groupings)
  dfu <- data.frame(unique(df[, tmpgrps, drop = FALSE]))
  dfu <- dfu[do.call(order, data.frame(dfu[, existing_groupings], random_order = sample(nrow(dfu)))),, drop = FALSE]
  dfu$newgrps <- rep(newgroups, (nrow(dfu)%/%length(newgroups) + 1))[1:nrow(dfu)]
  df[, new_group_name] <- dfu[match(interaction(df[,tmpgrps]), interaction(dfu[,tmpgrps])), "newgrps"]
  return(df)
}