#' Groups and aggregates the portfolio by e.g. reporting year and development year since reporting
#'
#' @param data The portfolio of claims. The portfolio should have one record per development year for each claim.
#' @param group.var1 Character. Name of the first grouping variable.
#' @param group.var2 Character. Name of the second grouping variable.
#' @param value The name of the variable that will be aggregated over the two grouping variables.
#' @param cumulative Logical. Should the cumulative run-off triangle be returned?
#'
#' @import dplyr
#'
#' @export

construct_triangle <- function(data, group.var1, group.var2, value,  cumulative = FALSE) {
  coln <- colnames(data)
  if(!(group.var1 %in% coln) | !(group.var2 %in% coln) | !(value %in% coln))
    warning("One of the 'grouping' variables or the 'value' variable is incorrectly specified.")

  df_triangle <- data %>%
    group_by(get(group.var1), get(group.var2)) %>%
    dplyr::summarise(total = sum(get(value)))
  colnames(df_triangle) <- c(group.var1, group.var2, value)

  triangle <- matrix(NA, nrow = max(df_triangle[[group.var1]]), ncol = max(df_triangle[[group.var2]]))
  triangle[cbind(df_triangle[[group.var1]], df_triangle[[group.var2]])] <- df_triangle[[value]]

  if(cumulative){
    triangle_list <- lapply(1:nrow(triangle), function(x) cumsum(na.omit(triangle[x,])))
    triangle <- do.call(rbind, lapply(triangle_list,
                                      function(x) c(x, rep(NA, nrow(triangle) - length(is.na(x))))))
  }

  triangle
}
