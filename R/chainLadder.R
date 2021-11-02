#' Classical chain-ladder model.
#'
#' This function implements the classic chain ladder method on a cumulative run-off triangle.
#'
#' @param triangle The run-off triangle. The rows are the reporting years (or occurrence years), the columns the development years.
#' @param is_cumulatif Logical. Is the input triangle a cumulative run-off triangle? If `FALSE`, the cumulutive run-off triangle is constructed using the incremental input run-off triangle.
#'
#' @export


chainLadder <- function(triangle, is_cumulatif = FALSE)
{
  if(is_cumulatif) {
    triangle.cum <- triangle
  } else {
    triangle.cum <- t(apply(triangle, 1, cumsum))
  }

  triangle.pred <- triangle.cum

  n <- ncol(triangle)

  for(i in 2:n)
  {
    f <- sum(triangle.cum[1:(n-i+1), i], na.rm = TRUE) / sum(triangle.cum[1:(n-i+1), i-1], na.rm = TRUE)
    if(is.nan(f) | f == 0) {
      f = 1
    }
    triangle.pred[(n-i+2):n, i] <- triangle.pred[(n-i+2):n, (i-1)] * f
  }

  if(is_cumulatif) {
    increment <- triangle.pred
  } else {
    increment <- t(apply(triangle.pred, 1, function(x){c(0, diff(x))}))
  }
  increment[triangle != 0] <- 0

  return(increment)
}

#' Special version of the chain-ladder model on the incremental run-off triangle.
#'
#' This function applies a special version of the chain-ladder model for an incremental run-off triangle.
#'
#' @param triangle The incremental run-off triangle.
#'
#' @export

chainLadder_open <- function(triangle) {
  n <- ncol(triangle)

  triangle.pred <- triangle

  for(i in 3:n)
  {
    f <- sum(triangle.pred[1:(n-i+2), i], na.rm = TRUE) / sum(triangle.pred[1:(n-i+2), i-1], na.rm = TRUE)
    if(is.nan(f) | f == 0) {
      f = 1
    }
    triangle.pred[(n-i+3):n, i] <- triangle.pred[(n-i+3):n, (i-1)] * f
  }

  triangle.pred[triangle != 0] <- 0

  triangle.pred
}
