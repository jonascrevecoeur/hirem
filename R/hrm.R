#' Initiates the construction of a hierarchical reserving model composed of a sequence of layers
#'
#' @param data The input data for constructing, evaluating and testing the hierarchical reserving model
#' @export
hirem <- function(data) {
  obj <- c();
  obj$data <- data;
  obj$data_observed <- data;
  obj$data_training <- data;

  obj$layers <- list();

  obj$updaters <- c();
  obj$update_after <- c();

  class(obj) <- 'hirem';

  return(obj)
}

#' @export
print.hirem <- function(obj, ...) {
  cat('Hierarchical reserving model\n\n')
  cat('Data:\n----------\n')
  cat('Observed:', nrow(obj$data_observed), 'records\n')
  if(!is.null(obj$data_validation)) {
    cat('- Training: ', nrow(obj$data_training), 'records\n')
    cat('- Validation: ', nrow(obj$data_validation), 'records\n')
  }
  if(!is.null(obj$data_test)) {
    cat('Test:', nrow(obj$data_test), 'records\n')
  }
  cat('\nLayers (', length(obj$layers), '):\n----------\n', sep='')
  for(i in seq_along(obj$layers)) {
    cat('* ');
    print(obj$layers[[i]])
  }
}
