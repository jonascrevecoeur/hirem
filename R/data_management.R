#' Splits the data in a hierarchical reserving model into a training, validation and test data set
#'
#' @param obj The hierarchical reserving model
#' @param observed This input can be \itemize{
#'   \item A data set to be used for training and validation
#'   \item A function that extracts the observed data from the full data set
#' }
#' @param validation This input can be \itemize{
#'   \item A data set to be used for validation
#'   \item A function that extracts the validation data set from the observed data set
#'   \item A number between zero and one, which expresses the fraction of the data that will be used for validation
#' }
#' @param cv_fold Number of folds to be used in cross validation
#' @export
split_data <- function(obj, observed = NULL, validation = NULL, cv_fold = NULL) {

  if(!is.null(observed)) {
    if(class(observed) == "function") {
      obj$data_observed <- observed(obj$data)
    } else {
      obj$data_observed <- observed
    }
  }

  if(!is.null(cv_fold)) {
    fold <- ceiling(runif(nrow(obj$data_observed)) * cv_fold)
    obj$data_observed <- cbind(obj$data_observed, cv_fold = fold);
  }

  if(!is.null(validation)) {
    if(class(validation) == "numeric") {
      subset <- runif(nrow(obj$data_observed)) < fraction
      obj$data_validation <- obj$data_observed[subset, ]
    } else if(class(validation) == "function") {
      obj$data_validation <- validation(obj$data_observed)
    } else {
      obj$data_validation <- validation
    }

    obj$data_training <- fsetdiff(obj$data_observed, obj$data_validation, all = TRUE)
  } else {
    obj$data_training <- obj$data_observed
  }

  return(obj)
}

