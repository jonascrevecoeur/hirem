#' @export
fit <- function(object, ...) {
  UseMethod("fit")
}

fit.layer_glm <- function(layer, obj, formula, training = FALSE, fold = NULL) {
  layer$formula <- formula

  data <- obj$data_training
  if(!training) {
    data <- obj$data_observed
  }

  if(!is.null(fold)) {
    data <- data %>% filter(cv_fold != fold)
  }

  if(layer$method_options$family == 'Gamma') {
    layer$fit <- glm(as.formula(formula), data[layer$filter(data), ], family = Gamma(link = log))

  } else {
    layer$fit <- glm(as.formula(formula),
                     data = data[layer$filter(data), ],
                     family = layer$method_options)
  }

  if(layer$method_options$family == 'gaussian') {
    layer$sigma <- sd(layer$fit$residuals)
  } else if(layer$method_options$family == 'Gamma') {
    shape <- hirem_gamma_shape(layer$fit)
    layer$shape <- shape$shape
    layer$shape.se <- shape$se
  }

  return(layer)
}

#' @export
fit.layer_gbm <- function(layer, obj, formula, training = FALSE, fold = NULL) {
  layer$formula <- formula

  data <- obj$data_training
  if(!training) {
    data <- obj$data_observed
  }

  if(!is.null(fold)) {
    data <- data %>% filter(cv_fold != fold)
  }

  data <- data[layer$filter(data), ]

  layer$fit <- gbm(as.formula(formula),
                   data = data,
                   distribution = layer$method_options$distribution,
                   n.trees = layer$method_options$n.trees,
                   cv.folds = layer$method_options$cv,
                   interaction.depth = layer$method_options$interaction.depth,
                   shrinkage = layer$method_options$shrinkage,
                   n.minobsinnode = layer$method_options$n.minobsinnode,
                   bag.fraction = layer$method_options$bag.fraction,
                   keep.data = TRUE)

  if(layer$method_options$select_trees == 'last') {
    layer$iter <- layer$method_options$n.trees
  } else {
    layer$iter <- gbm.perf(layer$fit, plot.it = FALSE)

    if(length(layer$iter) == 0) {
      layer$iter <- which.min(layer$fit$oobag.improve[is.finite(layer$fit$oobag.improve)])
    }
  }

  if(layer$method_options$distribution == 'gaussian') {
    layer$sigma <- sd(predict(layer$fit, n.trees = layer$iter, type = "response") - layer$fit$data$y)
  }

  if(layer$method_options$distribution == 'gamma') {
    shape <- gamma_fit_shape(layer$fit$data$y, predict(layer$fit, n.trees = layer$iter, type = "response"))
    layer$shape <- shape$shape
    layer$shape_sd <- shape$s.e.
  }

  return(layer)
}

#' Fitting layers in a hierarchical reserving model
#'
#' fit one or multiple layers of the hierarchical reserving model
#'
#' @param obj The hierarchical reserving model
#' @param training TRUE: Fit the layers on the training data set \cr
#'                 FALSE: Fit the layers on the observed data set
#' @param fold Index of the fold on which the model should be estimated \cr
#'             When \code{fold == NULL} the layer is estimated on all available records
#'  @param ... Add for each layer an argument with the same name as the layer and as value a formula describing the regression model
#'
#' @export
fit.hirem <- function(obj, training = FALSE, fold = NULL, ...) {
  formulas <- list(...)

  for(i in 1:length(formulas)) {
    index <- hirem_get_layer_pos(obj, names(formulas)[i])

    obj$layers[[index]] <- fit(obj$layers[[index]], obj, formulas[[i]], training, fold)
  }

  return(obj)
}

