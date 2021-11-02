#' Register a new update function in the hierarchical reserving model
#'
#' Register an update function that will be called in the simulation algorithm to update the covariates that are not directly modelled in the hierarchical reserving model.
#'
#' @param obj The hierarchical reserving model
#' @param update Function that takes as input a reserving data set and updates stochastic covariates that are not directly modelled in the hirem.
#' @param after Name of the layer after which this update function should be applied
#' @export
register_updater <- function(obj, update, after = 'end') {
  obj$updaters <- c(obj$updaters, update);
  obj$update_after <- c(obj$update_after, after);

  found = FALSE;
  if(after != 'end') {
    for(i in 1:length(obj$layers)) {
      if(obj$layers[[i]]$name == after) {
        found = TRUE;
        break;
      }
    }
    if(!found) {
      warning(paste('layer ', after, ' not found.\n', sep = ''))
    }
  }

  return(obj)
}

#' @export
simulate <- function(object, ...) {
  UseMethod("simulate")
}

#' @export
hirem_update <- function(obj, data, after) {

  select <- which(obj$update_after == after)
  for(i in select) {
    data <- obj$updater[[i]](data)
  }

  return(data)
}

#' @export
simulate.layer_glm <- function(obj, data, balance.correction, balance.var) {
  select <- obj$filter(data)

  response <- predict(obj$fit, newdata = data[select, ], type = 'response')

  if(balance.correction) {
    response <- response * obj$balance.correction[(data[select,])[[balance.var]]]
  }

  if(obj$method_options$family == 'binomial') {
    simulation <- runif(length(response)) < response
  } else if(obj$method_options$family == 'gaussian') {

    simulation <- rnorm(length(response), mean = response, sd = obj$sigma)

  } else if(obj$method_options$family == 'Gamma') {
    scale <- response / obj$shape
    simulation <- rgamma(length(response), scale = scale, shape = obj$shape)
  } else if(obj$method_options$family == 'poisson') {
    simulation <- rpois(length(response), lambda = response)
  }

  if(!is.null(obj$transformation)) {
    simulation <- obj$transformation$inverse_transform(simulation)
  }

  ret <- rep(0, nrow(data))
  ret[select] <- simulation

  return(ret)
}

#' @export
simulate.layer_gbm <- function(obj, data, balance.correction, balance.var) {
  select <- obj$filter(data)

  response <- predict(obj$fit, n.trees = obj$iter, newdata = data[select, ], type = 'response')

  if(balance.correction) {
    response <- response * obj$balance.correction[(data[select,])[[balance.var]]]
  }

  if(obj$method_options$distribution == 'bernoulli') {
    simulation <- runif(length(response)) < response
  } else if(obj$method_options$distribution == 'gaussian') {
    simulation <- rnorm(length(response), mean = response, sd = obj$sigma)
  } else if(obj$method_options$distribution == 'gamma') {
    simulation <- rgamma(length(response), scale = response / obj$shape, shape = obj$shape)
  }

  ret <- rep(0, nrow(data))
  ret[select] <- simulation

  return(ret)
}

#' Simulate the future development of claims
#'
#' Simulates multiple paths for the future development of each claim
#'
#' @param obj The hierarchical reserving model
#' @param nsim Number of paths to simulate for each claim
#' @param filter Function with \itemize{
#'     \item input: Data set with reserving data
#'     \item output: Subset of records for which the simulation should continue
#' }
#' @param data Last observed record for each claim. From these records onwards the future development is simulated.
#' @param balance.correction Logical. Apply a bias correction step: yes or no?
#'
#' @return A data set with the same structure as the input data set, containing multiple simulations for the future development of each claim.
#' @export


simulate.hirem <- function(obj, nsim, filter, data, balance.correction = FALSE) {
  last <- filter(hirem_update(obj, data, 'end'))

  # replicate nsim times to create all simulations simultaneously
  last <- last[rep(seq_len(nrow(last)), nsim), ]
  last <- data.frame(last, simulation = rep(1:nsim, each = nrow(last)/nsim))

  simulation <- c();
  while(nrow(last) > 0) {

    for(index in seq_along(obj$layers)) {

      layer <- obj$layers[[index]]

      last[, layer$name] <- simulate(layer, last, balance.correction, obj$balance.var)

      #select <- layer$filter(last)
      #last[, layer$name] <- 0; # reinitialize
      #last[select, layer$name] <- simulate(layer, last[select, ])

      last <- hirem_update(obj, last, layer$name)
    }

    simulation <- rbind(simulation, last);
    last <- filter(hirem_update(obj, last, 'end'))

  }

  return(simulation)
}
