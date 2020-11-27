#' @export
hirem_transformation <- function(name, formula, transform, inverse_transform, dtransform) {

  obj <- c();
  obj$transform <- transform;
  obj$inverse_transform <- inverse_transform;
  obj$name <- name;
  obj$formula <- formula;
  obj$dtransform <- dtransform;

  class(obj) <- 'hirem_transformation';

  return(obj)

}

#' @export
hirem_transformation_identity <- hirem_transformation('identity', function(x){return(x)}, function(x){ return(x) }, function(x) { return(x) }, function(x) {return(x*0+1)})

#' @export
hirem_transformation_log <- hirem_transformation('log',
                                             function(x){return(paste('log(', x, ')', sep = ''))},
                                             function(x) { return(log(x)) },
                                             function(x){ return(exp(x)) },
                                             function(x) {return(1/x)})

#' @export
hirem_transformation_log_neg <- hirem_transformation('log',
                                                 function(x){return(paste('log(', x, ')', sep = ''))},
                                                 function(x) { return(-log(x)) },
                                                 function(x){ return(exp(-x)) },
                                                 function(x) {return(1/x)})

#' @export
hirem_transformation_logit <- hirem_transformation('logit',
                                               function(x){return(paste('log(', x, '/(1-', x, '))', sep = ''))},
                                               function(x){ return(log(x / (1 - x))) },
                                               function(x){ return(exp(x)/(1+exp(x))) },
                                               function(x){1/(x * (1-x))})
