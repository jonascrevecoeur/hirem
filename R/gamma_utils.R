#' @export
hirem_gamma_shape <- function(fit)
{
  likelihood <- function(k)
  {
    scale <- fit$fitted.values / k
    -sum(dgamma(fit$y, shape = k, scale = scale, log = TRUE))
  }

  start <- 1 / (sum(fit$residuals^2) / (length(fit$fitted.values) - fit$rank))

  fit.nlm <- nlm(likelihood, start, hessian = TRUE)
  return(list(shape = fit.nlm$estimate, se = 1/sqrt(fit.nlm$hessian)))
}

#' @export
get_family_gamlss <- function(family_gam) {
  if(family_gam == "gaussian") {
    return("NO");
  }
}

#' @export
safe_multiplication <- function(x, y) {
  z <- x * y
  z[x == 0 | y == 0] <- 0
  return(z)
}

#' @export
truncated_gaussian_fit_sigma <- function(y, mu, lower = -Inf, upper = Inf, sigma_start = 1) {

  sigma <- sigma_start
  for(i in 1:10) {
    S <- pnorm(upper, mu, sigma, TRUE) - pnorm(lower, mu, sigma, TRUE);
    dS <- safe_multiplication(dnorm(lower, mu, sigma), (lower - mu)) / sigma -
      safe_multiplication(dnorm(upper, mu, sigma), (upper - mu)) / sigma;
    d2S <- -2 * dS / sigma +
      safe_multiplication(dnorm(lower, mu, sigma), (lower - mu)^3) / sigma^4 -
      safe_multiplication(dnorm(upper, mu, sigma), (upper - mu)^3) / sigma^4


    gradient <- -1/sigma + (y-mu)^2/sigma^3 - dS / S;
    hessian <- 1/sigma^2 - 3 * (y-mu)^2/sigma^4 + dS^2 / S^2 - d2S / S

    gradient_logscale <- gradient * sigma
    hessian_logscale <- hessian * sigma^2 + gradient * sigma

    sigma <- exp(log(sigma) - sum(gradient_logscale) / sum(hessian_logscale))
  }

  return(sigma)

}

#' @export
gamma_fit_shape <- function(observed, fitted)
{
  likelihood <- function(k)
  {
    scale <- fitted / k
    -sum(weight * dgamma(observed, shape = k, scale = scale, log = TRUE))
  }

  start <- 1 / sd(fitted - observed)

  fit.nlm <- nlm(likelihood, start, hessian = TRUE)
  return(list(shape = fit.nlm$estimate, s.e. = 1/sqrt(fit.nlm$hessian)))
}

#' @export
colProd <- function(matrix, vector)
{
  return(matrix * rep(vector, rep(nrow(matrix), length(vector))))
}

#' @export
numParam <- function(formula, data, subset)
{
  designMatrix <- model.matrix(as.formula(formula), data[subset, ])
  designMatrix <- designMatrix[, colSums(designMatrix) > 0, drop = FALSE]
  return(ncol(designMatrix))
}

#' @export
exponentialGlm <- function(formula, data, subset, weight, link = 'log')
{
  designMatrix <- model.matrix(as.formula(formula), data[subset, ])
  designMatrix <- designMatrix[, colSums(designMatrix) > 0, drop = FALSE]

  x <- data[subset, all.vars(update(as.formula(formula), .~0))]
  w <- as.numeric(weight[subset])

  coef <- rep(0, ncol(designMatrix))
  if(colnames(designMatrix)[1] == "(Intercept)")
  {
    coef[1] <- 1/mean(x)
  }

  loglikelihood <- 0;
  lastLikelihood <- Inf;
  maxIter <- 50;
  tol <- 10^-7

  continue <- FALSE;

  for(i in 1:maxIter)
  {
    if(link == 'log')
    {
      lambda <- exp(-rowSums(colProd(designMatrix, coef)))
    } else if(link == 'inverse')
    {
      lambda <- rowSums(colProd(designMatrix, coef))
      sign <- (lambda > 0) * 2 - 1
      lambda <- abs(lambda)

      if(sum(sign == -1) != 0)
      {
        continue <- TRUE # We can not stop on this result
      } else {
        continue <- FALSE
      }
      #lambda[lambda < 0] <- 1/max(x);
    }
    loglikelihood <- sum((w * log(lambda) - w * lambda * x))

    if(!continue && abs((lastLikelihood - loglikelihood)/loglikelihood) < tol)
    {
      # convergence
      break;
    }

    lastLikelihood <- loglikelihood

    if(link == 'log')
    {
      d1 <- colSums(w * designMatrix - w * x * lambda * designMatrix)
      d2 <- crossprod(sqrt(w * x * lambda) * designMatrix)
    } else if(link == 'inverse')
    {
      d1 <- colSums(sign * w * designMatrix * 1/lambda - sign * w * x * designMatrix)
      d2 <- -crossprod(sqrt(w/lambda^2) * designMatrix)
    }

    require(MASS)
    change <- - ginv(d2) %*% d1

    if(link == 'inverse')
    {
      bound <- 1.5 * abs(coef)
      bound[bound == 0] <- 1.5 * 1/mean(x)

      edit <- abs(change) > bound
      change[edit] <- change[edit] / abs(change[edit]) * bound[edit]
    }

    require(MASS)
    coef <- coef + change
  }

  if(i == maxIter)
  {
    status <- 1;
    print('No convergence')
  } else {
    status <- 0
  }

  coef <- as.numeric(coef)

  names(coef) <- colnames(designMatrix)

  bic <- -2 * loglikelihood + log(length(x)) * ncol(designMatrix)

  return(list(coefficients = coef, bic = bic, status = status))
}
