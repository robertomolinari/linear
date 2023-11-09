#' @title Linear Loss Function
#'
#' @description Compute an approximation of the integral of the function f(x)
#' with respect to dx in the range [a, b] by Monte-Carlo integration using
#' uniform sampling.
#' @param resp A \code{vector} of dimension 2 used to denote the integration
#' region of interest, i.e. [a, b].
#' @param pred A \code{string} containing the function to be integrated. It
#' is assumed that \code{x} is used as the variable of interest.
#' @param beta A \code{numeric} (integer) used to denote the number of simulations.
#' @param norm A \code{numeric} used to control the seed of the random number
#' generator used by this function.
#' @return A \code{numeric} containing the loss function at \code{beta}
#' @author Rob Molinari
#' @importFrom stats
#' @export
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(600), nrow = 200) # true matrix of predictors
#' epsilon <- rnorm(200, 0, sd = 0.25) # observation error
#' X <- cbind(rep(1, 200), X) # add a column of ones to generate the response with an intercept value
#' beta <- c(-1, 4, -5, 2) # true coefficient values we want to estimate (first element is the intercept)
#' y <- X%*%beta + epsilon # generate response y based on this model
#' beta_ls(resp = y, pred = X, beta = beta, norm = "L2")
beta_ls <- function(resp, pred, beta, norm = "L2") {

  if(norm == "L2") {

    out <- t(resp - pred%*%beta)%*%(resp - pred%*%beta) # L2-norm (i.e. least-squares)

  } else {

    out <- sum(abs(resp - pred%*%beta)) # L1-norm (robust but inefficient)

  }

  return(out)

}
