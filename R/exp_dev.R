### =========================================================================
### exp_dev
### =========================================================================
#' Get explained deviance from model fit
#'
#' Estimate explained deviance from model output
#'
#' @param mod a fitted model object (GLM or GAM)
#' @return Estimate of explained deviance
#' @author Taken from ecospat package & modified by Philipp Brun
#' @export
#' @examples
#'
#' # explained deviance for a glm
#' mod1=glm(X3740 ~ poly(bio_01,2) + poly(bio_01,2) + poly(forest_fraction,2),
#' data=obs.sel,family="binomial")
#' exp_dev(mod1)
#'
#' # explained deviance for a gam
#' library(gam)
#' mod2=gam(X3740 ~ s(bio_01) + s(bio_01) + s(forest_fraction),
#' data=obs.sel,family="binomial")
#' exp_dev(mod2)
#'
exp_dev=function(mod){

  D2 <- (mod$null.deviance - mod$deviance)/mod$null.deviance
  p <- length(mod$coefficients)
  n <- length(mod$fitted)
  adj.D2 <- 1 - ((n - 1)/(n - p)) * (1 - D2)
  if (adj.D2 < 0) {
    adj.D2 <- 0
    return(adj.D2)
  }
  else {
    return(adj.D2)
  }
}
