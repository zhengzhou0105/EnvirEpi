## Utility functions for censored data

## load packages-----
require("tidyverse")

## General functions-------
# when the upper bound of the last group is closed
censored_lnorm_up_close <- function(low,up,Nsub){

  G <- length(low)
  if(is.na(up[G])) warning("The upper bound of the last group is infinity")
  
  q <- Nsub / sum(Nsub)
  
  fit_lnorm_up_close <- function(par,low,up,Nsub){
    # LS <- vector(length = G)
    # for(i in 1:G){
    #   LS[i] <- (plnorm(log(up[i]),par[1],par[2]) - plnorm(log(low[i]),par[1],par[2]) - q[i])^2
    # }
    # SLS <- sum(LS)
    # return(SLS)
    LL <- vector(length = G)
    for(i in 1:G){
      LL[i] <- q[i] * log(plnorm(up[i],par[1],par[2])-plnorm(low[i],par[1],par[2]))
    }
    MLE <- -sum(LL)
  }
  
  fit <- optim(
    # par = c(log(0.1),5),
    par = c(0,1),
    fn = fit_lnorm_up_close,
    low = low, up = up , Nsub = Nsub
  )
  
  pars_lnorm <- fit$par
  return(pars_lnorm)
}

# when the upper bound of the last group is open
censored_lnorm_up_open <- function(low,up,Nsub){
  G <- length(low)
  q <- Nsub / sum(Nsub)
  
  if(!is.na(up[G])) warning("The upper bound of the last group is infinity")
  
  fit_lnorm_up_open <- function(par,low,up,Nsub){
    # LS <- vector(length = G)
    # for(i in 1:(G-1)){
    #   LS[i] <- (plnorm(log(up[i]),par[1],par[2]) - plnorm(log(low[i]),par[1],par[2]) - q[i])^2
    # }
    # LS[G] <- (1-plnorm(log(low[G]),par[1],par[2]) - q[G])^2
    # SLS <- sum(LS)
    # return(SLS)
    LL <- vector(length = G)
    for(i in 1:(G-1)){
      LL[i] <- q[i] * log(plnorm(up[i],par[1],par[2])-plnorm(low[i],par[1],par[2]))
    }
    LL[G] <- q[G] * log(1-plnorm(low[G],par[1],par[2]))
    MLE <- -sum(LL)
    
  }
  
  fit <- optim(
    # par = c(log(0.1),5),
    par = c(0,1),
    fn = fit_lnorm_up_open,
    low = low, up = up , Nsub = Nsub
  )
  
  pars_lnorm <- fit$par
  return(pars_lnorm)
}

censored_lnorm_up <- function(low,up,Nsub){
  # check if last upper bound is infinity
  G <- length(low)
  UpInf <- is.na(up[G])
  
  if(UpInf){
    pars <- censored_lnorm_up_open(low,up,Nsub)
  } else {
    pars <- censored_lnorm_up_close(low,up,Nsub)
  }

  return(pars)
}

censored_quartMedian <- function(median,Nsub){
  median <- as.numeric(median)
  
  G <- length(median)
  q <- c(0.125,0.375,0.625,0.875)
  
  fit_lnorm_quartMedian <- function(par,median,Nsub){
    LS <- vector(length = G)
    for(i in 1:G){
      LS[i] <- (plnorm(median[i],par[1],par[2])-q[i])^2
    }
    SLS <- sum(LS)
    return(SLS)
  }
  
  fit <- optim(
    par = c(0,1),
    fn = fit_lnorm_quartMedian,median = median, Nsub = Nsub)
  
  pars_lnorm <- fit$par
  
  return(pars_lnorm)
}
