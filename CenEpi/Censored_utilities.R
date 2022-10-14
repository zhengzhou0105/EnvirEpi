## Utility functions for censored data

# when the upper bound of the last group is closed
censored_lnorm_up_close <- function(low,up,Nsub){

  G <- length(low)
  if(is.na(up[G])) warning("The upper bound of the last group is infinity")
  
  q <- Nsub / sum(Nsub)
  
  fit_lnorm_up_close <- function(par,low,up,Nsub){
    LS <- vector(length = G)
    for(i in 1:G){
      LS[i] <- plnorm(log(up[i]),par[1],par[2]) - plnorm(log(low[i]),par[1],par[2]) - q[i]
    }
    SLS <- sum(LS)
    return(SLS)
  }
  
  fit <- optim(
    par = c(1,1),
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
    LS <- vector(length = G)
    for(i in 1:(G-1)){
      LS[i] <- plnorm(log(up[i]),par[1],par[2]) - plnorm(log(low[i]),par[1],par[2]) - q[i]
    }
    LS[G] <- 1-plnorm(log(low[G]),par[1],par[2]) - q[G]
    SLS <- sum(LS)
    return(SLS)
  }
  
  fit <- optim(
    par = c(1,1),
    fn = fit_lnorm_up_open,
    low = low, up = up , Nsub = Nsub
  )
  
  pars_lnorm <- fit$par
  return(pars_lnorm)
}
