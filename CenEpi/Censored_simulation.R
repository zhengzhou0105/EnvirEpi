## Characterize uncertainty in censored dose/exposure data
## Zheng Zhou
## Date: 10/14/2022



## Change input------
mywd <- paste0(getwd(),"/")
fileDir <- mywd
CensDist <- "lognormal"

## Load Censored data-----
df_censored <- read.csv(
  file = paste0(mywd,"bladder cancer ~ ADD all 0520.csv")
)

## Settings------
samplingiter <- 1e3
warmup <- 0.5
Nchain <- 4

source(file = paste0(fileDir,"BHBMD_model_utilities.R"),
       local = T)

## distributional characterization by study-----

# study 1: 
G <- 2
low <- c(0,10)
up <- c(10,20)
Nsub <- c(300,200)
q <- Nsub/sum(Nsub)
par <- c(1,1)

LS <- vector(length = 2)

for(i in 1:G){
  LS[i] <- plnorm(log(up[i]),par[1],par[2]) - plnorm(log(low[i]),par[1],par[2]) - q[i]
}
SLS <- sum(MLE)
