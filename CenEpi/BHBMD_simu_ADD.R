## BHBMD bladder iAs data uncertainty censored epidemiology data 
## Author: Zheng Zhou
## Date: Oct 17 2022
## Propagate uncertainty in exposure data through simulation
## Simulated exposure used in BHBMD modeling

## Input----
# HPC batch settings
task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
## task_id <- 1
## mywd <- paste0(getwd(),"/")
## fileDir <- mywd
mywd <- "/N/scratch/zhezhou/test/"
fileDir <- "/N/slate/zhezhou/"
iter <- 5
DSName <- "simu_ADD"

## stan sampling settings
samplingiter <- 1E4*5   # number of iterations in stan sampling
warmup <- 0.5      # proportion of steps used for warmup in stan sampling
Nchain <- 4       # number of chains in stan sampling
thin <- 1          # factor of thin in stan sampling
# seed <- 47405      # use IUB postal code as seed for random simulation

## Settings------
source(file = paste0(fileDir,"BHBMD_model_utilities.R"),
       local = T)
batchID <- task_id
batchsize <- iter

fn_simu_iAsADD <- function(){
  # Exposure via drinking water
  # study 8 & 9 unit is ug/D not concentration
  waterConc <- df_water_simu %>% rowwise() %>% mutate(
    waterConc = rlnormTrunc(1,waterConc_meanlog,waterConc_sdlog,
                            min = waterConc_low,max = waterConc_up)
  )
  # conc * intake rate
  waterExpo_1 <- waterConc %>% filter(Index == 1) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.7636997,0.9686693,min = 0 )
  ) %>% select(Expo)
  waterExpo_2 <- waterConc %>% filter(Index == 2) %>% mutate(
    Expo = waterConc * rnormTrunc(1,2.28,1.02,min = 0)
  ) %>% select(Expo)
  waterExpo_3 <- waterConc %>% filter(Index == 3) %>% mutate(
    Expo = waterConc * runif(1,2,4)
  )%>% select(Expo)
  waterExpo_4 <- waterConc %>% filter(Index == 4) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.6,0.8,min = 0)
  )%>% select(Expo)
  waterExpo_5 <- waterConc %>% filter(Index == 5) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.2,1,min = 0)
  )%>% select(Expo)
  waterExpo_6 <- waterConc %>% filter(Index == 6) %>% mutate(
    Expo = waterConc * rnormTrunc(1,3,1,min = 0)
  )%>% select(Expo)
  waterExpo_7 <- waterConc %>% filter(Index == 7) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.9,1,min = 0)
  )%>% select(Expo)
  waterExpo_8 <- waterConc %>% filter(Index == 8) %>% rename(
    Expo = waterConc) %>% select(Expo)
  waterExpo_9 <- waterConc %>% filter(Index == 9) %>% rename(
    Expo = waterConc) %>% select(Expo)
  waterExpo_10 <- waterConc %>% filter(Index == 10) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.5,0.8,min = 0)
  ) %>% select(Expo)
  
  # Other Sources--------
  OAE_8 <- rlnorm(1,2.94,0.69)
  OAE_9 <- rlnorm(1,2.94,0.69)
  
  # ADD-------
  ADD_1 <- waterExpo_1 / 80 + 0.0319
  ADD_2 <- (waterExpo_2 + 10.7)/77
  ADD_3 <- waterExpo_3 / 65 + runif(3,0.079,1.04)
  ADD_4 <- waterExpo_4 / 72 + 0.0319
  ADD_5 <- waterExpo_5 / 80 + 0.0319
  ADD_6 <- (waterExpo_6 + 56.4)/60
  ADD_7 <- (waterExpo_7 + 38)/60
  ADD_8 <- (waterExpo_8 + rlnorm(4,2.94,0.69))/60
  ADD_9 <- (waterExpo_9 + rlnorm(4,2.94,0.69))/60
  ADD_10 <- waterExpo_10 / 80 + 0.0319
  
  ADD <- as.numeric(unlist(rbind(ADD_1,ADD_2,ADD_3,ADD_4,ADD_5,ADD_6,ADD_7,ADD_8,ADD_9,ADD_10)))
  ADDZ <- dnormalize_minmax(ADD)
  
  doseList <- list(
    dose = ADD,doseZ = ADDZ
  )
}

## load data-------
df_raw <- read.csv(file = paste0(fileDir,"Metadata_bladder_iAs_censored.csv"))
df_water_simu <- read.csv(file = paste0(fileDir,"iAs_water_censored_simulation.csv"))

## Extract fixed data
N <- nrow(df_raw)
S <- length(unique(df_raw$Index))
Gcount <- df_raw %>% group_by(Index) %>% count()
G <- as.data.frame(ungroup(Gcount))[,2]
Index <- df_raw$Index
Study <- df_raw$Study
Nsub <- df_raw$Nsub
ymean <- df_raw$RR
ymeanL <- log(df_raw$RR)
ysdL <- getRRSE(RR.l = df_raw$RR.l,RR.u = df_raw$RR.u)
ysd <- get_ysd(ymean,ysdL)

## Simulate ADD---------
DataList_ADD <- vector("list",length = batchsize)
for(i in 1:batchsize){
  DataList_ADD[[i]] <- fn_simu_iAsADD()
}

## Integrate DataList-----
df_batch <- lapply(DataList_ADD,function(x){
  append(list(
           Index = Index,Study = Study,Nsub = Nsub,
           ymean = ymean,ymeanL = ymeanL,
           ysd = ysd, ysdL = ysdL
         ),x)
})
mapply(write.csv,x = df_batch,
       file = paste0("DataList","_batch_",batchID,"_",1:batchsize, ".csv"),
       row.names = F)

DataList_batch <- lapply(df_batch,FUN = function(x){
  append(list(N = N,S = S,G = G),x)
})

## stan fitting------
fitList_batch <- lapply(DataList_batch,function(x){
  dataName <- "simu_iAsbladder"
  DataList <- x
  
  modelname <- "Lognormal_Summary_Linear_meta_3"
  assign(paste0("stanfit_",modelname,"_",dataName),
         get_metaStanfit(modelname = modelname,dataName = dataName,DataList = DataList))
  modelname <- "Lognormal_Summary_Power_meta_4"
  assign(paste0("stanfit_",modelname,"_",dataName),
         get_metaStanfit(modelname = modelname,dataName = dataName,DataList = DataList))
  modelname <- "Lognormal_Summary_Hill_meta_5"
  assign(paste0("stanfit_",modelname,"_",dataName),
         get_metaStanfit(modelname = modelname,dataName = dataName,DataList = DataList))
  modelname <- "Lognormal_Summary_Expo5_meta_5"
  assign(paste0("stanfit_",modelname,"_",dataName),
         get_metaStanfit(modelname = modelname,dataName = dataName,DataList = DataList))
  
  ## organize the results into a list
  assign(
    paste0("fitList_",dataName),
    list(
      Linear = eval(parse(text = paste0("stanfit_Lognormal_Summary_Linear_meta_3_",dataName))),
      Hill = eval(parse(text = paste0("stanfit_Lognormal_Summary_Hill_meta_5_",dataName))),
      Power = eval(parse(text = paste0("stanfit_Lognormal_Summary_Power_meta_4_",dataName))),
      Expo5 = eval(parse(text = paste0("stanfit_Lognormal_Summary_Expo5_meta_5_",dataName)))
    )
  )
  
  return(eval(parse(text = paste0("fitList_",dataName))))
})

assign(
  paste0("fitList_",DSName, "_batch_",batchID),
  fitList_batch
)

# export stanfit objects per iteration
save(list = paste0("fitList_",DSName, "_batch_",batchID),
     file = paste0("fitList_",DSName, "_batch_",batchID,".rdata"))

## Weighting-----
weights_batch <- lapply(fitList_batch, getwts)
assign(
  paste0("weights_",DSName,"_batch_",batchID),
  weights_batch
)

# Export weightings
## save(list = paste0("weights_batch_",batchID),
##     file = paste0("Weights__batch_",batchID,".rdata"))
mapply(write.csv,x = weights_batch,
       file = paste0("Weights_",DSName,"_batch_",batchID,"_",1:batchsize,".csv"))

## Weighting Evaluation-----
# Performance evaluation
PerfMetrics_batch <- t(mapply(ModelEvalbyFd,      # a batchsize * 4 matrix
                              fitList = fitList_batch,
                              modelweights = weights_batch,
                              DataList = DataList_batch))
Weighted_batch <- PerfMetrics_batch[,1]
mapply(write.csv,x = Weighted_batch, 
       file = paste0("Weighted_",DSName,"_batch_",batchID,"_",1:batchsize,".csv"))

## Weighted_mean_batch <- PerfMetrics_batch[,2]
## mapply(write.csv,x = Weighted_mean_batch,
##       file = paste0("Weighted_mean__batch_",batchID,"_",1:batchsize,".csv"),row.names =F)

Metrics_batch <- PerfMetrics_batch[,3]
mapply(write.csv,x = Metrics_batch,
       file = paste0("Metrics_",DSName,"_batch_",batchID,"_",1:batchsize,".csv"),row.names =T)

ChoiceList_batch <- PerfMetrics_batch[,4]

mapply(
  write.csv,
  x = ChoiceList_batch,
  file = paste0("Choice_",DSName,"_batch_",batchID,"_",1:batchsize, ".csv"),
  row.names =T
)

# Generate figures
options(bitmapType='cairo')
mapply(ShowWeightedValues,
       DataList = DataList_batch,
       modelwts = weights_batch,
       weighted = Weighted_batch,
       dataName = paste0("Graph_",DSName,"_batch_",batchID,"_",1:batchsize)
)
