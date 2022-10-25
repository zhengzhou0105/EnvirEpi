## BHBMD bladder iAs data clean censored exposure
## Author: Zheng Zhou
## Date: Oct 17 2022
## Propagate uncertainty in exposure data through simulation
## Simulated exposure used in BHBMD modeling

## Input ----------
mywd <- paste0(getwd(),"/")
fileDir <- mywd

df_raw <- read.csv(file = paste0(fileDir,"Metadata_bladder_iAs_censored.csv"))

## Settings-------
source(paste0(fileDir,"BHBMD_model_utilities.R"),
       local = T)

## simulate water concentration ug/L-----
N <- nrow(df_raw)
lnorm_meanlog <- df_raw$lnorm_meanlog
lnorm_sdlog <- df_raw$lnorm_sdlog

water_conc <- vector(length = N)
low <- df_raw$Expo_low
up <- df_raw$Expo_up
bounds_replace <- which(df_raw$Index %in% c(8,9))
low[bounds_replace] <- c(
  qlnorm(c(0,0.25,0.5,0.75), meanlog = lnorm_meanlog[25],sdlog = lnorm_sdlog[25]),
  qlnorm(c(0,0.25,0.5,0.75), meanlog = lnorm_meanlog[29],sdlog = lnorm_sdlog[29])
)
up[bounds_replace] <- c(
  qlnorm(c(0.25,0.5,0.75,1), meanlog = lnorm_meanlog[25],sdlog = lnorm_sdlog[25]),
  qlnorm(c(0.25,0.5,0.75,1), meanlog = lnorm_meanlog[29],sdlog = lnorm_sdlog[29])
)
up[is.na(up)] <- Inf

df_output <- data.frame(
  Index = df_raw$Index,
  waterConc_low = low,
  waterConc_up = up,
  waterConc_meanlog = lnorm_meanlog,
  waterConc_sdlog = lnorm_sdlog
)
write.csv(df_output,"iAs_water_censored_simulation.csv",row.names = F)

## waterIR L/day------
# study 1
Nwater_1 <- c(
  226+327,
  246+328,
  282+325,
  243+254,
  82+53
)
waterIR_low_1 <- c(0,1.1,1.5,2.2,3.8)
waterIR_up_1 <- c(1.1,1.5,2.2,3.8,NA)
# normal
waterIR_par_1 <- censored_norm_up_open(low = waterIR_low_1,
                                        up = waterIR_up_1,
                                        Nsub = Nwater_1)

# study 2
waterIR_par_2 <- c(2.28,1.02)           # normal

# study 3
waterIR_par_3 <- c(2,4)                 # uniform     FDA 2016

# study 4
waterIR_par_4 <- c(1.6,0.8)               # normal

# study 5
waterIR_par_4 <- c(1.2,1)              # normal US EFH

# study 6
waterIR_par_5 <- c(3,1)                   # normal Onno 2007

# study 7
waterIR_par_6 <- c(1.9,1)               # normal

# study 8 & 9 is reported as ug/day 

# study 10
waterIR_par_10 <- c(1.5,0.8)           # normal

## Body Weight kg-----
BW_1 <- 80
BW_2 <- 77
BW_3 <- 65
BW_4 <- 72
BW_5 <- 80
BW_6 <- 60
BW_7 <- 60
BW_8 <- 60
BW_9 <- 60
BW_10 <- 80

## Other oral sources----
# study 1
OAS_1 <- 0.0319        # ug/kg/day

# study 2
OAS_2 <- 10.7          # ug/D

# study 3
OAS_3 <- c(0.079,0.104)       # ug/kg/D unif

# study 4
OAS_4 <- 0.0319       # comparable to US ug/kg/D

# study 5
OAS_5 <- 0.0319        # ug/kg/day

# study 6
OAS_6 <- 56.4          # ug/D  from Rabman 2009

# study 7
OAS_7 <- 38             # ug/D

# study 8 & 9
OAS_8 <- c(2.94,0.69)          # lnorm estimated from Oguri 2014
OAS_9 <- c(2.94,0.69)           # ug/D

# study 10
OAS_8 <- 0.0319