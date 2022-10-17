## Characterize uncertainty in censored dose/exposure data
## Zheng Zhou
## Date: 10/14/2022



## Change input------
mywd <- paste0(getwd(),"/")
fileDir <- mywd
CensDist <- "lognormal"

## Load Censored data-----
## Using bladder cancer data
df_censored <- read.csv(
  file = paste0(mywd,"bladder cancer ~ ADD all.csv")
)

## Settings------
source(file = paste0(fileDir,"Censored_utilities.R"),local = T)

## Cleaning-----

# Clean
df_censored_clean <- df_censored %>% rename(
  Expo_Cens = 6, Nsub = 12, WaterIR = 17, BW =18,
  SSource = 20, SDist = 21
) 

## distributional characterization by study-----
Nstudy <- max(df_censored$Index)

# study 1
df_study <- df_censored_clean %>% filter(Index == 1)
print(df_study)

pars_s1 <- censored_lnorm_up(low = df_study$Expo_low,
                             up = df_study$Expo_up,
                             Nsub = df_study$Nsub)

# study 2
df_study <- df_censored_clean %>% filter(Index == 2)
print(df_study)

pars_s2 <- censored_lnorm_up(low = df_study$Expo_low,
                             up = df_study$Expo_up,
                             Nsub = df_study$Nsub)

# study 3
df_study <- df_censored_clean %>% filter(Index == 3)
print(df_study)

pars_s3 <- censored_lnorm_up(low = df_study$Expo_low,
                             up = df_study$Expo_up,
                             Nsub = df_study$Nsub)

# study 4
df_study <- df_censored_clean %>% filter(Index == 4)
print(df_study)

pars_s4 <- censored_lnorm_up(low = df_study$Expo_low,
                             up = df_study$Expo_up,
                             Nsub = df_study$Nsub)

# study 5
df_study <- df_censored_clean %>% filter(Index == 5)
print(df_study)

pars_s5 <- censored_lnorm_up(low = df_study$Expo_low,
                             up = df_study$Expo_up,
                             Nsub = df_study$Nsub)

# study 6
df_study <- df_censored_clean %>% filter(Index == 6)
print(df_study)

pars_s6 <- censored_lnorm_up(low = df_study$Expo_low,
                             up = df_study$Expo_up,
                             Nsub = df_study$Nsub)

# study 7
df_study <- df_censored_clean %>% filter(Index == 7)
print(df_study)

pars_s7 <- censored_lnorm_up(low = df_study$Expo_low,
                             up = df_study$Expo_up,
                             Nsub = df_study$Nsub)

# study 8
df_study <- df_censored_clean %>% filter(Index == 8)
print(df_study)

pars_s8 <- censored_quartMedian(median = df_study$Expo_Cens,
                                Nsub = df_study$Nsub)

# study 9
df_study <- df_censored_clean %>% filter(Index == 9)
print(df_study)

pars_s9 <- censored_quartMedian(median = df_study$Expo_Cens,
                                Nsub = df_study$Nsub)

# study 10
df_study <- df_censored_clean %>% filter(Index == 10)
print(df_study)

pars_s10 <- censored_lnorm_up(low = df_study$Expo_low,
                             up = df_study$Expo_up,
                             Nsub = df_study$Nsub)

# Output-----
pars_all <- list(pars_s1,pars_s2,pars_s3,pars_s4,pars_s5,
                  pars_s6,pars_s7,pars_s8,pars_s9,pars_s10)
Ngroups <- df_censored_clean %>% group_by(Index) %>% count()
ls_pars <- list(length = Nstudy)

for(i in 1:Nstudy){
  ls_pars[[i]] <- rep(pars_all[[i]], Ngroups[i,2])
}

df_pars <- as.data.frame(matrix(unlist(ls_pars),ncol = 2,byrow = T))
colnames(df_pars) <- c("lnorm_meanlog","lnorm_sdlog")

df_output <- cbind(df_censored_clean,df_pars)

write.csv(df_output,"Metadata_bladder_iAs_censored.csv",row.names = F)

