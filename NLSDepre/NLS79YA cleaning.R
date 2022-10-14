library(ggplot2)

# read version V1.1
NLS79YA <- read.csv("NLS79YA_label v1.1.csv")
NLS79YA$CYRB_XRND <- as.character(NLS79YA$CYRB_XRND)

# ==========identify rural subjects===========
# add rural variable
NLS79YA$ruralyr <- rep(NA,nrow(NLS79YA))

# number of rural years
urbannames <- paste("URBAN.RURAL",seq(from=1994,to=2016,by=2),sep = "_")
match(urbannames,names(NLS79YA))
for(i in 1:nrow(NLS79YA)){
    NLS79YA$ruralyr[i] <- sum(NLS79YA[i,75:86] == "0: RURAL")
}
NLS79YA$rural <- apply(NLS79YA[87],1,function(x) ifelse(x >= 1, 1, 0))

# ===========output rural file===========
# select rural subjects
NLS79rural <- subset(NLS79YA, rural >= 6)

# output rural data
write.csv(NLS79rural,"NLS79YA_rural.csv", row.names = F)

# to identify if a subject lived in the rural area during the first x years

# =========based on rural file=============
# ===========identify region of residence=========
regionnames <- paste("REGION",seq(from=1994,to=2016,by=2),sep = "")
match(regionnames,names(NLS79rural))
# region of longest residence
NLS79rural$region <- apply(NLS79rural[52:62],1,max)

# output as v1.0
write.csv(NLS79rural,"NLS79YA_rural v1.0.csv",row.names = F)

# =============add west variable======
NLS79rural$west <- apply(NLS79rural[88],1,function(x) ifelse(x =="4: WEST",1,0))
table(NLS79rural$west)


# =========read clean data v1.3========

# based on v1.3
NLS79clean <- read.csv("NLS79YA_rural v1.3.csv")
library(reshape2)
NLS79.long <- melt(NLS79clean,id.vars = c("ID","Race","Sex","LBW","west"),
                   measure.vars = paste("y",seq(from=1994,to=2016,by=2),sep = ""),
                   value.name = "y")
colnames(NLS79.long)[6] <- "Wave"
NLS79.long$Wave <- as.factor(substr(as.character(NLS79.long$Wave),2,5))
NLS79.long <- as.data.frame(NLS79.long)
NLS79.long <- NLS79.long[order(NLS79.long$ID),]
NLS79.long$ID <- as.integer(as.character(NLS79.long$ID))


# output long data
write.csv(NLS79.long,"NLS79long.csv",row.names = F)


# based on v1.4
NLS79clean.bi <- read.csv("NLS79YA_rural v1.4.csv")
library(reshape2)
NLS79.long.bi <- melt(NLS79clean.bi,id.vars = c("ID","Race","Sex","LBW","west"),
                   measure.vars = paste("y",seq(from=1994,to=2016,by=2),sep = ""),
                   value.name = "y")
colnames(NLS79.long.bi)[6] <- "Wave"
NLS79.long.bi$Wave <- as.factor(substr(as.character(NLS79.long.bi$Wave),2,5))
NLS79.long.bi <- as.data.frame(NLS79.long.bi)
NLS79.long.bi <- NLS79.long.bi[order(NLS79.long.bi$ID),]
NLS79.long.bi$ID <- as.integer(as.character(NLS79.long.bi$ID))


# output long data
write.csv(NLS79.long.bi,"NLS79.bi.long.csv",row.names = F)

# =========based on all file=============
# ===========identify region of residence=========
regionnames <- paste("REGION",seq(from=1994,to=2016,by=2),sep = "")
match(regionnames,names(NLS79YA))
# region of longest residence
NLS79YA$region <- apply(NLS79YA[52:62],1,max)

# output as v1.2
write.csv(NLS79YA,"NLS79YA v1.2.csv",row.names = F)

# =============add west variable======
NLS79YA$West <- apply(NLS79YA[89],1,function(x) ifelse(x =="4: WEST",1,0))
table(NLS79YA$West)

# output as v1.3
write.csv(NLS79YA,"NLS79YA v1.3.csv",row.names = F)

# =========data cleaning===========
# based on v1.3

# birth year
colnames(NLS79YA)[6] <- "BirthYr"
NLS79YA$BirthYr <- apply(NLS79YA[6],1, function(x) 
  if(x == "1970 TO 1978: < before 1979"){
    1974} else {
      x
    })
NLS79YA$BirthYr <- as.integer(NLS79YA$BirthYr)

# age at wave 2016
NLS79YA$age2016 <- 2016 - NLS79YA$BirthYr
NLS79YA$age1994 <- 1994 - NLS79YA$BirthYr
summary(NLS79YA$age1994)

# rename LBW
colnames(NLS79YA)[7] <- "LBW"
NLS79YA$LBW <- as.character(NLS79YA$LBW)
NLS79YA$LBW <- apply(NLS79YA[7],1,function(x)  
  if(x == ""){
    NA
  } else {
    x
  })

# clean Race and Sex
NLS79YA$Race <- as.character(NLS79YA$Race)
NLS79YA$Sex <- as.character(NLS79YA$Sex)
NLS79YA$Race <- apply(NLS79YA[4],1,function(x)  
  if(x == ""){
    NA
  } else {
    x
  })
NLS79YA$Sex <- apply(NLS79YA[5],1,function(x)  
  if(x == ""){
    NA
  } else {
    x
  })

# ======add expo indicator=======
nrow(subset(NLS79YA, age1994 <= 6))
nrow(subset(NLS79YA, age1994 <= 6 & West == 1))
nrow(subset(NLS79YA, age1994 <= 6 & West == 1 & rural == 1))

NLS79YA$ExpoSur <- rep(NA,nrow(NLS79YA))
for(i in 1: nrow(NLS79YA)){
  NLS79YA$ExpoSur[i] <- ifelse(NLS79YA$age1994[i] <=6 & NLS79YA$West[i] == 1 & NLS79YA$rural[i] == 1,
                               1,0)
}
table(NLS79YA$ExpoSur)

# rename y
ynames <- paste("Q16.6C",seq(from=1994,to=2016,by=2),sep = "_")
match(ynames,names(NLS79YA))
# region of longest residence
colnames(NLS79YA)[63:74] <- paste("y",seq(from=1994,to=2016,by=2),sep = "_")

# output as v1.4
write.csv(NLS79YA,"NLS79YA v1.4.csv",row.names = F)

# ======melt to long format=========

library(reshape2)
NLS79.long <- melt(NLS79YA,id.vars = c("ID","Race","Sex","LBW","ExpoSur"),
                   measure.vars = paste("y",seq(from=1994,to=2016,by=2),sep = "_"),
                   value.name = "y")
colnames(NLS79.long)[6] <- "Year"
NLS79.long$Year <- as.numeric(substr(as.character(NLS79.long$Year),3,6))
NLS79.long <- as.data.frame(NLS79.long)
NLS79.long <- NLS79.long[order(NLS79.long$ID),]
# convert all y == "" to NA
NLS79.long$y <- apply(NLS79.long[7],1,function(x)  
  if(x == ""){
    NA
  } else {
    x
  })

# add year
NLS79.long$Wave <- apply(NLS79.long[6],1, function(x) (x-1994)/2+1)

# add dummy missing indicator
NLS79.long$missing <- apply(NLS79.long[7],1,function(x) ifelse(is.na(x),1,0))

# output long data
write.csv(NLS79.long,"NLS79long.csv",row.names = F)

# ======descriptive by expo========
# NLS79YA <- read.csv("NLS79YA v1.4.csv")

t.test(as.numeric(Sex)~ExpoSur,
       data = NLS79YA)

table(NLS79YA$Race,NLS79YA$ExpoSur)
prop.table(table(NLS79YA$Race,NLS79YA$ExpoSur),2)
summary(aov(as.numeric(Race)~ExpoSur, data = NLS79YA))

t.test(as.numeric(LBW) ~ ExpoSur,data = NLS79YA)

