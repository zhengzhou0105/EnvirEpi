# read USGS data
USGSAs <- read.csv("USGS_As v1.0.csv")

# exploratory analysis
table(USGSAs$STATE)

aggregate(x=USGSAs$AS_CONC,
          by=list(USGSAs$STATE),
          function(x) c(n=length(x),mean=mean(x),sd=sd(x),quantile(x,c(0.25,0.5,0.75,0.95)),max=max(x)))

# add region variable
region <- function(state){
  if(state %in% c("CT","MA","ME","NH","NJ","NY","PA","RI","VT")){
    region <- "NE"
  } else if (state %in% c("IA","ID","IL","KS","MI","MN","MO","NE","ND","OH","SD","WI")){
    region <- "NC"
  } else if (state %in% c("AK","DE","FL","GA","KY","LA","MD","AL","DC","GA","MS","NC","OK","SC","TN","TX","VA","WV")){
    region <- "ST"
  } else {
    region <- "WT"
  }
  return(region)
}
for(i in 1:nrow(USGSAs)){
  USGSAs$region[i] <- region(USGSAs$STATE[i])
}

table(USGSAs$region)


# add limit variable
limit <- function(As){
  ifelse(As > 50,1,0)
}
USGSAs$limit <- apply(USGSAs[3],1,limit)


# describe conc and limit by region
aggregate(x=USGSAs$AS_CONC,
          by=list(USGSAs$region),
          function(x) c(mean(x),sd(x),quantile(x,c(0.25,0.5,0.75,0.95)),max(x)))

aggregate(x=USGSAs$limit,
          by=list(USGSAs$region),
          function(x) c(length(x),mean(x)))

# add west indicator
USGSAs$West <- apply(USGSAs[4],1,FUN = function(x) ifelse(x == "WT","West","Non-west"))

# describe by west
aggregate(x=USGSAs$AS_CONC,
          by=list(USGSAs$West),
          function(x) c(n=length(x),mean=mean(x),sd=sd(x),quantile(x,c(0.25,0.5,0.75,0.95)),max=max(x)))


# visualization
library(ggplot2)

ggplot(data = USGSAs,aes(x=log(AS_CONC),group=West))+
  geom_histogram(aes(y=..count../sum(..count..)),binwidth = 0.5, fill="green")+
  geom_freqpoly(aes(y=..count../sum(..count..)), binwidth = 0.5,size=1.5,color="red")+
  facet_grid(.~West)+
  # geom_boxplot()+
  scale_x_continuous(name = "Log-transformed As concentration in well water(ug/L)")+
  scale_y_continuous(name = "Proportion",breaks = seq(from=0,to=0.5,by=0.05))+
  theme_gray(base_size = 20)
  # scale_y_discrete(name = "Located in the western states" , breaks = c("Non-west","West"))
