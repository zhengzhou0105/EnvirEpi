
library(gee)
library(lme4)
library(ggplot2)
library(lattice)
library(GGally)
library(multgee)
library(mlogit)
library(ordinal)
library(geepack)
library(dplyr)
library(mixor)
library(expss)

# ===========read data===========
# read long data
# NLS79.long <- read.csv("NLS79long.csv")

# read binary Y long data
NLS79.long.bi <- read.csv("NLS79long v1.1.csv")

# =========exam missing mechanism==========
# examine y data
ytable <- table(NLS79.long$y,NLS79.long$Year)
print(ytable)
yprop <- prop.table(ytable,2)
print(yprop)

# examination on binary y

ytable <- table(NLS79.long.bi$y,NLS79.long.bi$Year,NLS79.long.bi$ExpoSur)
print(ytable)
yprop <- prop.table(ytable,1)
print(yprop)

ytable <- table(NLS79.long.bi$y,NLS79.long.bi$ExpoSur)
print(ytable)
yprop <- prop.table(ytable,2)
print(yprop)

aggregate(NLS79.long.bi$y,
          by=list(NLS79.long.bi$Year,NLS79.long.bi$ExpoSur),
          function(x) mean(x,na.rm=T))

# examine missing pattern using 
NLS79.long$LBW <- as.character(NLS79.long$LBW)

NLS79.long.bi$LBW <- as.character(NLS79.long.bi$LBW)

# sex
t.test(data=NLS79.long.bi,
       missing~Sex)

# west
t.test(data=NLS79.long.bi,
       missing~ExpoSur)

# wave
summary(glm(data=NLS79.long.bi,
            missing~factor(Wave),
            family = "binomial"))

aggregate(x=NLS79.long.bi$y,
          by=list(NLS79.long.bi$Wave),
          function(x) mean(x,na.rm=T))

# missingfreq <- table(NLS79.long$missing,NLS79.long$Wave)
# print(missingfreq)
# missingfreq <- t(missingfreq)
# missingfreq <- as.matrix(missingfreq)
# colnames(missingfreq) <- c("complete","missing")


# =======exploratory============
ggplot(NLS79.long.bi,
       aes(x=Race,y=Sex))+
  stat_sum(aes(size=..n..,group = 1))+
  scale_size_area()
ggplot(NLS79.long.bi,
       aes(x=ExpoSur,y=Race))+
  stat_sum(aes(size=..n..,group = 1))+
  scale_size_area()
ggplot(NLS79.long.bi,
       aes(x=ExpoSur,y=Sex))+
  stat_sum(aes(size=..n..,group = 1))+
  scale_size_area()


dfprint <- NLS79.long.bi
match("ExpoSur",names(dfprint))
dfprint$ExpoSur <- apply(dfprint[5],1,function(x) ifelse(x == 0, "Non-exposed","Exposed"))
ggplot(data = dfprint,aes(x = Year, y= y), group = ID)+
  stat_summary(geom = "line", fun = function(x) mean(x,na.omit=T), size=1.5)+
  facet_grid(.~as.character(ExpoSur))+
  scale_x_continuous(breaks = seq(from=1994,to=2016,by=4))+
  scale_y_continuous(name = "Proportion of Depression")+
  theme_gray(base_size = 20)
# ========ordinal Y===========
# =========GEE============

# GEE fit
# using multgee
intrinsic.pars(y, NLS79.long, ID, Wave, rscale = "ordinal")

# mod1 Year as continuous
NLS79.long.gee1 <- ordLORgee(y~Year*West,
                            data = NLS79.long,
                            id=ID,
                            repeated = Year,
                            LORstr = "time.exch")
summary(NLS79.long.gee1)

# mod2 Wave as continous
NLS79.long.gee2 <- ordLORgee(y~Wave*West,
                             data = NLS79.long,
                             id=ID,
                             repeated = Wave,
                             LORstr = "time.exch")
summary(NLS79.long.gee2)
head(NLS79.long.gee2$fitted.values)

# mod3 Wave as discreet
NLS79.long.gee3 <- ordLORgee(y~factor(Wave)*West,
                             data = NLS79.long,
                             id=ID,
                             repeated = Wave,
                             LORstr = "time.exch")
summary(NLS79.long.gee3)

# using geepack
NLS79.long.gee4 <- ordgee( ordered(y) ~  Wave*West,
                         data = NLS79.long,
                         id = ID,
                         # waves = Wave,
                         corstr = "exchangeable")
summary(NLS79.long.gee4)


# =========mixed effects================
# using mixor
NLS79.long.clmm1 <- mixor(y ~ West * Wave,
                          data = NLS79.long,
                          id = ID,
                          link = "logit")
summary(NLS79.long.clmm1)
pihat <- predict(NLS79.long.clmm1)
head(pihat$predicted)


# using clmm from ordinal package
NLS79.long.clmm2 <- clmm(ordered(y)~Wave*West+ (1|ID),
                        data = NLS79.long)
summary(NLS79.long.clmm2)

# compare fitted vs observed 

# compare <- NLS79.long %>%
#   group_by(West,Year) %>%
#   summarise(sum(y,na.rm = T) / length(y))
# colnames(compare)[3] <- "PropObv"
# print(compare)
# compare$oddsfit.gee <- 0.86891 + 0.02267 * ((compare$Year - 1994)/2+1) -0.61677 * compare$West +
#   0.15445 * compare$West * ((compare$Year - 1994)/2+1)
# compare$fit.gee <- exp(compare$oddsfit.gee) / (1+exp(compare$oddsfit.gee))


# ===========binary y=========
# ------mixed effects--------

NLS79.long.bi.glmm <- glmer(y ~ ExpoSur * Wave + Race + Sex + LBW + (1|ID),
                            data = NLS79.long.bi,
                            family = "binomial",
                            nAGQ = 10)
NLS79.long.bi.glmm <- glmer(y ~ ExpoSur * Wave + Sex + LBW + (1|ID),
                            data = NLS79.long.bi,
                            family = "binomial",
                            nAGQ = 10)
summary(NLS79.long.bi.glmm)

# -------GEE--------
NLS79.long.bi.gee <- gee(y ~ExpoSur * Wave  + Sex + LBW,
                         data = NLS79.long.bi,
                         id = ID,
                         family = "binomial",
                         corstr = "exchangeable")
summary(NLS79.long.bi.gee)

