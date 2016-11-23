rm(list = ls())
library(lme4)
library(bbmle)
library(rptR)
library(MASS)
library(boot)
library(beepr)
library(lsmeans)
library(piecewiseSEM)
library(MuMIn)

library(RPostgreSQL)
drv<-dbDriver("PostgreSQL")
con<-dbConnect(drv, dbname="lab1",host="localhost",port=5432,user="postgres",password="postgres")
dbListTables(con)##returns a vector object
mantis<-dbGetQuery(con,"SELECT * From mantis;")
View(mantis)
mantids<-as.data.frame.matrix(mantis)



mantids$id <- as.factor(mantids$id)
mantids$trial <- as.factor(mantids$trial)

# add OLRE
mantids$res <- as.factor(seq(1,length(mantids$id), 1)) #With poisson models expectation of tail of unknown length. Can have overdispersion
#If don't account, will be too confident (tight confidence intervals). Way to account is to have 

mantids$loss.p <- (mantids$start_mass - mantids$max_mass)/mantids$max_mass
mantids$loss.p.c <- (mantids$loss.p - mean(mantids$loss.p, na.rm=TRUE))/(sd(mantids$loss.p, na.rm=TRUE))

# boldness measures are skewed, log transform

#Latency to move has some zeros

mantids$bold.LTM <- mantids$bold_ltm + 1
mantids$bold.LTM.log <- log(mantids$bold.LTM)



mantids$bold.TTLC.log <- log(mantids$bold_leave_circle)
mantids$bold.TTS.log <- log(mantids$bold_time_shelter)

# center mass
mantids$mass.c <- (mantids$start_mass - mean(mantids$start_mass, na.rm=TRUE))/sd(mantids$start_mass, na.rm=TRUE)

mantids$max.mass.c <- (mantids$max_mass - mean(mantids$max_mass, na.rm=TRUE))/sd(mantids$max_mass, na.rm=TRUE)


# quick pearson's - everything is correlated
cor.test(mantids$bold.LTM.log, mantids$agg_approach)
cor.test(mantids$bold.TTLC.log, mantids$agg_approach)
cor.test(mantids$bold.TTS.log, mantids$agg_approach)
cor.test(mantids$bold.LTM.log, mantids$agg_strike)
cor.test(mantids$bold.TTLC.log, mantids$agg_strike)
cor.test(mantids$bold.TTS.log, mantids$agg_strike)


#Eaten correlation-no correlation
cor.test(mantids$eaten,mantids$bold.LTM.log)
cor.test(mantids$eaten,mantids$bold.TTLC.log)
cor.test(mantids$eaten,mantids$bold.TTS.log)
cor.test(mantids$eaten,mantids$agg_strike)
cor.test(mantids$eaten,mantids$agg_approach)


#Latency to move model
mantid.LTM <- lmer(bold.LTM.log ~ 1 + species + sex + mass.c + (1|id),
	data = mantids)
mantid.LTM
#residuals look fine so no big deal to log transform
plot(qqnorm(resid(mantid.LTM)))

#Time to leave circle model
mantid.TTLC <- lmer(bold.TTLC.log ~ 1 + species + sex + mass.c + (1|id),
	data = mantids)
plot(qqnorm(resid(mantid.TTLC)))
mantid.TTLC
#Time to shelter model
mantid.TTS <- lmer(bold.TTS.log ~ 1 + species + sex + mass.c + (1|id),
	data = mantids)
plot(qqnorm(resid(mantid.TTS)))
#Straight to shelter model
mantid.shelter <- glmer(bold_straight_to_shelter~ 1 + species + sex + mass.c + (1|id),
	data = mantids, family = binomial)

plot(qqnorm(resid(mantid.shelter)))

#Approach model
mantid.approach <- lmer(agg_approach ~ 1 + species + sex + mass.c + (1|id),
	data = mantids)
plot(qqnorm(resid(mantid.approach)))

#Strike model
mantid.strike <- lmer(agg_strike ~ 1 + species + sex + mass.c + (1|id),
	data = mantids)

plot(qqnorm(resid(mantid.strike)))


#Voracity models

eaten.10 <- glmer(eaten ~ 1 + species + sex + mass.c + (1|id) + (1|res),
               data = mantids, family = poisson)



eaten <- glmer(eaten ~ 1 + (1|id),
               data = mantids, family = poisson)

eaten.1 <- glmer(eaten ~ 1 + species + (1|id),
                 data = mantids, family = poisson)

eaten.2 <- glmer(eaten ~ 1 + sex + (1|id),
                 data = mantids, family = poisson)

eaten.3 <- glmer(eaten ~ 1 + mass.c + (1|id),
                 data = mantids, family = poisson)

eaten.4 <- glmer(eaten ~ 1 + bold.LTM.log + (1|id),
                 data = mantids, family = poisson)

eaten.5 <- glmer(eaten ~ 1 + bold.TTLC.log + (1|id),
                 data = mantids, family = poisson)

eaten.6 <- glmer(eaten ~ 1 + bold.TTS.log + (1|id),
                 data = mantids, family = poisson)

eaten.7 <- glmer(eaten ~ 1 + agg_approach + (1|id),
                 data = mantids, family = poisson)

eaten.8 <- glmer(eaten ~ 1 + agg_strike + (1|id),
                 data = mantids, family = poisson)

eaten.9 <- glmer(eaten ~ 1 + loss.p.c + (1|id),
                 data = mantids, family = poisson)

eaten.11<-glmer(eaten~ 1 + bold.TTS.log + species + (1|id),
                data= mantids, family = poisson)

eaten.12<-glmer(eaten ~ 1 + bold.TTLC.log + sex + (1|id),
                data= mantids, family = poisson)

eaten.13<-glmer(eaten ~ 1 + agg_strike + sex + (1|id),
                data= mantids, family = poisson)

eaten.14<-glmer(eaten ~ 1 + mass.c  + sex + (1|id),
                data= mantids, family = poisson)

eaten.15<-glmer(eaten ~ 1 + agg_strike + sex + mass.c + (1|id),
                data= mantids, family = poisson)

eaten.16<-glmer(eaten ~ 1 + mass.c + sex + species + (1|id),
                data= mantids, family = poisson)

eaten.17<-glmer(eaten ~ 1 + sex + bold.LTM.log + (1|id),
                data= mantids, family = poisson)

eaten.18<-glmer(eaten ~ 1 + bold.LTM.log + mass.c + (1|id),
                data= mantids, family = poisson)

eaten.19<-glmer ( eaten ~ 1 + sex + bold.LTM.log + mass.c + (1|id),
                  data=mantids, family = poisson)

eaten.20<- glmer ( eaten ~ 1 + agg_approach + bold.LTM.log + (1|id),
                    data= mantids, family = poisson)

eaten.21<-glmer(eaten ~ 1 + bold.TTLC.log + mass.c + (1|id),
                data= mantids, family = poisson)

eaten.22<-glmer(eaten ~ 1 + bold.TTS.log + mass.c + (1|id),
                data= mantids, family = poisson)

eaten.24<- glmer (eaten~ 1 + mass.c + species + (1|id),
                  data=mantids, family = poisson)

eaten.23<-glmer(eaten ~ 1 + bold.LTM.log + bold.TTS.log+ bold.TTLC.log + mass.c + (1|id),
                data= mantids, family = poisson)



AICctab(eaten, eaten.1, eaten.2, eaten.3, eaten.4, 
        eaten.5,eaten.6,eaten.7, eaten.8, eaten.9,
        eaten.10, eaten.11, eaten.12, eaten.13, eaten.14, 
        eaten.15, eaten.16, eaten.17, eaten.18, eaten.19,
        eaten.20, eaten.21, eaten.22, eaten.23, eaten.24,
        nobs = 142, base=TRUE, weights=TRUE)

eaten.3

eaten.18

eaten.22

cor.test(mantids$mass.c,mantids$bold.LTM.log)
