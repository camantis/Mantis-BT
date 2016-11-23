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


# first a DF of just behavior and web

mantid <- read.csv("C:/Users/Nick/Google Drive/Cameron_mantids/mantids.csv", na = 'NA')

mantid$ID <- as.factor(mantid$ID)
mantid$trial <- as.factor(mantid$trial.number)

# add OLRE
mantid$res <- as.factor(seq(1,length(mantid$ID), 1))

mantid$loss.p <- (mantid$starting.mass - mantid$max.mass)/mantid$max.mass
mantid$loss.p.c <- (mantid$loss.p - mean(mantid$loss.p, na.rm=TRUE))/(sd(mantid$loss.p, na.rm=TRUE))

# boldness measures are skewed, log transform
mantid$bold.LTM.log <- log(mantid$bold.LTM)
mantid$bold.TTLC.log <- log(mantid$bold.TTLC)

# one zero in TTS measurement which is messing with the log transformation
mantid$bold.TTS <- mantid$bold.TTS + 1
mantid$bold.TTS.log <- log(mantid$bold.TTS)

# center mass
mantid$mass.c <- (mantid$starting.mass - mean(mantid$starting.mass, na.rm=TRUE))/sd(mantid$starting.mass, na.rm=TRUE)

mantid$max.mass.c <- (mantid$max.mass - mean(mantid$max.mass, na.rm=TRUE))/sd(mantid$max.mass, na.rm=TRUE)


# quick pearson's - everything is correlated
cor.test(mantid$bold.LTM.log, mantid$agg.approach)
cor.test(mantid$bold.TTLC.log, mantid$agg.approach)
cor.test(mantid$bold.TTS.log, mantid$agg.approach)
cor.test(mantid$bold.LTM.log, mantid$agg.strike)
cor.test(mantid$bold.TTLC.log, mantid$agg.strike)
cor.test(mantid$bold.TTS.log, mantid$agg.strike)


mantid.LTM <- lmer(bold.LTM.log ~ 1 + species + sex + mass.c + (1|ID),
	data = mantid)

#residuals look fine so no big deal to log transform
plot(qqnorm(resid(mantid.LTM)))

mantid.TTLC <- lmer(bold.TTLC.log ~ 1 + species + sex + mass.c + (1|ID),
	data = mantid)

mantid.TTS <- lmer(bold.TTS.log ~ 1 + species + sex + mass.c + (1|ID),
	data = mantid)

mantid.shelter <- glmer(bold.STS~ 1 + species + sex + mass.c + (1|ID),
	data = mantid, family = binomial)

mantid.approach <- lmer(agg.approach ~ 1 + species + sex + mass.c + (1|ID),
	data = mantid)

mantid.strike <- lmer(agg.strike ~ 1 + species + sex + mass.c + (1|ID),
	data = mantid)


eaten <- glmer(eaten ~ 1 + species + sex + mass.c + (1|ID) + (1|res),
	data = mantid, family = poisson)



eaten <- glmer(eaten ~ 1 + (1|ID),
	data = mantid, family = poisson)

eaten.1 <- glmer(eaten ~ 1 + species + (1|ID),
	data = mantid, family = poisson)

eaten.2 <- glmer(eaten ~ 1 + sex + (1|ID),
	data = mantid, family = poisson)

eaten.3 <- glmer(eaten ~ 1 + mass.c + (1|ID),
	data = mantid, family = poisson)

eaten.4 <- glmer(eaten ~ 1 + bold.LTM.log + (1|ID),
	data = mantid, family = poisson)

eaten.5 <- glmer(eaten ~ 1 + bold.TTLC.log + (1|ID),
	data = mantid, family = poisson)

eaten.6 <- glmer(eaten ~ 1 + bold.TTS.log + (1|ID),
	data = mantid, family = poisson)

eaten.7 <- glmer(eaten ~ 1 + agg.approach + (1|ID),
	data = mantid, family = poisson)

eaten.8 <- glmer(eaten ~ 1 + agg.strike + (1|ID),
	data = mantid, family = poisson)

eaten.9 <- glmer(eaten ~ 1 + loss.p.c + (1|ID),
	data = mantid, family = poisson)



AICctab(eaten, eaten.1, eaten.2, eaten.3, eaten.4, eaten.5,eaten.6,eaten.7, eaten.8, eaten.9,  nobs = 142, base=TRUE, weights=TRUE)





eaten <- glmer(eaten ~ 1 + species + sex + mass.c + (1|ID),
	data = mantid, family = poisson)

