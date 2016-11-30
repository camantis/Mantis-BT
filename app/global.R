library(shiny)
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
mantids<-as.data.frame.matrix(mantis)
mantids$id <- as.factor(mantids$id)
mantids$trial <- as.factor(mantids$trial)
mantids$res <- as.factor(seq(1,length(mantids$id), 1))
mantids$loss.p <- (mantids$start_mass - mantids$max_mass)/mantids$max_mass
mantids$loss.p.c <- (mantids$loss.p - mean(mantids$loss.p, na.rm=TRUE))/(sd(mantids$loss.p, na.rm=TRUE))


# boldness measures are skewed, log transform

#Latency to move has some zeros

mantids$bold.LTM <- mantids$bold_ltm + 1
mantids$bold.LTM.log <- log(mantids$bold.LTM)



mantids$bold.TTLC.log <- log(mantids$bold_leave_circle)
mantids$bold.TTS.log <- log(mantids$bold_time_shelter)# center mass
mantids$mass.c <- (mantids$start_mass - mean(mantids$start_mass, na.rm=TRUE))/sd(mantids$start_mass, na.rm=TRUE)

mantids$max.mass.c <- (mantids$max_mass - mean(mantids$max_mass, na.rm=TRUE))/sd(mantids$max_mass, na.rm=TRUE)


#SUBSET SL SPECIES
sldata <- subset(mantids, species=="S.l",
                 select=id:max.mass.c)


#SUBSET MR SPECIES
mrdata<-subset(mantids,species=="M.r",
               select=id:max.mass.c)

#Models for variance


#But I want BOTH models to be called for one choice in order to compare variation between species

#Eaten model
m11<-glmer(eaten~1+sex+mass.c+(1|id),data=sldata,family=poisson)

mean(sldata$eaten)
mean(mrdata$eaten)
summary(m11)
m12<-glmer(eaten~1+sex+mass.c+(1|id),data=mrdata,family=poisson)


#LTM
m21 <- lmer(bold.LTM.log ~ 1 + sex + mass.c + (1|id),
           data = sldata)

m22 <- lmer(bold.LTM.log ~ 1  + sex + mass.c + (1|id),
           data = mrdata)

#LTLC
m31<-lmer(bold.TTLC.log ~ 1  + sex + mass.c + (1|id),
         data = sldata)

m32<-lmer(bold.TTLC.log ~ 1  + sex + mass.c + (1|id),
         data = mrdata)

#TTS

m41<-lmer(bold.TTS.log ~ 1  + sex + mass.c + (1|id),
         data = sldata)

m42<-lmer(bold.TTS.log ~ 1  + sex + mass.c + (1|id),
         data = mrdata)


#Approach models
m51<- lmer(agg_approach ~ 1 + sex + mass.c + (1|id),
          data = sldata)

m52<-lmer(agg_approach ~ 1  + sex + mass.c + (1|id),
          data = mrdata)

#Strike models
m61<- lmer(agg_strike ~ 1 + sex + mass.c + (1|id),
           data = sldata)

m62<- lmer(agg_strike ~ 1 + sex + mass.c + (1|id),
           data = mrdata)


##MOdels for mean behavior??
behavList <- c("Latency to move", "Time to leave circle", 
  "Time to reach shelter", "Latency to approach novel prey", 
  "Time to strike novel prey", "Number of prey items eaten")
#BOOTSTRAP for repeatability 
calc.icc<-function(y){
  sumy<-summary(y)
  (sumy$varcor$id[1])/(sumy$varcor$id[1]+sumy$sigma^2)
}


##VARIANCE
test.icc<-function(y){
  sumy<-summary(y)
  (sumy$varcor$id[1])
}

#To plot variance use arrows ()




#pull.model
fetchModel <- function(behavNb)
{
  model1 <- get(paste("m",behavNb,"1",sep=""))
  model2 <- get(paste("m",behavNb,"2",sep=""))
  return(list(model1,model2))
}
