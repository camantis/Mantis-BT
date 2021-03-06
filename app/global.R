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
library(ggplot2)
library(RPostgreSQL)

library(dplyr)
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

#subset DataFrame for Lea's Tab with only Inidividual, Trial, and the 6 Behaviors - not sure I did this right!
mantids2 <- cbind(mantids$id, mantids$trial, mantids$eaten, mantids$bold.LTM.log, mantids$bold.TTLC.log, mantids$bold.TTS.log, mantids$agg_approach, mantids$agg_strike)
colnames(mantids2)<-c("id","trial","eaten","bold.LTM.log","bolt.TTLC.log","bold.TTS.log","agg_approach","agg_strike")
#Models for variance


#But I want BOTH models to be called for one choice in order to compare variation between species

#Eaten model
m11<-glmer(eaten~1+sex+mass.c+(1|id),data=sldata,family=poisson)


m12<-glmer(eaten~1+sex+mass.c+(1|id),data=mrdata,family=poisson)




#LTM
m21<- lmer(bold.LTM.log ~ 1  + sex + mass.c + (1|id),
                   data = sldata)
m22<-lmer(bold.LTM.log ~ 1 +  sex + mass.c + (1|id),
          data = mrdata)
#TTLC
m31<-lmer(bold.TTLC.log~1+sex+mass.c+(1|id),data=sldata)

m32<-lmer(bold.TTLC.log~1+sex+mass.c+(1|id),data=mrdata)

#TTS
m41<-lmer(bold.TTS.log~1+sex+mass.c+(1|id),data=sldata)
m42<-lmer(bold.TTS.log~1+sex+mass.c+(1|id),data=mrdata)

#App
m51<-lmer(agg_approach~1+sex+mass.c+(1|id),data=sldata)
m52<-lmer(agg_approach~1+sex+mass.c+(1|id),data=mrdata)


#Strike
m61<-lmer(agg_strike~1+sex+mass.c+(1|id),data=sldata)
m62<-lmer(agg_strike~1+sex+mass.c+(1|id),data=mrdata)

#Set names of choices to an object in global
behavList <- c("Number of prey items eaten", "Latency to move", "Time to leave circle", 
  "Time to reach shelter", "Latency to approach novel prey", 
  "Time to strike novel prey")

#Based on your choice, run this function to get the response values for the behavior
collide<-function(v)
{
  beh<-get(paste("d",v,sep=""))
  return(beh)
}

#We need to combine BOTH behavior values into one

#Can name the function that cbinds the two behaviors

#flip<-collide(2)
#sikh<-collide(3)
#nexus<-cbind(flip,sikh)
#nexus<-cbind(collide(3),collide(4))
#nexus
#Necessary for the collide function
d1<-mantids$eaten
d2<-mantids$bold.LTM.log
d3<-mantids$bold.TTLC.log
d4<-mantids$bold.TTS.log
d5<-mantids$agg_approach
d6<-mantids$agg_strike

synlist<-cbind(d1,d2,d3,d4,d5,d6)
colnames(synlist)<-
  c("Number of prey items eaten", "Latency to move", "Time to leave circle", 
    "Time to reach shelter", "Latency to approach novel prey", 
    "Time to strike novel prey")



#collide<-function(v)
#{
 # beh<-get(paste("d",v,sep=""))
  #return(beh)
#}
#cratos<-collide(1)
#platos<-collide(2)
#unicron<-sentient(cratos,platos)
#unicron
#cyborg<-plof(unicron)
idtri<-cbind(mantids$id,mantids$trial)

#Organizes the data from cratos and combines with idtri
#id




#sim<-plof(platos,cratos)
#sim
#cratos<-collide(6)
#platos<-collide(2)
#unicron<-sentient(cratos,platos)
#cyborg<-plof(unicron)
#cyborg
#cyborg<-plof(unicron)
#data_selected<-cbind(idtri,dupe)
#data_selected
#data_selected[id]
#data_agg<-aggregate(data_selected[,2:4],list(data_selected[id]),mean)
#data_agg
#colnames(data_agg) <- c("Individual", "Trial", "Behavior_B", "Behavior_C")
#data_agg
##Plot this new data frame
#munch %>% 
 #ggplot(aes(x=Behavior_B, y=Behavior_C))+
#geom_point()+
 # geom_abline()

#plof
#dupe<-cbind(cratos,platos)

#ful<-cbind(id,tri)
#fix<-galv(1,2)
#munch<-galv(6,3)
galv<-function(zeta,tron){
  cruton<-collide(zeta)
  suton<-collide(tron)
  centroid<-cbind(cruton,suton)
  data_selected<-cbind(idtri,centroid)
  data_agg<-aggregate(data_selected[,2:4],list(data_selected[,1]),mean)
  colnames(data_agg)<-c("Individual","Trial","Behavior_B","Behavior_C")
  return(rbind(data_agg))
}
#zeta<-collide(1)
#tron<-collide(3)
#collide<-function(v)
#{
# beh<-get(paste("d",v,sep=""))
#return(beh)
#}
sentient<-function(cratos,platos){
  centroid<-cbind(cratos,platos)
  return(centroid)
}
#sentient(cratos,platos)
#unicron
#idtri
#unicron<-sentient(cratos,platos)
plof<-function(unicron){
  data_selected<-cbind(idtri,unicron)
  data_agg<-aggregate(data_selected[,2:4],list(data_selected[,1]),mean)
  colnames(data_agg)<-c("Individual","Trial","Behavior_B","Behavior_C")
  return(rbind(data_agg))
}

#names the results from the plof function
#regal<-data.frame(cratos,platos,id)
#regal
#data_agg<-aggregate(regal[,1:2],list(regal$id),mean)
#data_agg

#data_selected <- data.frame(id,tri,data_agg)
#data_selected
#data_agg <- aggregate.data.frame(data_selected[,3:4], list(data_selected$id), mean)
#data_agg
#check<-data.frame(id,tri,data_agg)
#check
#colnames(data_agg) <- c( "Trial", "Behavior_B", "Behavior_C")
#data_agg
#colnames(data_agg)

#?aggregate







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




#Function for mean of each behavior

turb<-function(numb)
{
species1<-mean(get(paste("m",numb,"3",sep="")))
species2<-mean(get(paste("m",numb,"4",sep="")))
return(c(species1,species2))
}

#Function to get the number of responses per behavior
tnum<-function(n)
{
  count1<-get(paste("m",n,"3",sep=""))
  count2<-get(paste("m",n,"4",sep=""))
  return(c(count1,count2))
}

#Function that creates dataframe of length ready to be used for density plot
crush<-function(k)
{
  tart<-data.frame(eat=get(paste("m",k,"3",sep="")))
  curb<-data.frame(eat=get(paste("m",k,"4",sep="")))
  tart$species<-'sl'
  curb$species<-'mr'
  lvegLengths<-rbind(tart,curb)
  return(lvegLengths)
}





#Function that brings the variance for models associated w/ behavior
fetchModel <- function(behavNb)
{
  model1 <- test.icc(get(paste("m",behavNb,"1",sep="")))
  model2 <- test.icc(get(paste("m",behavNb,"2",sep="")))
  return(c(model1,model2))
}





x<-1:2



m13<-sldata$eaten
m14<-mrdata$eate

m23<-sldata$bold.LTM.log
m24<-mrdata$bold.LTM.log

m33<-sldata$bold.TTLC.log
m34<-mrdata$bold.TTLC.log

m43<-sldata$bold.TTS.log
m44<-mrdata$bold.TTS.log

m53<-sldata$agg_approach
m54<-mrdata$agg_approach

m63<-sldata$agg_strike
m64<-mrdata$agg_strike







