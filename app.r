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


#SUBSET SL SPECIES
sldata <- subset(mantids, species=="S.l",
                 select=id:max.mass.c)


#SUBSET MR SPECIES
mrdata<-subset(mantids,species=="M.r",
               select=id:max.mass.c)

#Models for variance

#Eaten model
sleat<-glmer(eaten~1+sex+mass.c+(1|id),data=sldata,family=poisson)

mreat<-glmer(eaten~1+sex+mass.c+(1|id),data=mrdata,family=poisson)

#LTM
sl.ltm <- lmer(bold.LTM.log ~ 1 + sex + mass.c + (1|id),
                   data = sldata)
mr.ltm <- lmer(bold.LTM.log ~ 1  + sex + mass.c + (1|id),
               data = mrdata)

#LTLC
sl.ltlc<-lmer(bold.TTLC.log ~ 1  + sex + mass.c + (1|id),
              data = sldata)

mr.ltlc<-lmer(bold.TTLC.log ~ 1  + sex + mass.c + (1|id),
              data = mrdata)

#TTS

sl.tts<-lmer(bold.TTS.log ~ 1  + sex + mass.c + (1|id),
             data = sldata)
  
mr.tts<-lmer(bold.TTS.log ~ 1  + sex + mass.c + (1|id),
             data = mrdata)


#Approach models
sl.app<- lmer(agg_approach ~ 1 + sex + mass.c + (1|id),
        data = sldata)

mr.app<-lmer(agg_approach ~ 1  + sex + mass.c + (1|id),
             data = mrdata)
#Strike models
sl.st<- lmer(agg_strike ~ 1 + sex + mass.c + (1|id),
             data = sldata)
  
mr.st<- lmer(agg_strike ~ 1 + sex + mass.c + (1|id),
             data = mrdata)

#BOOTSTRAP SUCKAS
calc.icc<-function(y){
  sumy<-summary(y)
  (sumy$varcor$id[1])/(sumy$varcor$id[1]+sumy$sigma^2)
}


##VARIANCE
test.icc<-function(y){
  sumy<-summary(y)
  (sumy$varcor$id[1])
}





ui <- fluidPage(
  #Add theme to fluidpage 
  selectInput(inputId = "choose",
              label="Choose Behavior",
              choices=c("Latency to move", "Time to leave circle", 
                        "Time to reach shelter", "Latency to approach novel prey", 
                        "Time to strike novel prey", "Number of prey items eaten")),
  plotOutput("behavior")
  #Need to add input for if you want to log transform or not
  )

server <- function(input, output) {
  ltmbar<-renderPlot()
}





#Based on what you choose in selectInput
#renderPlot(barplot(input$choose))
#The model for that behavior is created

#The mean and variation of the model is pulled
#These values are used to create a barplot with mean value and error bars representing variation
#Binn the 




shinyApp(ui = ui, server = server)