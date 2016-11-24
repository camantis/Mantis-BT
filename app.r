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
View(mantis)
mantids<-as.data.frame.matrix(mantis)
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
LTM<-c(0.4,0.502)
TTLC<-c(0.489,0.192)
TTS<-c(0.28,0.369)
AAP<-c(0.697,0.657)
AAS<-c(0.779,0.660)
EAT<-c(0.192,0.468)

ui <- fluidPage(
  
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)