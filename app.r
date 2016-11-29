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
mantids$id <- as.factor(mantids$id)
mantids$trial <- as.factor(mantids$trial)
mantids$res <- as.factor(seq(1,length(mantids$id), 1))
mantids$loss.p <- (mantids$start_mass - mantids$max_mass)/mantids$max_mass
mantids$loss.p.c <- (mantids$loss.p - mean(mantids$loss.p, na.rm=TRUE))/(sd(mantids$loss.p, na.rm=TRUE))


ui <- fluidPage(
  selectInput(inputId = "choose",
              label="Choose Behavior",
              choices=c("Latency to move", "Time to leave circle", 
                        "Time to reach shelter", "Latency to approach novel prey", 
                        "Time to strike novel prey", "Number of prey items eaten")),
  plotOutput("behavior")
  )

server <- function(input, output) {}


shinyApp(ui = ui, server = server)