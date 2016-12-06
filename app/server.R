server <- function(input, output,session) {
  
  
  dbconnection <- eventReactive(input$postgresConnect, { #Event reactive reacts to the input variable. In this case, postgresConnect is the name of the action button.
    #Output of the function is saved as a variable
    if ( is.null(input$postgresHost) | is.null(input$postgresDBname) | is.null(input$postgresUser) | is.null(input$postgresPort) | is.null(input$postgresPwd)) return (NULL) #This states  actually (input$postgresHost==""|)that if one field is missing, function returns nothing, to avoid a bunch of error codes
    drv <- dbDriver("PostgreSQL")  #Stating the driver we're connecting to
    ##disconnect all current connections
    all_cons <- dbListConnections(drv) #If run script a bunch of times, can still have established connections to database
    for(con in all_cons) dbDisconnect(con)
    ##create a single new connection
    con <- dbConnect(drv=drv, dbname = input$postgresDBname, host = input$postgresHost, port = input$postgresPort, user = input$postgresUser, password = input$postgresPwd) #Establish new connection-these are the textbox names
    return(con)
  })
  output$DoneConnect <- renderText({ #Refering to doneconnect, declared in UI. Statement saying if dbconnect is null, do nothing.
    if (is.null(dbconnection())) return(NULL)
    message <- paste0("SUCCESS! Connected to ", input$postgresDBname , " on ", input$postgresUser, "@", input$postgresHost) #If not null (if connected), return an object with this text
    return(message)
  })
  
  #Choose from behavList and call that model
  #Take from index the variance 
  #Take from index the mean of the response variables 

  index<-reactive({
return(which(behavList==input$choose))
  })
  
  
  inde<-reactive({
    return(which(behavList==input$choose))
  })
  
  trial<-reactive({
    c(turb(which(behavList==input$choose)))
  })
  
  curb<-reactive({
    c(fetchModel(which(behavList==input$choose)))
  })
  
  rip<-reactive({
    tnum(which(behavList==input$choose))
  })
  
  kik<-reactive({
    crush(which(behavList==input$choose))
  })
  
  output$variation<-renderPlot({
    ggplot(kik(),aes(rip(),fill=species))+geom_density(alpha=.2)
  })
  
output$behavior<-renderPlot({
  barplot(trial(),names.arg=c("S.limbata","M.religiosa"),ylim=range(c(0,trial()+trial())),main=paste0("Behavioral mean and variation of ", input$choose))
  arrows(x, trial()-curb(), x, trial()+curb(), length=0.05,angle=90,code=3)
})


#observe({
 # z<-input$behaviors

#if(is.null(z))
 # z<-character(0)
#updateCheckboxGroupInput(session,"behaviors2",label=paste("Checkboxgroup label",length(z)),choices=z,selected=z)
#})
#Output function for Lea's Tab    

#nexus<-reactive({
 # return(cbind(collide(which(behavList==input$behaviors))))
#})

#Loading datasets

#SUBTOT<-reactive({
 # dataset<-synlist
#})
#
#getData<-reactive({
  #behvs<-input$behaviors
 # cbind(idtri,behvs)
#})


#output$selectUI2<-renderUI({
 # checkboxGroupInput("behaviors","Behaviors:",
  #                         choices =
   #    c("Number of prey items eaten"="Number of Prey items eaten",
    #        "Latency to move"="Latency to move", "Time to leave circle"="Time to leave circle", 
     #"Time to reach shelter"="Time to reach shelter", "Latency to approach novel prey"="Latency to approach novel prey", 
   #"Time to strike novel prey"="Time to strike novel prey")
 #        )
#})

#ride<-reactive({
 #collide(input$behaviors)
#})

 
#nexus<-reactive({if(length(indi() <=1))return(NULL) 
 # else if(length (indi()>=2))
  #{
    #cratos<-collide(which(behavList==input$behaviors))
   # nexus<-cbind(collide(which(behavList==input$behaviors))) 
    #return(nexus())
    #nexus<-
     # cbind(cratos())
    #return(nexus)
  #}
#nexus<-reactive({
 # cbind(collide(which(behavList==input$behaviors)))
##})


#data_selected<-reactive({
 # cbind(idtri,input$behaviors)})

#data_agg<-reactive({
 # aggregate(data_selected()[,2:4],list(id),mean)})



#cru<-reactive ({
 # plof(nexus())
#})

#cratos<-reactive({
 #collide(which(behavList==input$behaviors))
  
#)

#nexus<-reactive({
 # cbind(cratos())
  
#})

#ru<-reactive({
 # plof(cratos())
#})

#output$syndrome<-renderPlot({
 # synlist<-getData()
  #data_agg<-aggregate(synlist[,2:4],list(id),mean)
  
  #data_agg() %>% 
   # ggplot(aes(ride,x=paste0("Behavior",input$behaviors), y=paste0("Behavior",input$behaviors)))+
    #geom_point()+
    #geom_abline()
#})
indi<-reactive({
  return(which(behavList==input$behaviors))
})

sindi<-reactive({
  return(which(behavList==input$behaviors2))
})




cyborg<-reactive({
  galv(which(behavList==input$behaviors),(which(behavList==input$behaviors2)))
})

#platos<-reactive({
 # collide(sindi())
#})

#unicron<-reactive({
 # sentient(which(behavList==input$behaviors))
#})
#unicron<-reactive({
 # sentient(cratos(),platos())
#})

#cyborg<-reactive({
 # plof(unicron())
#})
output$syndrome<-renderPlot({
cyborg() %>% 
  ggplot(aes(x=Behavior_B, y=Behavior_C))+
  geom_point()+
  geom_abline()
#output$behaviors<-renderPlot({ 
  ##Need to rework data frame in R for output for Searching for syndrome's tab:    
  ##input$behaviors should already have the same colnames as dataframe mantids2
  ##Create a new data frame with just individual, trial, and the two selected behaviors
  #data_selected <- cbind(mantids2[,1:2], mantids2[, input$behaviors])
  ##Create a New data frame which averages each variable per individual 
  #data_agg <- aggregate(data_selected[,3:4], list(data_selected$Individual), mean)
  #colnames(data_agg) <- c("Individual", "Trial", "Behavior_B", "Behavior_C")
  ##Plot this new data frame
  #data_agg %>% 
   # ggplot(aes(x=Behavior_B, y=Behavior_C))+
    #geom_point()+
    #geom_abline()
#
#if(input$action>0 & input$action<=2){
 # isolate({
  #  data_selected <- cbind(idtri, indi())
   # data_agg <- aggregate(data_selected[,2:4], list(id), mean)
    #colnames(data_agg) <- c("Individual", "Trial", "Behavior_B", "Behavior_C")})
##Need to rework data frame in R for output for Searching for syndrome's tab:    
##input$behaviors should already have the same colnames as dataframe mantids2
##Create a new data frame with just individual, trial, and the two selected behaviors
#data_selected <- reactive({
 # cbind(idtri, unicron())
#})
##Create a New data frame which averages each variable per individual 
#data_agg <- aggregate(data_selected()[,2:4], list(id), mean)
#colnames(data_agg) <- c("Individual", "Trial", "Behavior_B", "Behavior_C")
##Plot this new data frame

  
})
    
}




