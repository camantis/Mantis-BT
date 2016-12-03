server <- function(input, output) {
  
  
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
  
  
  inde<-reactive({
    return(which(behavList==input$choose))
  })
  
  trial<-reactive({
    c(turb(which(behavList==input$choose)))
  })
  
output$behavior<-renderPlot({
  barplot(trial(),main=paste0("Behavioral mean and variation of ", input$choose))
}
)

}


#fetchModel(input$choose)
#plot(qqnorm(resid(mantid.TTLC)))
