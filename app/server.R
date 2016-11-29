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
  
  model<-paste ("m",x,sep="")
  get
}



#Solution 2

#call the model
#paste ("m",x,sep="") <---create's a character that has same name has variable used for model
#To get this model of that name, use function get() 




#Based on what you choose in selectInput
#renderPlot(barplot(input$choose))
#The mean and variation of that model is pulled

#The mean and variation of the model is pulled
#These values are used to create a barplot with mean value and error bars representing variation
#Binn the 