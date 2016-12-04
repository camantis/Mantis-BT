
ui <- fluidPage(
  #Add theme to fluidpage 
  ##Set up tabs on App
 tabsetPanel(
    #tab 1 connect to database
    tabPanel("Connnect", "Connect to Database"),  
     sidebarLayout(
        sidebarPanel(
          textInput("postgresDBname", "Enter Database Name"),
          textInput("postgresUser", "Enter username"),
          textInput("postgresPort", "Enter Port number"),
          textInput("postgresHost", "Enter host name"),
          passwordInput ("postgresPwd","Enter password"),
          actionButton("postgresConnect", label = "Connect"),
          br(),
          br(),
        textOutput("DoneConnect")
    
    #Tab for histograms comparing populations     
    tabPanel("Copmaring Populations", "However you want to describe it"),
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "choose",
              label="Choose Behavior",
              choices=behavList)),
      mainPanel(
        textOutput("text1"),
        plotOutput("behavior")
        
   #Tab for scatter plots comparing behaviors   
   tabPanel("Searching for Syndromes", "Check two behaviors to look for syndromes"),
   sidebarLayout(
      sidebarPanel(
        br(),  
        checkboxGroupInput("behaviors", "Behaviors:", choices = behavList),
        br(),
        br()
      ),
      mainPanel(
        hr(),
        ##show plot
        plotOutput(OutputId = "synscatter")
      )
  
 
)
)


