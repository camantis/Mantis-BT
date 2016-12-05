
ui <- fluidPage(
  #Add theme to fluidpage 
  #Tem
  library(shinythemes)
  
  
  
  fluidpage(theme=shinytheme("darkly"),
  
   tabsetPanel(
    tabPanel("Connect","Connect to Database"),
    sidebarLayout(
      sidebarPanel(
        textInput("postgresDBname", "Enter Database Name"),
        textInput("postgresUser","Enter username"),
        textInput("postgresPort","Enter Port number"),
        textInput("postgresHost","Enter host name"),
        passwordInput("postgresPwd","Enter password"),
        actionButton("postgresConnect",label="Connect"),
        br(),
        br(),
        textOutput("DoneConnect")
      )
    )
    
    
    
    #TEMP
    tabPanel("Searching for Syndromes", "Check two behaviors to look for syndromes"),
    sidebarLayout(
      sidebarPanel(
        br(),
        checkboxGroupInput("behaviors","Behaviors:",choices=behavList),
        br(),
        br()
      ),
      mainPanel(
        hr(),
        plotOutput(OutputID="synscatter",label="syn") 
      )
    )
  tabPanel("Behavioral differences","Compare mean and variation of behavioral traits"),
    sidebarLayout(
    sidebarPanel(
      textInput("postgresDBname", "Enter Database Name"),
      textInput("postgresUser", "Enter username"),
      textInput("postgresPort", "Enter Port number"),
      textInput("postgresHost", "Enter host name"),
      passwordInput ("postgresPwd","Enter password"),
      selectInput(inputId = "choose",
              label="Choose Behavior",
              choices=behavList)),
      mainPanel(
        textOutput("text1"),
        plotOutput("behavior"),
        hr(),
        plotOutput("variation")
      )
)
)
)