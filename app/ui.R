
ui <- fluidPage(
  #Add theme to fluidpage 
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
