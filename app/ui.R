
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
              choices=c("Latency to move", "Time to leave circle", 
                        "Time to reach shelter", "Latency to approach novel prey", 
                        "Time to strike novel prey", "Number of prey items eaten")),
      mainPanel(
        plotOutput("behavior"),
        hr(),
        plotOutput("variation")
      )
)
)
)


