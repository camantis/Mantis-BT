

  #Add theme to fluidpage 
  #Tem
  library(shinythemes)
  
  
  
 ui<- fluidPage(theme=shinytheme("darkly"),
  tabPanel("Behavioral differences","Compare mean and variation of behavioral traits"),
    sidebarLayout(
    sidebarPanel(
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
