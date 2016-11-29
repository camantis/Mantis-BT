
ui <- fluidPage(
  #Add theme to fluidpage 
  selectInput(inputId = "choose",
              label="Choose Behavior",
              choices=c("Latency to move", "Time to leave circle", 
                        "Time to reach shelter", "Latency to approach novel prey", 
                        "Time to strike novel prey", "Number of prey items eaten")),
  plotOutput("behavior")
  
)


