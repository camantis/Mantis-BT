
ui<-fluidPage(
  
        selectInput(inputID = "choose",
                    label="Choose Behavior",
                    choices=behavList),
     
        plotOutput("behavior")
)




