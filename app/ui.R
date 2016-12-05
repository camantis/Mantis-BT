

  #Add theme to fluidpage 
  #Tem
  library(shinythemes)
  ui<- fluidPage(theme=shinytheme("darkly"),
  #Tem
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
    
    tabPanel("welcome","Welcome to the CA Mantis Shiny App"),
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        img(src="cover.jpg"),
        p("Welcome to the CA Mantis shiny app. With this application,
          you will be able to visualize statistical results from a recent
          comparing the behavior of two species of praying mantids"),
        p("The CA Mantis study was designed to determine how the behavioral phenotype
          structure differs between native and invasive species of praying mantids present
          in northern California,"),
        em("Stagmomantis limbata and Mantis religiosa"),
        p(span(em("M. religiosa")), "is recently established and evidence suggests
          it is displacing", span(em("S. limbata")), "from the local habitat."),
        p("In this app, you will be able to look at the compare the results of
the behavioral assays across species to look at both mean and variation of behaviors as 
          well as correlations between behaviors."),
        img(src="s_l.jpg"),
        img(src="hopper.jpg"),
        p("Number of prey items eaten refers to the voracity of individuals, 
          which were allowed to capture as many prey items as possible in a
          15 minute assay."),
        p("'Latency to move', 'time to leave circle', and 'time to reach shelter'
          refer to assays in which mantids were placed in an open, preceived 'risky'
          arena, and were monitored to record the time at which they made their first
movement, exited the area in which they were introduced, and reached shelter."),
        p("Finally, individuals were exposed to a novel prey in the form of a fiddler crab.
          Latency to begin approaching the crab and time it took to ellicit a predatory
          strike was recorded.")
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
        img(src="mantis.jpg")
        hr(),
        plotOutput(OutputID="synscatter",label="syn") 
      )
    )

  tabPanel("Behavioral differences","Compare mean and variation of behavioral traits"),
    sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "choose",
              label="Choose Behavior",
              choices=behavList)),
      mainPanel(
        img(src="male.jpg"),
        textOutput("text1"),
        plotOutput("behavior"),
        hr(),
        plotOutput("variation")
      )
)
)
)
