

  #Add theme to fluidpage 
  #Tem
  library(shinythemes)
  
  ui<- fluidPage(theme=shinytheme("darkly"),
  titlePanel ("CA Mantis"),
  #Tem
  tabsetPanel(
  tabPanel("Connect","Connect to Database",
    sidebarLayout(
      sidebarPanel(
        textInput("postgresDBname", "Enter Database Name"),
        textInput("postgresUser","Enter username"),
        textInput("postgresPort","Enter Port number"),
        textInput("postgresHost","Enter host name"),
        passwordInput("postgresPwd","Enter password"),
        actionButton("postgresConnect",label="Connect"),
        br(),
        br()),
      mainPanel(
        hr(),
        textOutput("DoneConnect")
      ))),
  tabPanel("Welcome","Welcome to the CA Mantis Shiny App",
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      img(src="cover.jpg"),
      p("Welcome to the CA Mantis shiny app. With this application,
        you will be able to visualize statistical results from a recent study
        comparing the behavior of two species of praying mantids"),
      p("The CA Mantis study was designed to determine how the behavioral phenotype
        structure differs between native and invasive species of praying mantids present
        in northern California,", span(em("Stagmomantis limbata and Mantis religiosa")),"and how
        several behavioral traits are correlated with one another."),
      p(span(em("M. religiosa")), "is recently established and evidence suggests
        it is displacing", span(em("S. limbata")), "from the local habitat."),
      img(src="hopper.jpg"),
      p("In this app, you will be able to compare the results of
        the behavioral assays across species to look at both mean and variation of behaviors as 
        well as correlations between behaviors."),
      img(src="s_l.jpg",height=300,width=300),
      
      p("Number of prey items eaten refers to the voracity of individuals, 
        which were allowed to capture as many prey items as possible in a
        15 minute assay."),
      p("'Latency to move', 'time to leave circle', and 'time to reach shelter'
        refer to assays in which mantids were placed in an open, preceived 'risky'
        arena, and were monitored to record the time at which they made their first
        movement, exited the area in which they were introduced, and reached shelter."),
      p("Finally, individuals were exposed to a novel prey in the form of a fiddler crab.
        Latency to begin approaching the crab and time it took to ellicit a predatory
        strike were recorded.")
      ))),
  
  #TEMP
  tabPanel("Searching for Syndromes", "Check two behaviors to look for syndromes",
  sidebarLayout(
    sidebarPanel(
      br(),
      checkboxGroupInput("behaviors","Behaviors:",
                         choices =behavList),
      checkboxGroupInput("behaviors2","Relationship to behavior:",
                         choices =behavList),
      
      br(),
      br()
    ),
    mainPanel(
      hr(),
      plotOutput("syndrome")
    ))),
  
  tabPanel("Behavioral differences","Compare mean and variation of behavioral traits",
  sidebarLayout(
    sidebarPanel(
      br(),
      img(src="male.jpg",height=300,width=300),
      selectInput(inputId = "choose",
              label="Choose Behavior",
              choices=behavList)),
      mainPanel(
        plotOutput("behavior"),
        
        hr(),
        plotOutput("variation")
        
      )))
)
)               
