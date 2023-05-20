source("data.R")

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("TCGA Cancer Data"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Stages", "Select Stage:", 
                  choices = c("Stage I", "Stage II", "Stage III", "Stage IV"),
                  multiple = TRUE,
                  selected = c("Stage I", "Stage II", "Stage III", "Stage IV")),
      hr(),
      helpText("Data from the TGCA Dataset for 371 cancer patients")
    ),
    
    # Create a spot for the plots
    mainPanel(
      tableOutput("vital_Status"),
      plotOutput("suvival_plot"),
      plotOutput("coxph_plot"),
      plotOutput("heat_map"),
      plotOutput("genderPlot")
    )
    
  )
)