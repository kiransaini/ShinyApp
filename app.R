library("shiny")
library("readxl")
data_vitc <- read_excel("life_exp.xlsx", sheet = 1)
data_race <- read_excel("life_exp.xlsx", sheet = 2)
data_gender <- read_excel("life_exp.xlsx", sheet = 3)
data_diet <- read_excel("life_exp.xlsx", sheet = 4)
# Define UI for app that draws a barplot ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("A Study of factors that influence life expectancy in individuals"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Dropdown for factors ----
      selectInput(
        "factor", "Select a factor",
        c("Race" = "Race",
          "Gender" = "Gender",
          "Vitamin C Consumption" = "Vitamin C Consumption(mg)",
          "Diet" = "Diet")
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Bar plot ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Bar Plot of Life Expectancy ----
  # with requested factor
  output$distPlot <- renderPlot({
    
    if(identical(input$factor,"Race")){
      x <- data_race$Race
      y <- data_race$`Life Expectancy`
    }
    if(identical(input$factor,"Gender")){
      x <- data_gender$Gender
      y <- data_gender$`Life Expectancy`
    }
    if(identical(input$factor,"Vitamin C Consumption(mg)")){
      x <- data_vitc$`Consumption of Vitamin C`
      y <- data_vitc$`Life Expectancy`
    }
    if(identical(input$factor,"Diet")){
      x    <- data_diet$Diet
      y    <- data_diet$`Life Expectancy`
    }
    
    
    barplot(y, col = "#75AADB", border = "white",
            xlab = input$factor,
            ylab = "Life Expectancy",
            names.arg = x,
            main = paste("Bar plot of Life Expectancy vs.",input$factor,sep=" "))
    
  })
  
}


shinyApp(ui = ui, server = server)