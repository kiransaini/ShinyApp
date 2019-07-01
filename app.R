knitr::opts_chunk$set(echo = TRUE)
library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(tidyverse)
library(readxl)
library(png)
Sys.setenv("plotly_username"="kiransaini680")
Sys.setenv("plotly_api_key"="sdkfNtk3jRwCHsnDNzc5")
# Read file
data_factors<- read_excel("life_exp2.xlsx", sheet = 2)

#Convert column to type character
data_factors$Vitamin_C_Consumption <- as.character(data_factors$Vitamin_C_Consumption)

#Define dictionary of choices for dropdown 1 with column name and axis name mappings
choices_all_1 <-  c("Race", "Gender", "Vitamin_C_Consumption", "Diet")
names_1 <- c("Race", "Gender", "Vitamin C Consumption (mg)", "Diet")
names(choices_all_1) <- names_1

#Initialise choices in Dropdown 1
choices_1 <- choices_all_1

#Define list of choices for Dropdown 2
choices_all_2 <-  c("None", "Race", "Gender", "Vitamin C Consumption (mg)", "Diet")

#Initialise choices in Dropdown 2
choices_2 <- choices_all_2
# Define UI for app that draws a bar plot ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("A study of factors that influence life expectancy in individuals"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Dropdowns for factors ----
      selectInput(
        "factor1", "Select factor 1",choices_1
        
      ),
      selectInput(
        "factor2", "Select factor 2",choices_2
      ),
      downloadButton('downloadPlots', 'Download Plot')
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Bar plot ----
      plotlyOutput("distPlot")
      
    )
  )
)
# Define server logic for shiny app

server <- function(input, output, session) {
  
  #Reset choices in Dropdown 2 by removing option selected in Dropdown 1 
  choices_2 <- reactive(setdiff(choices_all_2, names(choices_all_1)[match(input$factor1,choices_all_1)]))
  
  #Update Dropdown 2
  observe({
    updateSelectInput(session, "factor2",
                      choices = choices_2()
    )})
  
  plotInput <- reactive({
    
    # Case 1: Only one factor is selected
    if(identical(input$factor2,"None")){
      # Group by factor 1 and find mean of life expectency
      cols2group <- c(input$factor1)
      col2summarize <- "Life_expectancy" 
      data_rel <- data_factors %>% 
        select(input$factor1,"Life_expectancy") %>%
        group_by_at(cols2group) %>% 
        summarize_at(.vars = col2summarize, .funs = mean)
      
      # Define x axis label from dictionary with column name and axis label mappings
      x <- names(choices_all_1)[match(input$factor1,choices_all_1)]
      
      # Define bar plot with x axis - factor 1 and y axis- life expectency
      p <- (ggplotly(
        ggplot(data_rel, aes_string(input$factor1, "Life_expectancy")) + 
          geom_col(fill = "#9ecae1") + 
          
          # Define x axis label, y axis label and title
          xlab(x) +  ylab("Life expectency") + 
          ggtitle(paste("Bar plot of Life Expectancy vs.",x,sep=" "))
      ))
    }
    
    # Case 2: Two factors is selected
    else{
      # Define fill column from dictionary with column name and axis label mappings
      fill <- as.character(choices_all_1[input$factor2])
      
      # Group by factor 1 and find mean of life expectency
      cols2group <- c(input$factor1,fill)
      col2summarize <- "Life_expectancy" 
      data_rel <- data_factors %>% 
        select(input$factor1,fill,"Life_expectancy") %>%
        group_by_at(cols2group) %>% 
        summarize_at(.vars = col2summarize, .funs = mean)
      
      # Define x axis label from dictionary with column name and axis label mappings   
      x<- names(choices_all_1)[match(input$factor1,choices_all_1)]
      
      # Define bar plot with x axis - factor 1, fill- factor 2 and y axis- life expectency  
      p <- (ggplotly(
        ggplot(data_rel, aes_string(x = input$factor1, y = "Life_expectancy", fill=fill)) + 
          geom_col(position="dodge", stat="identity") +  
          
          # Define x axis label, y axis label and title
          xlab(x) +   ylab("Life expectency") + 
          ggtitle(paste("Bar plot of Life Expectancy vs.",x,sep=" ")) +
          labs(fill = str_wrap(input$factor2,10)) +
          theme(legend.position=c(0.85, 0.3),legend.title=element_text(size=7)) +
          #Define color palette
          scale_fill_brewer(palette="Pastel1")
      ))
      
    }
    return(p)
  })
  
  #Define output plot
  output$distPlot <- renderPlotly({
    print(plotInput())
    
  })
  
  output$downloadPlots <- downloadHandler(
    
    filename = function() { paste("test", '.png', sep='') },
    content = function(file) {
      p <- plotInput()
      plotly_IMAGE(p, format = "png", out_file = file)
    }
  )
  
  
}

#Run Shiny App
shinyApp(ui, server)