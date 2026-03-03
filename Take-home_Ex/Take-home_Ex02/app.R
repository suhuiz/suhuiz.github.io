library(shiny)
library(tidyverse)
library(plotly)
library(bslib)

customer <- read.csv("data/customer_data.csv")
transactions <- read.csv("data/transactions_data.csv")

# Define UI for application that draws a histogram
ui <- page_sidebar(

    # Application title
    title = "FinRetain: Customer Visual Analytics",
    
    # Sidebar with a slider input for number of bins 
    sidebar = sidebar("Filter",
          selectInput(inputId = "gender_filter",
                      label = "Gender",
                      choices = c("All" = "All",
                                  "Male" = "Male",
                                  "Female" = "Female",
                                  "Other" = "Other"),
                      selected = "All"),
          selectInput(inputId = "segment_filter",
                      label = "Customer Segment",
                      choices = c("All" = "All",
                                  "Inactive" = "inactive",
                                  "Occasional" = "occasional",
                                  "Power" = "power",
                                  "Regular" = "regular"),
                      selected = "All")
          ),

    layout_columns(card("Customer Income",
                        plotlyOutput("barPlot", height = "500px")),
                   card("Age by Education",
                        plotlyOutput("boxPlot", height = "500px")),
                   card("Row 2 (Width 12)"),
                   col_widths = c(6, 6, 12),
                   row_heights = c(2, 1))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$barPlot <- renderPlotly({

    plot_data <- customer
    if (input$gender_filter != "All") {
      plot_data <- plot_data %>% 
        filter(gender == input$gender_filter)
    }
    if (input$segment_filter != "All") {
      plot_data <- plot_data %>% 
        filter(customer_segment == input$segment_filter)
    }
    
    plot_data$income_bracket <- factor(plot_data$income_bracket, levels = c("Low", "Medium", "High", "Very High"))
    
    p <- ggplot(plot_data, aes(x = income_bracket, fill = income_bracket)) +
      geom_bar() +
      labs(x = "Income Bracket",
           y = "Number of Customers") +
      theme_minimal() +
      theme(axis.title = element_text(size = 10),
            axis.text = element_text(size = 10),
            plot.title = element_text(size = 15),
            legend.position = "none")
    ggplotly(p)
  })
  
  output$boxPlot <- renderPlotly({
    
    plot_data <- customer
    if (input$gender_filter != "All") {
      plot_data <- plot_data %>% 
        filter(gender == input$gender_filter)
    }
    if (input$segment_filter != "All") {
      plot_data <- plot_data %>% 
        filter(customer_segment == input$segment_filter)
    }
    
    plot_data$education_level <- factor(plot_data$education_level, levels = c("High School", "Bachelor", "Master", "PhD"))
    
    p <- ggplot(plot_data, aes(x = education_level, y = age, fill = education_level)) +
      geom_boxplot() +
      labs(x = "Education Level",
           y = "Age") +
      theme_minimal() +
      theme(axis.title = element_text(size = 10),
            axis.text = element_text(size = 10),
            plot.title = element_text(size = 15),
            legend.position = "none")
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
