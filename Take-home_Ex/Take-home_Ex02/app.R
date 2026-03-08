library(shiny)
library(tidyverse)
library(plotly)
library(bslib)
library(ggridges)
library(corrplot)
library(ggstatsplot)
library(treemap)
library(survival)
library(survminer)
library(networkD3)

customer <- read.csv("data/customer_data.csv")
transactions <- read.csv("data/transactions_data.csv")

pred_churn <- lm(churn_probability ~ age + support_tickets_count + tx_count + satisfaction_score, data = customer)
pred_clv <- lm(customer_lifetime_value ~ age + support_tickets_count + tx_count + satisfaction_score, data = customer)
pred_login <- lm(app_logins_frequency ~ age + support_tickets_count + tx_count + satisfaction_score, data = customer)


ui <- navbarPage(

    title = "FinRetain: Customer Visual Analytics",
    
    tabPanel("Exploratory Data (EDA)",
             
             page_sidebar(
               
               sidebar = sidebar("Filters",
                                 
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
                                             selected = "All")),
               
               layout_columns(
                 
                 card("Customer Income",
                      plotlyOutput("barPlot")),
                 
                 card("Age by Education",
                      plotlyOutput("boxPlot")),
                              
                 card("Customer Tenure Density by Segment",
                      plotlyOutput("ridgelinePlot")),
                 
                 col_widths = c(6, 6, 12),
                 row_heights = c(1, 1)
                 
                 )
             )
             ),
    
    tabPanel("Confirmatory Data (CDA)",
             
             layout_columns(
               
               card("Macro Correlation Matrix",
                    plotOutput(outputId = "corrPlot")),
                            
               card("Statistical Hypothesis Testing (Spearman)",
                    
                    selectInput(inputId = "facet_var_filter",
                                label = "Facet Variable",
                                choices = c("income_bracket",
                                            "education_level"),
                                selected = "income_bracket"),
                    
                    plotOutput(outputId = "facetCorrPlot")),
               
               col_widths = c(6, 6),
               row_heights = c(1)
               )
             ),
    
    tabPanel("Customer Segmentation",
             
             layout_columns(
               
               card("Algorithmic Personas (K-Means Clustering)",
                    plotOutput("clusterPlot")),
                            
               card("Value Stratification (Treemap)",
                    plotOutput("treemapPlot")),
                            
               col_widths = c(6, 6),
               row_heights = c(1)
               )
             ),
    
    tabPanel("Survival Timeline",
             
             page_sidebar(
               
               sidebar = sidebar("Survival Parameters",
                                 
                                 selectInput(inputId = "survival_para",
                                             label = "Grouping Variable",
                                             choices = c("Acquisition Channel" = "acquisition_channel",
                                                         "Income Bracket" = "income_bracket",
                                                         "Customer Segment" = "customer_segment"),
                                             selected = "Aquisition Channel")
                                 ),
               
               card("Kaplan-Meier Churn Probability Curve",
                    plotOutput("survivalPlot"))
               )
             ),
             
    tabPanel("Ecosystem Cash Flow",
             
             card("Macro Liquidity Flow ($)",
                  sankeyNetworkOutput("cashflowPlot"))
             ),
    
    tabPanel("Predictive Risk Simulator",
             
             page_sidebar(
               
               sidebar = sidebar("Customer Behavior Levers",
                                 
                                 sliderInput(inputId = "age_slider",
                                             label = "Age",
                                             min = 20, max = 80, value = 30, step = 1),
                                 
                                 sliderInput(inputId = "support_slider",
                                             label = "Support Tickets Count",
                                             min = 0, max = 10, value = 5, step = 1),
                                 
                                 sliderInput(inputId = "tx_slider",
                                             label = "Transaction Count",
                                             min = 0, max = 1000, value = 100, step = 50),
                                 
                                 sliderInput(inputId = "sat_slider",
                                             label = "Satisfaction Score",
                                             min = 1, max = 10, value = 5, step = 1)
                                 ),
               layout_columns(
                 
                 value_box(title = "Predicted Churn Risk",
                           value = textOutput("churn_prediction"),
                           showcase = bsicons::bs_icon("exclamation-triangle"),
                           theme = "danger"),
                 
                 value_box(title = "Predicted CLV",
                           value = textOutput("clv_prediction"),
                           showcase = bsicons::bs_icon("graph-up"),
                           theme = "teal"),
                 
                 value_box(title = "Predicted App Logins Frequency",
                           value = textOutput("login_prediction"),
                           showcase = bsicons::bs_icon("bar-chart"),
                           theme = "purple"),
                 
                 value_box(title = "Recommended Action",
                           value = textOutput("action_prediction"),
                           showcase = bsicons::bs_icon("pie-chart"),
                           theme = "pink"),
                 
                 col_widths = c(6, 6, 6, 6),
                 row_heights = c(1, 1)
                 )
             )
    )
)

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
    
    plot_data$income_bracket <- factor(plot_data$income_bracket,
                                       levels = c("Low",
                                                  "Medium",
                                                  "High",
                                                  "Very High"))
    
    p <- ggplot(plot_data,
                aes(x = income_bracket,
                    fill = income_bracket)) +
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
    
    plot_data$education_level <- factor(plot_data$education_level,
                                        levels = c("High School",
                                                   "Bachelor",
                                                   "Master",
                                                   "PhD"))
    
    p <- ggplot(plot_data, aes(x = education_level,
                               y = age,
                               fill = education_level)) +
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
  
  output$ridgelinePlot <- renderPlotly({
    
    plot_data <- customer
    
    if (input$gender_filter != "All") {
      plot_data <- plot_data %>% 
        filter(gender == input$gender_filter)
    }
    
    if (input$segment_filter != "All") {
      plot_data <- plot_data %>% 
        filter(customer_segment == input$segment_filter)
    }
    
    plot_data$clv_segment <- factor(plot_data$clv_segment,
                                    levels = c("Bronze",
                                               "Silver",
                                               "Gold",
                                               "Platinum"))
    
    p <- ggplot(plot_data,
                aes(x = customer_tenure,
                    y = clv_segment,
                    fill = clv_segment)) +
      geom_density_ridges(alpha = 0.7,
                          scale = 1.5,
                          rel_min_height = 0.01) +
      scale_fill_viridis_d(option = "D",
                           guide = "none") +
      labs(x = "Customer Tenure (Months)",
           y = "Segment") +
      theme_minimal() +
      theme(axis.title = element_text(size = 10),
            axis.text = element_text(size = 10),
            plot.title = element_text(size = 15),
            legend.position = "none")
    ggplotly(p)
  })
  
  output$corrPlot <- renderPlot({
    
    plot_data <- customer %>%
      select(age,
             app_logins_frequency,
             tx_count,
             total_tx_volume,
             satisfaction_score,
             support_tickets_count,
             customer_tenure,
             churn_probability)
    
    customer.cor <- cor(plot_data)
    
    corrplot.mixed(customer.cor,
                   lower = "ellipse",
                   upper = "number",
                   tl.pos = "lt",
                   diag = "l",
                   tl.col = "black")
  })
  
  output$facetCorrPlot <- renderPlot({
    
    plot_data <- customer
    
    plot_data$income_bracket <- factor(plot_data$income_bracket,
                                       levels = c("Low",
                                                  "Medium",
                                                  "High",
                                                  "Very High"))
    
    plot_data$education_level <- factor(plot_data$education_level,
                                        levels = c("High School",
                                                   "Bachelor",
                                                   "Master",
                                                   "PhD"))
    
    ggscatterstats(data = plot_data,
                   x = tx_count,
                   y = satisfaction_score,
                   type = "nonparametric") +
      facet_wrap(input$facet_var_filter) +
      theme_minimal() +
      theme(axis.title = element_text(size = 10),
            axis.text = element_text(size = 10))
  })
  
  output$clusterPlot <- renderPlot({
    
    cluster_data <- customer %>%
      select(tx_count, total_tx_volume)
    
    kmeans_result <- kmeans(scale(cluster_data), centers = 3)
    
    cluster_data$cluster <- paste("Persona", kmeans_result$cluster)
    
    ggplot(cluster_data, aes(x = tx_count, y = total_tx_volume, color = cluster)) +
      geom_point(alpha = 0.6, size = 2) +
      scale_color_manual(values = c("Darkblue", "Darkcyan", "Red")) +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal() +
      labs(x = "Transaction Count",
           y = "Total Volume") +
      theme_minimal() +
      theme(axis.title = element_text(size = 15),
            axis.text = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15))
  })
  
  output$treemapPlot <- renderPlot({
    plot_data <- customer %>%
      group_by(customer_segment, clv_segment) %>%
      summarise(median_tx_count = median(tx_count),
                median_tx_volume = median(total_tx_volume))
    treemap(plot_data,
            index=c("clv_segment", "customer_segment"),
            vSize="median_tx_count",
            vColor="median_tx_volume",
            type="value",
            palette="RdYlBu",
            title="Transaction Count (Size) and Volume (Colour)",
            title.legend = "Median Transcation Volume"
    )
  })
  
  output$survivalPlot <- renderPlot({
    
    plot_data <- customer %>%
      mutate(is_churned = ifelse(customer_segment == "inactive" | churn_probability > 0.8, 1, 0))
    
    if (input$survival_para == "acquisition_channel"){
      km_fit <- survfit(Surv(customer_tenure, is_churned) ~ acquisition_channel, data = plot_data)
    }
    
    if (input$survival_para == "income_bracket"){
      km_fit <- survfit(Surv(customer_tenure, is_churned) ~ income_bracket, data = plot_data)
    }

    if (input$survival_para == "customer_segment"){
      km_fit <- survfit(Surv(customer_tenure, is_churned) ~ customer_segment, data = plot_data)
    }

    p <- ggsurvplot(km_fit,
               data = plot_data,
               risk.table = TRUE,
               pval = TRUE,
               conf.int = TRUE,
               palette = "Set1",
               xlim = c(0, 12),
               title = paste0("Retention Probability by", input$survival_para),
               legend.title = "Strata",
               ggtheme = theme_minimal())
    
    p$plot <- p$plot +
      theme(axis.title = element_text(size = 15),
            axis.text = element_text(size = 15),
            plot.title = element_text(size = 15),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12))
    
    p$table <- p$table + 
      theme(axis.title = element_text(size = 15),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 15))
    
    p
  })
  
  output$cashflowPlot <- renderSankeyNetwork({
    
    tx_summary <- transactions %>%
      group_by(type) %>%
      summarise(total_volume = sum(amount, na.rm = TRUE))
    
    links <- data.frame(source = c("Deposit", "App Wallet", "App Wallet", "App Wallet"),
                        target = c("App Wallet", "Payment", "Transfer", "Withdrawal"),
                        type = c("Deposit", "Payment", "Transfer", "Withdrawal")) %>%
      left_join(tx_summary, by = "type") %>%
      rename(value = total_volume)
    
    node_totals <- data.frame(name = unique(c(links$source, links$target))) %>%
      rowwise() %>%
      mutate(total = max(sum(links$value[links$source == name]), sum(links$value[links$target == name])),
             label = paste0(name, " ($", format(round(total, 0), big.mark = ","), ")"))
    
    links$IDsource <- match(links$source, node_totals$name) - 1
    links$IDtarget <- match(links$target, node_totals$name) - 1
    
    sankeyNetwork(Links = links, Nodes = node_totals, Source = "IDsource", Target = "IDtarget",
                  Value = "value", NodeID = "label", fontSize = 14, nodeWidth = 40, 
                  nodePadding = 20, sinksRight = FALSE, 
                  colourScale = JS("d3.scaleOrdinal().range(['#1c2541', '#0077b6', '#00b4d8', '#90e0ef']);"))
  })
  
  output$churn_prediction <- renderText({

    newdata <- data.frame(age = input$age_slider,
                          tx_count = input$tx_slider,
                          support_tickets_count = input$support_slider,
                          satisfaction_score = input$sat_slider)
    
    res <- predict(pred_churn, newdata = newdata)
    
    scales::percent(max(0, min(1, res)))
  })
  
  output$clv_prediction <- renderText({
    
    newdata <- data.frame(age = input$age_slider,
                          tx_count = input$tx_slider,
                          support_tickets_count = input$support_slider,
                          satisfaction_score = input$sat_slider)
    
    res <- predict(pred_clv, newdata = newdata)
    
    format(round(res, 0), big.mark = ",")
  })
  
  output$login_prediction <- renderText({
    
    newdata <- data.frame(age = input$age_slider,
                          tx_count = input$tx_slider,
                          support_tickets_count = input$support_slider,
                          satisfaction_score = input$sat_slider)
    
    res <- predict(pred_login, newdata = newdata)
    
    format(round(res, 0), big.mark = ",")
  })
  
  output$action_prediction <- renderText({
    
    newdata <- data.frame(age = input$age_slider,
                          tx_count = input$tx_slider,
                          support_tickets_count = input$support_slider,
                          satisfaction_score = input$sat_slider)
    
    risk <- predict(pred_churn, newdata = newdata)
    sat <- input$sat_slider
    
    action <- case_when(
      risk > 0.6 & sat < 5 ~ "Priority: Manager Call",
      risk > 0.6 & sat >= 5 ~ "Offer: Loyalty Discount",
      risk <= 0.6 & risk > 0.3 ~ "Send: Educational Email",
      TRUE ~ "Status: Healthy (Monitor)")
      
    action
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
