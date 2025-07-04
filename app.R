# Load required libraries
library(shiny)
library(ggplot2)
library(shinythemes)
library(readxl)
ai_fear_dataset <- read_excel("ai_fear_dataset.xlsx")

# Build regression model (used in the prediction form)
model <- lm(ai_fear ~ tech_interest + social_media_hours + ai_use_freq, data = ai_fear_dataset)

# Define user interface (UI)
ui <- fluidPage(
  theme = shinytheme("cosmo"),  # Apply Cosmo theme to the app
  tags$head(
    tags$style(HTML("      
      .shiny-output-error { visibility: hidden; }
      .shiny-output-error:before { visibility: hidden; }
      .box-effect:hover { 
        box-shadow: 0 0 15px rgba(0, 123, 255, 0.7);
        transition: box-shadow 0.3s ease-in-out;
      }
      .fade-in {
        animation: fadeIn 2s;
      }
      @keyframes fadeIn {
        0% {opacity:0;}
        100% {opacity:1;}
      }
      body {
        background-color: #f8f9fa;
      }
    "))
  ),
  
  titlePanel("AI Fear Predictor & Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Prediction Form"),
      sliderInput("tech_interest", "Technology Interest (1-10):", min = 1, max = 10, value = 5),
      sliderInput("social_media_hours", "Social Media Usage (hours/day):", min = 0, max = 10, value = 3),
      sliderInput("ai_use_freq", "AI Usage Frequency (per week):", min = 0, max = 7, value = 2),
      actionButton("predict_btn", "Predict", class = "btn btn-primary"),
      hr(),
      h4("Filters:"),
      selectInput("gender_filter", "Gender:", choices = c("All", "Female", "Male")),
      selectInput("user_filter", "AI User Status:", choices = c("All", "User", "Non-User"))
    ),
    
    mainPanel(
      h4("Predicted AI Fear and Comment:"),
      verbatimTextOutput("prediction_result"),
      textOutput("prediction_comment"),
      hr(),
      h4("Summary Info:"),
      fluidRow(
        column(4, div(class = "fade-in", strong("Avg. AI Fear:"), textOutput("avg_fear"))),
        column(4, div(class = "fade-in", strong("Avg. Social Media Hours:"), textOutput("avg_social"))),
        column(4, div(class = "fade-in", strong("AI User Ratio:"), textOutput("user_ratio")))
      ),
      hr(),
      tabsetPanel(
        tabPanel("AI Fear Distribution", 
                 div(class = "box-effect", plotOutput("hist_plot")),
                 br(),
                 textOutput("hist_comment")
        ),
        tabPanel("Gender vs Tech Interest", 
                 div(class = "box-effect", plotOutput("box_plot")),
                 br(),
                 textOutput("box_comment")
        ),
        tabPanel("Social Media vs Fear", 
                 div(class = "box-effect", plotOutput("scatter_plot")),
                 br(),
                 textOutput("scatter_comment")
        ),
        tabPanel("AI Usage Frequency", 
                 div(class = "box-effect", plotOutput("bar_plot")),
                 br(),
                 textOutput("bar_comment")
        ),
        tabPanel("Statistical Test Results",
                 selectInput("test_choice", "Choose a test:", 
                             choices = c(
                               "Gender and AI Fear" = "gender_test",
                               "AI User? Tech Interest" = "chi_test",
                               "Social Media Hours & AI Fear" = "corr_test",
                               "Is Weekly AI Usage Mean = 3?" = "mean_test"
                             )),
                 verbatimTextOutput("test_result"),
                 textOutput("test_comment")
        )
      )
    )
  )
)

# Server logic
declare
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- ai_fear_dataset
    if (input$gender_filter != "All") {
      data <- data[data$gender == input$gender_filter, ]
    }
    if (input$user_filter != "All") {
      user_status <- ifelse(data$ai_use_freq > 0, "User", "Non-User")
      data <- data[user_status == input$user_filter, ]
    }
    return(data)
  })
  
  prediction <- eventReactive(input$predict_btn, {
    Sys.sleep(0.5)
    new_data <- data.frame(
      tech_interest = input$tech_interest,
      social_media_hours = input$social_media_hours,
      ai_use_freq = input$ai_use_freq
    )
    predict(model, newdata = new_data)
  })
  
  output$prediction_result <- renderPrint({ prediction() })
  
  output$prediction_comment <- renderText({
    pred <- prediction()
    if (pred < 5) {
      "Predicted AI fear is low."
    } else if (pred >= 5 & pred <= 7) {
      "Predicted AI fear is at a moderate level."
    } else {
      "Predicted AI fear is high."
    }
  })
  
  output$avg_fear <- renderText({ round(mean(filtered_data()$ai_fear), 2) })
  
  output$avg_social <- renderText({ round(mean(filtered_data()$social_media_hours), 2) })
  
  output$user_ratio <- renderText({
    ratio <- mean(ifelse(filtered_data()$ai_use_freq > 0, 1, 0))
    paste0(round(ratio * 100, 1), "%")
  })
  
  output$hist_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = ai_fear)) +
      geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
      theme_minimal()
  })
  
  output$box_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = gender, y = tech_interest, fill = gender)) +
      geom_boxplot() +
      scale_fill_manual(values = c("pink", "lightblue")) +
      theme_minimal()
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = social_media_hours, y = ai_fear)) +
      geom_point(color = "darkgreen") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal()
  })
  
  output$bar_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = as.factor(ai_use_freq))) +
      geom_bar(fill = "skyblue", color = "black") +
      theme_minimal()
  })
  
  # Chart Comments (Untouched)
  output$hist_comment <- renderText({
    "AI fear distribution is mostly concentrated in mid-high levels. Most participants have fear levels between 6 and 8."
  })
  
  output$box_comment <- renderText({
    "When comparing technology interest by gender, females show slightly higher interest than males."
  })
  
  output$scatter_comment <- renderText({
    "As social media usage increases, AI fear slightly increases. The correlation is positive and statistically significant."
  })
  
  output$bar_comment <- renderText({
    "Most students use AI tools 2-3 times a week. Weekly usage frequency shows variety from low to high."
  })
  
  output$test_result <- renderPrint({
    if (input$test_choice == "gender_test") {
      t.test(ai_fear_dataset$ai_fear ~ ai_fear_dataset$gender)
    } else if (input$test_choice == "chi_test") {
      table_ai <- table(ifelse(ai_fear_dataset$ai_use_freq > 0, "User", "Non-User"),
                        ifelse(ai_fear_dataset$tech_interest > 6, "High", "Low"))
      chisq.test(table_ai)
    } else if (input$test_choice == "corr_test") {
      cor.test(ai_fear_dataset$social_media_hours, ai_fear_dataset$ai_fear)
    } else if (input$test_choice == "mean_test") {
      t.test(ai_fear_dataset$ai_use_freq, mu = 3)
    }
  })
  
  output$test_comment <- renderText({
    if (input$test_choice == "gender_test") {
      "Since p > 0.05, there is no statistically significant difference in AI fear between genders."
    } else if (input$test_choice == "chi_test") {
      "Since p > 0.05, there is no statistically significant relationship between being an AI user and technology interest."
    } else if (input$test_choice == "corr_test") {
      "Since p < 0.05, there is a positive and significant relationship between social media usage and AI fear."
    } else if (input$test_choice == "mean_test") {
      "Since p > 0.05, the average weekly AI usage is not significantly different from 3 hours."
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)