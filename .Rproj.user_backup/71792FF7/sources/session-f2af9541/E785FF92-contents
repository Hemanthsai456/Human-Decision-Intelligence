# ============================================================
# Human Decision Intelligence
# app.R
#
# Purpose:
#   Shiny interface for exploring the final analytical results.
# ============================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Paths are relative to the project root
project_root <- if (dir.exists("data/processed")) "." else ".."

read_result <- function(file) {
  read.csv(file.path(project_root, "data/processed", file))
}

# Load processed results
model_summary <- read_result("decision_intelligence_model_summary.csv")
factor_summary <- read_result("decision_factor_summary.csv")
statistical_evidence <- read_result("decision_statistical_evidence.csv")
behavioral_profiles <- read_result("decision_behavioral_profiles.csv")
test_predictions <- read_result("final_test_predictions.csv")

if (!"odds_ratio" %in% names(statistical_evidence)) {
  statistical_evidence$odds_ratio <- statistical_evidence$estimate
}

choice_vars <- c("expected_payoff_diff", "probability_diff", "payoff_spread_diff",
                 "low_payoff_diff", "high_lottery_ev_diff", "lottery_outcomes_diff",
                 "ambiguous_b", "payoff_correlation", "full_feedback",
                 "game_order", "trial_number", "time_block")

choice_data <- read_result("cpc18_features.csv") %>% select(chose_b, all_of(choice_vars))

ui <- dashboardPage(
  dashboardHeader(title = "Human Decision Intelligence"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabName = "overview"),
    menuItem("Choice Patterns", tabName = "choice"),
    menuItem("Statistical Evidence", tabName = "statistics"),
    menuItem("Model Performance", tabName = "model"),
    menuItem("Explainability", tabName = "explainability"),
    menuItem("Behavioral Profiles", tabName = "profiles")
  )),
  
  dashboardBody(tabItems(
    
    tabItem("overview",
            h2("Human Decision Intelligence"),
            p("Understanding, predicting, and explaining human choices under risk, ",
              "ambiguity, and feedback."),
            fluidRow(valueBoxOutput("model_box"), valueBoxOutput("accuracy_box"),
                     valueBoxOutput("auc_box"), valueBoxOutput("brier_box")),
            box(width = 12, title = "About this application",
                p("The application presents the main findings from the CPC18 decision-making ",
                  "dataset: how choices vary with decision characteristics, the statistical ",
                  "associations, how well choices can be predicted for new participants, and ",
                  "what the final model relies on."),
                p("All results are associations and predictions, not causal effects."))
    ),
    
    tabItem("choice",
            h2("Choice Patterns"),
            fluidRow(
              box(width = 4, title = "Controls",
                  selectInput("choice_var", "Decision variable", choice_vars),
                  sliderInput("choice_bins", "Bins (continuous variables)", 4, 20, 10, step = 1)),
              box(width = 8, title = "Proportion choosing B", plotOutput("choice_plot"))
            ),
            p("Descriptive only. Error bars treat every choice as independent, so they are ",
              "narrower than the mixed-effects model's uncertainty.")
    ),
    
    tabItem("statistics",
            h2("Statistical Evidence"),
            box(width = 12, title = "Mixed-effects model: odds ratios with 95% CI",
                plotOutput("statistics_plot"),
                p("An odds ratio above 1 means higher odds of choosing B. Continuous ",
                  "variables are per 1 SD."))
    ),
    
    tabItem("model",
            h2("Model Performance"),
            fluidRow(
              box(width = 6, title = "Final model on the test set", tableOutput("model_table")),
              box(width = 6, title = "Threshold explorer",
                  sliderInput("threshold", "Classification threshold", 0.1, 0.9, 0.5, step = 0.05),
                  tableOutput("threshold_table"))
            ),
            fluidRow(
              box(width = 6, title = "Confusion matrix", tableOutput("confusion_table")),
              box(width = 6, title = "Predicted probability of B", plotOutput("probability_plot"))
            )
    ),
    
    tabItem("explainability",
            h2("Model Explainability"),
            fluidRow(
              box(width = 6, title = "Top decision factors (SHAP)",
                  sliderInput("top_n", "Number of features", 3, 15, 8, step = 1),
                  plotOutput("factor_plot")),
              box(width = 6, title = "SHAP summary", imageOutput("shap_image"))
            ),
            p("SHAP values describe how the model uses its inputs, not causal effects.")
    ),
    
    tabItem("profiles",
            h2("Behavioral Profiles"),
            fluidRow(
              box(width = 6, title = "Cluster sizes", plotOutput("profile_plot")),
              box(width = 6, title = "Cluster centers", tableOutput("profile_table"))
            ),
            p("Profiles group observations by decision characteristics; silhouette scores ",
              "were low, so they are regions of a continuum, not distinct types.")
    )
  ))
)

server <- function(input, output, session) {
  
  # Overview
  output$model_box <- renderValueBox(
    valueBox(model_summary$final_model[1], "Final Model", icon = icon("robot")))
  output$accuracy_box <- renderValueBox(
    valueBox(paste0(round(model_summary$accuracy[1] * 100, 1), "%"),
             "Test Accuracy", icon = icon("bullseye")))
  output$auc_box <- renderValueBox(
    valueBox(round(model_summary$roc_auc[1], 3), "ROC-AUC", icon = icon("chart-line")))
  output$brier_box <- renderValueBox(
    valueBox(round(model_summary$brier_score[1], 3), "Brier Score", icon = icon("calculator")))
  
  # Choice patterns
  choice_summary <- reactive({
    x <- choice_data[[input$choice_var]]
    bin <- if (n_distinct(x) <= 10) factor(x) else
      cut(x, unique(quantile(x, seq(0, 1, length.out = input$choice_bins + 1))),
          include.lowest = TRUE)
    
    data.frame(bin = bin, chose_b = choice_data$chose_b) %>%
      group_by(bin) %>%
      summarise(rate = mean(chose_b), n = n(), .groups = "drop") %>%
      mutate(se = sqrt(rate * (1 - rate) / n))
  })
  
  output$choice_plot <- renderPlot({
    ggplot(choice_summary(), aes(x = bin, y = rate, group = 1)) +
      geom_hline(yintercept = mean(choice_data$chose_b), linetype = "dashed", colour = "grey50") +
      geom_line() + geom_point() +
      geom_errorbar(aes(ymin = rate - 1.96 * se, ymax = rate + 1.96 * se), width = 0.2) +
      labs(x = input$choice_var, y = "Proportion choosing B") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Statistical evidence
  output$statistics_plot <- renderPlot({
    ggplot(statistical_evidence, aes(x = odds_ratio, y = reorder(term, odds_ratio))) +
      geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
      geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
      scale_x_log10() +
      labs(x = "Odds ratio (log scale)", y = NULL) +
      theme_minimal()
  })
  
  # Model performance
  output$model_table <- renderTable(
    model_summary %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
  
  confusion <- reactive({
    predicted <- as.integer(test_predictions$probability_b >= input$threshold)
    actual <- test_predictions$actual
    list(tp = sum(predicted == 1 & actual == 1), tn = sum(predicted == 0 & actual == 0),
         fp = sum(predicted == 1 & actual == 0), fn = sum(predicted == 0 & actual == 1))
  })
  
  output$threshold_table <- renderTable({
    with(confusion(), {
      precision <- ifelse(tp + fp == 0, NA, tp / (tp + fp))
      recall <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
      f1 <- ifelse(precision + recall == 0, NA,
                   2 * precision * recall / (precision + recall))
      data.frame(Metric = c("Accuracy", "Precision", "Recall", "F1"),
                 Value = round(c((tp + tn) / (tp + tn + fp + fn), precision, recall, f1), 3))
    })
  })
  
  output$confusion_table <- renderTable({
    with(confusion(), data.frame(Actual = c("Chose A", "Chose B"),
                                 `Predicted A` = c(tn, fn), `Predicted B` = c(fp, tp),
                                 check.names = FALSE))
  }, digits = 0)
  
  output$probability_plot <- renderPlot({
    ggplot(test_predictions,
           aes(x = probability_b, fill = factor(actual, levels = 0:1, labels = c("Chose A", "Chose B")))) +
      geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
      geom_vline(xintercept = input$threshold, linetype = "dashed") +
      labs(x = "Predicted probability of B", y = "Observations", fill = NULL) +
      theme_minimal()
  })
  
  # Explainability
  output$factor_plot <- renderPlot({
    factor_summary %>%
      slice_head(n = input$top_n) %>%
      ggplot(aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
      geom_col() + coord_flip() +
      labs(x = "Feature", y = "Mean Absolute SHAP") +
      theme_minimal()
  })
  
  output$shap_image <- renderImage({
    file <- file.path(project_root, "outputs", "explainability", "shap_summary.png")
    req(file.exists(file))
    list(src = file, contentType = "image/png", width = "100%")
  }, deleteFile = FALSE)
  
  # Behavioral profiles
  output$profile_plot <- renderPlot({
    ggplot(behavioral_profiles, aes(x = factor(cluster), y = observations)) +
      geom_col() +
      labs(x = "Cluster", y = "Observations") +
      theme_minimal()
  })
  
  output$profile_table <- renderTable(
    behavioral_profiles %>% mutate(across(where(is.numeric), ~ round(.x, 2))))
}

shinyApp(ui, server)