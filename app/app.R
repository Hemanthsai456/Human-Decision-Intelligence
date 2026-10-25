# ============================================================
# Human Decision Intelligence
# app.R
#
# Purpose:
#   Shiny interface for exploring the final analytical results.
#   This revision redesigns the UI and adds charts to the overview.
#   Existing data files, analytical results, and page logic are retained.
# ============================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Deployment checks
cat("R version:", R.version.string, "\n")
cat("Working directory:", getwd(), "\n")
cat("Files here:", paste(list.files(), collapse = ", "), "\n")

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

choice_vars <- c(
  "Expected payoff difference" = "expected_payoff_diff",
  "Probability difference" = "probability_diff",
  "Payoff spread difference" = "payoff_spread_diff",
  "Low payoff difference" = "low_payoff_diff",
  "High-lottery expected value difference" = "high_lottery_ev_diff",
  "Lottery outcomes difference" = "lottery_outcomes_diff",
  "Ambiguous option B" = "ambiguous_b",
  "Payoff correlation" = "payoff_correlation",
  "Full feedback condition" = "full_feedback",
  "Game order" = "game_order",
  "Trial number" = "trial_number",
  "Time block" = "time_block"
)

choice_data <- read_result("shiny_choice_data.csv") %>%
  # choice_vars is a named display-label -> column-name vector; select its values.
  select(chose_b, all_of(unname(choice_vars)))

# ----------------------------- UI -----------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span("Human Decision Intelligence", class = "brand-title"),
    titleWidth = 310
  ),
  dashboardSidebar(
    width = 245,
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Choice Patterns", tabName = "choice", icon = icon("chart-line")),
      menuItem("Statistical Evidence", tabName = "statistics", icon = icon("sliders-h")),
      menuItem("Model Performance", tabName = "model", icon = icon("bullseye")),
      menuItem("Explainability", tabName = "explainability", icon = icon("brain")),
      menuItem("Behavioral Profiles", tabName = "profiles", icon = icon("users"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      @import url("https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap");
      body, .content-wrapper, .right-side { background: #f3f6fa !important; font-family: Inter, Arial, sans-serif; }
      .main-header .logo { background: #11263d !important; color: #fff !important; font-weight: 800; text-align: left; padding-left: 18px; }
      .main-header .navbar { background: #11263d !important; }
      .main-header .sidebar-toggle { color: #fff !important; }
      .main-sidebar { background: #11263d !important; }
      .sidebar-menu>li>a { color: #dce7f3 !important; font-weight: 600; padding: 15px 10px 15px 18px; }
      .sidebar-menu>li.active>a, .sidebar-menu>li:hover>a { background: #1b3a55 !important; border-left-color: #12b8ad !important; color: #fff !important; }
      .content-wrapper { min-height: 100vh; }
      .content { padding: 18px 22px; }
      .page-heading { color: #132b43; font-size: 30px; font-weight: 800; margin: 2px 0 4px 0; }
      .page-subtitle { color: #4c647c; margin-bottom: 14px; font-size: 15px; line-height: 1.45; }
      .hero-panel { background: #132b43; color: white; border-left: 5px solid #12b8ad; border-radius: 10px; padding: 16px 22px; margin-bottom: 14px; box-shadow: 0 4px 14px rgba(17,38,61,.08); }
      .hero-panel h3 { font-size: 21px; font-weight: 800; margin: 0 0 7px 0; }
      .hero-panel p { color: #f0f5fa; margin: 0; line-height: 1.55; max-width: 1200px; font-size: 14px; }
      .small-box { border-radius: 9px; box-shadow: 0 4px 12px rgba(17,38,61,.08); min-height: 98px; margin-bottom: 14px; }
      .small-box h3 { font-size: 30px; font-weight: 800; }
      .small-box p { font-size: 13px; font-weight: 800; text-transform: none; letter-spacing: .1px; }
      .small-box .icon-large { top: 13px; right: 14px; font-size: 48px; opacity: .18; }
      .box { border-radius: 9px; border-top: 3px solid #dce5ef; box-shadow: 0 4px 14px rgba(17,38,61,.045); margin-bottom: 14px; }
      .box.box-solid { border: 1px solid #dce5ef; }
      .box-header { padding: 12px 15px; border-bottom: 1px solid #edf1f6; }
      .box-title { color: #17324b; font-weight: 800; font-size: 16px; white-space: normal; }
      .box-body { padding: 12px 14px; }
      .section-label { color: #17324b; font-size: 16px; font-weight: 800; margin: 18px 0 10px 0; }
      .workflow-card { background: #fff; border: 1px solid #dce5ef; border-radius: 8px; padding: 16px; min-height: 115px; }
      .workflow-step { color: #119d98; font-size: 11px; font-weight: 800; letter-spacing: 1px; margin-bottom: 8px; }
      .workflow-title { color: #18324a; font-size: 14px; font-weight: 800; margin-bottom: 6px; }
      .workflow-copy { color: #4c647c; font-size: 13px; line-height: 1.55; }
      .insight-note { background: #e4f5f3; border-left: 3px solid #12a69e; border-radius: 5px; padding: 12px 14px; color: #24465a; line-height: 1.55; font-size: 13px; }
      .table { color: #284258; font-size: 13px; }
      .main-footer { display: none; }
      .content-header { display: none; }
      /* Larger chart typography improves browser and slide screenshots. */
      .plot-container { color: #20384f; }
      .chart-caption { color: #405a72; font-size: 12px; line-height: 1.45; margin: 2px 4px 4px; min-height: 34px; }
      @media (min-width: 1200px) { .overview-chart .box-body { padding: 8px 10px; } }
      @media (max-width: 767px) { .content { padding: 12px; } .page-heading { font-size: 24px; } .box-title { font-size: 15px; } }
    '))),
    tabItems(
      # Overview
      tabItem(
        tabName = "overview",
        fluidRow(column(12,
                        h1("Human Decision Intelligence", class = "page-heading"),
                        p("Research dashboard | Human choices under risk, ambiguity and feedback", class = "page-subtitle")
        )),
        fluidRow(column(12,
                        div(class = "hero-panel",
                            h3("Understanding how people make risky choices"),
                            p("Explore observed choice patterns, quantify statistical associations, evaluate predictions on held-out data, and interpret the features used by the final machine-learning model."))
        )),
        fluidRow(
          valueBoxOutput("model_box", width = 3),
          valueBoxOutput("accuracy_box", width = 3),
          valueBoxOutput("auc_box", width = 3),
          valueBoxOutput("brier_box", width = 3)
        ),
        fluidRow(
          box(width = 4, title = "Observed choice pattern", status = "primary", solidHeader = FALSE,
              plotOutput("overview_choice_plot", height = 300),
              p(class = "chart-caption", "Share of trials choosing B across expected-payoff-difference groups.")),
          box(width = 4, title = "Model probability predictions", status = "primary", solidHeader = FALSE,
              plotOutput("overview_probability_plot", height = 300),
              p(class = "chart-caption", "Predicted probability of B, separated by the actual choice on the held-out test set.")),
          box(width = 4, title = "Most influential model features", status = "primary", solidHeader = FALSE,
              plotOutput("overview_shap_plot", height = 300),
              p(class = "chart-caption", "Mean absolute SHAP values summarize each feature's overall contribution to model predictions."))
        ),
        fluidRow(column(12,
                        div(class = "insight-note", strong("Interpretation note: "),
                            "These results describe observed associations and predictive patterns, not causal effects. Behavioral clusters are exploratory because the reported silhouette scores were low.")
        ))
      ),
      
      # Choice patterns
      tabItem(
        tabName = "choice",
        h1("Choice Patterns", class = "page-heading"),
        p("Explore how the observed probability of choosing option B changes across decision variables.", class = "page-subtitle"),
        fluidRow(
          box(width = 4, title = "Analysis controls", status = "primary",
              selectInput("choice_var", "Decision variable", choices = choice_vars, selected = "expected_payoff_diff"),
              sliderInput("choice_bins", "Bins (continuous variables)", 4, 20, 10, step = 1)),
          box(width = 8, title = "Proportion choosing B", status = "primary", plotOutput("choice_plot", height = 390))
        ),
        p("Descriptive only. Error bars treat every choice as independent, so they may be narrower than the mixed-effects model's uncertainty.")
      ),
      
      # Statistical evidence
      tabItem(
        tabName = "statistics",
        h1("Statistical Evidence", class = "page-heading"),
        p("Mixed-effects model estimates with 95% confidence intervals.", class = "page-subtitle"),
        box(width = 12, title = "Odds ratios and 95% confidence intervals", status = "primary",
            plotOutput("statistics_plot", height = 470),
            div(class = "insight-note", "An odds ratio above 1 indicates higher odds of choosing B; an odds ratio below 1 indicates lower odds. Continuous predictors are expressed per 1 standard deviation, as specified by the analysis."))
      ),
      
      # Model performance
      tabItem(
        tabName = "model",
        h1("Model Performance", class = "page-heading"),
        p("Held-out test-set evaluation and threshold-dependent classification behaviour.", class = "page-subtitle"),
        fluidRow(
          box(width = 6, title = "Final model on the test set", status = "primary", tableOutput("model_table")),
          box(width = 6, title = "Threshold explorer", status = "primary",
              sliderInput("threshold", "Classification threshold", 0.1, 0.9, 0.5, step = 0.05),
              tableOutput("threshold_table"))
        ),
        fluidRow(
          box(width = 6, title = "Confusion matrix", status = "primary", tableOutput("confusion_table")),
          box(width = 6, title = "Predicted probability distribution", status = "primary", plotOutput("probability_plot", height = 340))
        )
      ),
      
      # Explainability
      tabItem(
        tabName = "explainability",
        h1("Model Explainability", class = "page-heading"),
        p("Global feature importance and SHAP-based interpretation of model predictions.", class = "page-subtitle"),
        fluidRow(
          box(width = 6, title = "Top decision factors (SHAP)", status = "primary",
              sliderInput("top_n", "Number of features", 3, 15, 8, step = 1),
              plotOutput("factor_plot", height = 420)),
          box(width = 6, title = "SHAP summary", status = "primary", imageOutput("shap_image", height = "420px"))
        ),
        div(class = "insight-note", "SHAP values describe how the model uses its inputs; they are not evidence that a feature causes a human choice.")
      ),
      
      # Behavioral profiles
      tabItem(
        tabName = "profiles",
        h1("Behavioral Profiles", class = "page-heading"),
        p("Exploratory grouping of observations by decision characteristics in principal-component space.", class = "page-subtitle"),
        fluidRow(
          box(width = 6, title = "Cluster sizes", status = "primary", plotOutput("profile_plot", height = 390)),
          box(width = 6, title = "Cluster centres in PCA space", status = "primary", tableOutput("profile_table"))
        ),
        div(class = "insight-note", "The reported silhouette scores were low. Treat clusters as exploratory regions along a continuum rather than distinct psychological types.")
      )
    )
  )
)

# ---------------------------- SERVER ---------------------------
server <- function(input, output, session) {
  
  # Overview KPI boxes
  output$model_box <- renderValueBox({
    valueBox(model_summary$final_model[1], "Final Model", icon = icon("robot"), color = "teal")
  })
  output$accuracy_box <- renderValueBox({
    valueBox(paste0(round(model_summary$accuracy[1] * 100, 1), "%"), "Test Accuracy", icon = icon("bullseye"), color = "teal")
  })
  output$auc_box <- renderValueBox({
    valueBox(round(model_summary$roc_auc[1], 3), "ROC-AUC", icon = icon("chart-line"), color = "teal")
  })
  output$brier_box <- renderValueBox({
    valueBox(round(model_summary$brier_score[1], 3), "Brier Score", icon = icon("calculator"), color = "teal")
  })
  
  # Choice patterns: same underlying descriptive analysis
  choice_summary <- reactive({
    x <- choice_data[[input$choice_var]]
    bin <- if (dplyr::n_distinct(x) <= 10) factor(x) else {
      breaks <- unique(as.numeric(quantile(x, seq(0, 1, length.out = input$choice_bins + 1), na.rm = TRUE)))
      if (length(breaks) < 2) factor(x) else cut(x, breaks = breaks, include.lowest = TRUE)
    }
    data.frame(bin = bin, chose_b = choice_data$chose_b) %>%
      filter(!is.na(bin), !is.na(chose_b)) %>%
      group_by(bin) %>%
      summarise(rate = mean(chose_b), n = n(), .groups = "drop") %>%
      mutate(se = sqrt(rate * (1 - rate) / n))
  })
  
  make_choice_plot <- function() {
    ggplot(choice_summary(), aes(x = bin, y = rate, group = 1)) +
      geom_hline(yintercept = mean(choice_data$chose_b, na.rm = TRUE), linetype = "dashed", colour = "#91a4b8") +
      geom_line(colour = "#159f9a", linewidth = 1) +
      geom_point(colour = "#132b43", size = 2.4) +
      geom_errorbar(aes(ymin = pmax(0, rate - 1.96 * se), ymax = pmin(1, rate + 1.96 * se)), width = 0.18, colour = "#71869b") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(x = names(choice_vars)[match(input$choice_var, choice_vars)], y = "Share choosing B") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1),
            axis.title = element_text(colour = "#526a80"))
  }
  
  output$choice_plot <- renderPlot({ make_choice_plot() })
  output$overview_choice_plot <- renderPlot({
    old_var <- input$choice_var
    # Overview always shows expected payoff difference, independent of the detail-page control.
    x <- choice_data$expected_payoff_diff
    bin <- if (dplyr::n_distinct(x) <= 10) factor(x) else {
      breaks <- unique(as.numeric(quantile(x, seq(0, 1, length.out = 11), na.rm = TRUE)))
      if (length(breaks) < 2) factor(x) else cut(x, breaks = breaks, include.lowest = TRUE)
    }
    d <- data.frame(bin = bin, chose_b = choice_data$chose_b) %>%
      filter(!is.na(bin), !is.na(chose_b)) %>% group_by(bin) %>%
      summarise(rate = mean(chose_b), n = n(), .groups = "drop") %>%
      mutate(se = sqrt(rate * (1-rate) / n))
    ggplot(d, aes(x = bin, y = rate, group = 1)) +
      geom_hline(yintercept = mean(choice_data$chose_b, na.rm = TRUE), linetype = "dashed", colour = "#91a4b8") +
      geom_ribbon(aes(ymin = pmax(0, rate - 1.96*se), ymax = pmin(1, rate + 1.96*se), group = 1), fill = "#159f9a", alpha = .12) +
      geom_line(colour = "#159f9a", linewidth = 1.1) + geom_point(colour = "#132b43", size = 2.2) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
      labs(x = "Expected payoff difference", y = "Proportion choosing B") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1, size = 9, colour = "#344b60"),
            axis.text.y = element_text(size = 10, colour = "#344b60"),
            axis.title = element_text(size = 11, face = "bold", colour = "#20384f"))
  })
  
  # Statistical evidence
  output$statistics_plot <- renderPlot({
    ggplot(statistical_evidence, aes(x = odds_ratio, y = reorder(gsub("_", " ", term), odds_ratio))) +
      geom_vline(xintercept = 1, linetype = "dashed", colour = "#91a4b8") +
      geom_pointrange(aes(xmin = conf.low, xmax = conf.high), colour = "#159f9a", linewidth = .55) +
      geom_point(size = 2.2, colour = "#132b43") +
      scale_x_log10() + labs(x = "Odds ratio (log scale; reference = 1)", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 11, colour = "#344b60"),
            axis.text.x = element_text(size = 10, colour = "#344b60"),
            axis.title.x = element_text(size = 11, face = "bold", colour = "#20384f"))
  })
  
  # Model performance
  output$model_table <- renderTable({
    model_summary %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
  })
  
  confusion <- reactive({
    predicted <- as.integer(test_predictions$probability_b >= input$threshold)
    actual <- test_predictions$actual
    list(tp = sum(predicted == 1 & actual == 1, na.rm = TRUE),
         tn = sum(predicted == 0 & actual == 0, na.rm = TRUE),
         fp = sum(predicted == 1 & actual == 0, na.rm = TRUE),
         fn = sum(predicted == 0 & actual == 1, na.rm = TRUE))
  })
  
  output$threshold_table <- renderTable({
    with(confusion(), {
      precision <- ifelse(tp + fp == 0, NA, tp / (tp + fp))
      recall <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
      f1 <- ifelse(is.na(precision) || is.na(recall) || precision + recall == 0, NA,
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
  
  make_probability_plot <- function() {
    ggplot(test_predictions,
           aes(x = probability_b, fill = factor(actual, levels = 0:1, labels = c("Chose A", "Chose B")))) +
      geom_histogram(bins = 30, position = "identity", alpha = .62) +
      geom_vline(xintercept = input$threshold, linetype = "dashed", colour = "#132b43", linewidth = .8) +
      scale_fill_manual(values = c("Chose A" = "#ef9b94", "Chose B" = "#19b8b0")) +
      labs(x = "Predicted probability of choosing B", y = "Observations", fill = NULL) +
      theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  }
  output$probability_plot <- renderPlot({ make_probability_plot() })
  output$overview_probability_plot <- renderPlot({
    ggplot(test_predictions,
           aes(x = probability_b, fill = factor(actual, levels = 0:1, labels = c("Chose A", "Chose B")))) +
      geom_histogram(bins = 28, position = "identity", alpha = .58) +
      geom_vline(xintercept = .5, linetype = "dashed", colour = "#132b43", linewidth = .8) +
      scale_fill_manual(values = c("Chose A" = "#ef9b94", "Chose B" = "#19b8b0")) +
      labs(x = "Predicted probability of choosing B", y = "Number of test observations", fill = "Actual choice") +
      scale_x_continuous(breaks = seq(0, 1, by = .2), limits = c(0, 1)) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom",
            axis.text = element_text(size = 10, colour = "#344b60"),
            axis.title = element_text(size = 11, face = "bold", colour = "#20384f"),
            legend.text = element_text(size = 10), legend.title = element_text(size = 10, face = "bold"))
  })
  
  # Explainability
  output$factor_plot <- renderPlot({
    factor_summary %>% slice_head(n = input$top_n) %>%
      ggplot(aes(x = reorder(gsub("_", " ", feature), mean_abs_shap), y = mean_abs_shap)) +
      geom_col(fill = "#159f9a", width = .72) + coord_flip() +
      labs(x = NULL, y = "Mean absolute SHAP value") +
      theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
  })
  output$overview_shap_plot <- renderPlot({
    factor_summary %>% slice_max(order_by = mean_abs_shap, n = 7, with_ties = FALSE) %>%
      ggplot(aes(x = reorder(gsub("_", " ", feature), mean_abs_shap), y = mean_abs_shap)) +
      geom_col(fill = "#159f9a", width = .72) + coord_flip() +
      labs(x = NULL, y = "Mean absolute SHAP value") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 10, colour = "#344b60"),
            axis.text.x = element_text(size = 10, colour = "#344b60"),
            axis.title = element_text(size = 11, face = "bold", colour = "#20384f"))
  })
  output$shap_image <- renderImage({
    file <- file.path(project_root, "outputs", "explainability", "shap_summary.png")
    req(file.exists(file))
    list(src = file, contentType = "image/png", width = "100%")
  }, deleteFile = FALSE)
  
  # Behavioral profiles
  output$profile_plot <- renderPlot({
    ggplot(behavioral_profiles, aes(x = factor(cluster), y = observations)) +
      geom_col(fill = "#159f9a", width = .72) +
      labs(x = "Cluster", y = "Observations") +
      theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
  })
  output$profile_table <- renderTable({
    behavioral_profiles %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
  })
}

shinyApp(ui, server)
