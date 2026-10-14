# HUman Decision Intelligence

**Understanding, Predicting, and Explaining Human Choices Under Risk, Ambiguity, and Feedback**

An R-based data science project exploring human decision-making using statistical analysis, machine learning, and explainable AI.

## Project Overview

This project analyzes the CPC18 dataset to investigate how expected payoffs, probabilities, ambiguity, feedback, and other decision-related factors relate to human choices.

## Methodology

- Data cleaning, validation, and feature engineering
- Exploratory data analysis and mixed-effects statistical modeling
- Principal Component Analysis (PCA) and behavioral clustering
- Machine learning using GLM, GAM, Random Forest, and XGBoost
- Subject-grouped model evaluation and model selection
- Model explainability using SHAP and LIME
- Interactive analytical application using Shiny

## Technology Stack

R · Shiny · ggplot2 · dplyr · lme4 · mgcv · ranger · XGBoost · SHAP · LIME

## Interactive Application

**Live Demo:** Coming soon

The Shiny application brings together model performance, decision-related patterns, explainability results, and behavioral profiles.

## Repository Structure

- `R/` — Analytical pipeline scripts
- `app/` — Shiny application
- `data/` — Data and processed analytical artifacts
- `reports/` — Stage-wise R Markdown documentation
- `outputs/` — Generated analytical outputs
- `docs/` — Project milestone documentation

## Reproducibility

The project uses `renv` for R package dependency management. The Shiny application is designed to use precomputed analytical artifacts without rerunning the complete pipeline.

## Notes

The findings describe predictive patterns and statistical associations; they should not be interpreted as causal conclusions. Raw dataset availability and redistribution are subject to the dataset's applicable terms.

## Author

**Hemanth Sai Charagundla**