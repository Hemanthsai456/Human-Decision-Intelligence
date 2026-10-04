# ============================================================
# Human Decision Intelligence
# 07_statistical_analysis.R
#
# Purpose:
#   Test defined questions about choice behavior under
#   expected value, probability, ambiguity, payoff structure,
#   correlation, and feedback while accounting for repeated
#   observations within subjects and decision problems.
# ============================================================

# Packages
library(dplyr)
library(lme4)
library(broom.mixed)

cpc18_features <- read.csv("data/processed/cpc18_features.csv")

cont_vars <- c("expected_payoff_diff", "probability_diff", "payoff_spread_diff")

analysis_data <- cpc18_features %>%
  mutate(across(all_of(cont_vars), ~ as.numeric(scale(.x)))) %>%
  group_by(subject_id, game_id, across(all_of(cont_vars)),
           ambiguous_b, payoff_correlation, full_feedback) %>%
  summarise(n_b = sum(chose_b), n_a = n() - sum(chose_b), .groups = "drop") %>%
  mutate(
    subject_id = factor(subject_id),
    game_id = factor(game_id),
    ambiguous_b = factor(ambiguous_b),
    full_feedback = factor(full_feedback),
    payoff_correlation = relevel(factor(payoff_correlation), ref = "0")
  )

stopifnot(sum(analysis_data$n_b + analysis_data$n_a) == nrow(cpc18_features))

# Predictor collinearity (EV and probability differences can overlap)
print(round(cor(analysis_data[cont_vars]), 2))

# Model fitting
fit_glmm <- function(formula, start = NULL) {
  t0 <- Sys.time()
  m <- glmer(formula, data = analysis_data, family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5)),
             start = start)
  attr(m, "minutes") <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  m
}

re <- "(1 | subject_id) + (1 | game_id)"
y  <- "cbind(n_b, n_a)"

# Main decision model: answers Q1-Q6 (adjusted effects)
model_main <- fit_glmm(as.formula(paste(y, "~ expected_payoff_diff + probability_diff +
  ambiguous_b + payoff_spread_diff + payoff_correlation + full_feedback +", re)))

# Justified interactions (start from the main-model variance estimates)
start_theta <- list(theta = getME(model_main, "theta"))

model_ambiguity_interaction <- fit_glmm(as.formula(paste(y, "~
  expected_payoff_diff * ambiguous_b + probability_diff + payoff_spread_diff +
  payoff_correlation + full_feedback +", re)), start_theta)

model_feedback_interaction <- fit_glmm(as.formula(paste(y, "~
  expected_payoff_diff * full_feedback + probability_diff + ambiguous_b +
  payoff_spread_diff + payoff_correlation +", re)), start_theta)

models <- list(
  "Main Model" = model_main,
  "EV x Ambiguity" = model_ambiguity_interaction,
  "EV x Feedback" = model_feedback_interaction
)

# Fixed effects (odds ratios with 95% Wald CI)
statistical_results <- bind_rows(lapply(names(models), function(nm) {
  broom.mixed::tidy(models[[nm]], effects = "fixed",
                    conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(model = nm)
}))

write.csv(statistical_results, "data/processed/statistical_model_results.csv",
          row.names = FALSE)

# Fit, convergence, and singularity; interaction tests vs. main model
lrt_p <- c(NA,
           anova(model_main, model_ambiguity_interaction)$`Pr(>Chisq)`[2],
           anova(model_main, model_feedback_interaction)$`Pr(>Chisq)`[2])

model_fit <- tibble(
  model = names(models),
  AIC = sapply(models, AIC),
  logLik = sapply(models, function(m) as.numeric(logLik(m))),
  singular = sapply(models, isSingular),
  converged = sapply(models, function(m) is.null(m@optinfo$conv$lme4$messages)),
  lrt_p_vs_main = lrt_p,
  minutes = sapply(models, function(m) round(attr(m, "minutes"), 1)),
  observations = sum(analysis_data$n_b + analysis_data$n_a)
)

write.csv(model_fit, "data/processed/statistical_model_fit.csv", row.names = FALSE)

cat("CPC18 statistical analysis completed.\n")

cat("Trials:", unique(model_fit$observations), "| aggregated rows:",
    nrow(analysis_data), "\n")

cat("Models fitted:", nrow(model_fit), "\n")