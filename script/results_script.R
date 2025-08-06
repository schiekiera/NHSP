# =============================================================================
# 1. Clear Workspace
# =============================================================================
# Remove all objects from the workspace to ensure a clean environment
rm(list = ls())

# =============================================================================
# 2. Load Required Libraries
# =============================================================================
# Load necessary libraries for data manipulation, visualization, and analysis
library(tidyverse)  # Data manipulation and visualization
library(readxl)     # Reading Excel files
library(writexl)    # Writing Excel files
library(report)     # Reporting tools
library(MASS)       # Statistical functions
library(sjPlot)     # Plotting tools
library(knitr)      # Dynamic report generation
library(kableExtra) # Table formatting
library(corrplot)   # Correlation plots
library(GGally)     # Extended ggplot2 functionalities
library(effects)    # Graphical and tabular effect displays
library(broom)      # Convert statistical analysis objects into tidy data frames
library(gridExtra)  # Miscellaneous functions for "grid" graphics
library(viridis)    # Color scales
library(brant)      # Brant test for proportional odds assumption
library(car)        # Companion to Applied Regression
library(ResourceSelection) # Goodness of fit tests

# =============================================================================
# 3. Data Acquisition
# =============================================================================
# Set the data directory and read the cleaned data file
# Change the working directory to the data directory
setwd("github_repo/data")

# Define the output directory for plots
output_dir_plots <- "output/plots"

# Read the cleaned data from GitHub
url <- "https://raw.githubusercontent.com/schiekiera/NHSP/refs/heads/main/data/data_nhsp.csv"
df <- read.csv(url)
head(df)

# =============================================================================
# 4. Descriptive Statistics
# =============================================================================
# Generate summary statistics for numeric variables

# Set a consistent theme for all plots
theme_set(theme_minimal(base_size = 12) + 
          theme(legend.position = "bottom"))

# Define numeric variables and their labels
numeric_vars <- c("ndg_mean_raw", "gdp_mean_raw", "gcri_mean_raw")
label_vars <- c("ND-GAIN", "GDP per capita", "CRI")

# Check which numeric variables exist in the dataframe
existing_vars <- numeric_vars[numeric_vars %in% names(df)]
cat("Available numeric variables:", paste(existing_vars, collapse = ", "), "\n")

if (length(existing_vars) > 0) {
  # Create summary statistics using base R approach for maximum compatibility
  numeric_summary <- data.frame(
    variable = existing_vars,
    n = sapply(existing_vars, function(x) sum(!is.na(df[[x]]))),
    mean = sapply(existing_vars, function(x) round(mean(df[[x]], na.rm = TRUE), 3)),
    median = sapply(existing_vars, function(x) round(median(df[[x]], na.rm = TRUE), 3)),
    sd = sapply(existing_vars, function(x) round(sd(df[[x]], na.rm = TRUE), 3)),
    min = sapply(existing_vars, function(x) round(min(df[[x]], na.rm = TRUE), 3)),
    max = sapply(existing_vars, function(x) round(max(df[[x]], na.rm = TRUE), 3)),
    q25 = sapply(existing_vars, function(x) round(quantile(df[[x]], 0.25, na.rm = TRUE), 3)),
    q75 = sapply(existing_vars, function(x) round(quantile(df[[x]], 0.75, na.rm = TRUE), 3)),
    stringsAsFactors = FALSE
  )
} else {
  cat("Warning: No numeric variables found in the expected names.\n")
  cat("Available column names:\n")
  print(names(df))
}

if (length(existing_vars) > 0) {
  print("=== DESCRIPTIVE STATISTICS FOR NUMERIC VARIABLES ===")
  output_dir <- "output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  #write.csv(numeric_summary, file.path(output_dir, "numeric_summary.csv"), row.names = FALSE)
  cat("Descriptive statistics have been written to data/output/numeric_summary.csv\n")
}

# Frequency table for dependent variable
cat("\n=== FREQUENCY TABLE FOR DEPENDENT VARIABLE ===\n")
dep_var_table <- table(df$dependent_variable, useNA = "ifany")
dep_var_prop <- prop.table(dep_var_table)
dep_var_summary <- data.frame(
  Category = names(dep_var_table),
  Frequency = as.numeric(dep_var_table),
  Percentage = round(as.numeric(dep_var_prop) * 100, 2)
)
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
#write.csv(dep_var_summary, file.path(output_dir, "dep_var_summary.csv"), row.names = FALSE)
cat("Frequency distribution of dependent variable has been written to output/dep_var_summary.csv\n")

# =============================================================================
# 5. Data Visualizations
# =============================================================================
# Create histograms and bar plots for numeric and dependent variables

# Histograms for numeric variables
plot_list_hist <- list()
for (var in existing_vars) {
  p <- ggplot(df, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
    geom_density(aes(y = ..density.. * nrow(df) * diff(range(df[[var]], na.rm = TRUE))/30), 
                 color = "red", size = 1) +
    labs(
         x = str_to_title(str_replace(var, "_", " ")),
         y = "Frequency") +
    theme_minimal()
  plot_list_hist[[var]] <- p
}

# Display histograms
if (length(plot_list_hist) > 0) {
  print("=== HISTOGRAMS FOR NUMERIC VARIABLES ===")
  grid.arrange(grobs = plot_list_hist, ncol = 2)
}

# Bar plot for dependent variable
p_bar <- ggplot(df, aes(x = dependent_variable, fill = dependent_variable)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("green" = "#2E8B57", "yellow" = "#FFD700", "red" = "#DC143C")) +
  scale_x_discrete(labels = c("green" = "High", "yellow" = "Moderate", "red" = "Low")) +
  labs(
       x = "Consideration of Climate Change",
       y = "Frequency",
       fill = "Category") +
  theme_minimal() +
  theme(legend.position = "none")

#ggsave(filename = file.path(output_dir_plots, "bar_plot_dependent_variable.pdf"), plot = p_bar, device = "pdf")

cat("Bar plot for dependent variable has been written to output/plots/bar_plot_dependent_variable.pdf\n")

# Correlation matrix for numeric variables
if (length(existing_vars) > 0) {
  cor_data <- df[, existing_vars, drop = FALSE]
  cor_data <- cor_data[complete.cases(cor_data), ]
} else {
  cor_data <- data.frame()
}
colnames(cor_data) <- c("ND-GAIN", "GDP per capita", "CRI")

if(nrow(cor_data) > 0) {
  # Rename columns and rows of cor_data
  
  cor_matrix <- cor(cor_data)
  
  # Correlation plot using corrplot
  print("=== CORRELATION MATRIX ===")
  corrplot(cor_matrix, method = "color", type = "upper", 
           order = "hclust", tl.cex = 0.8, tl.col = "black",
           addCoef.col = "black", number.cex = 0.8,
           mar = c(0,0,2,0))
  
  # Pairs plot using GGally
  p_pairs <- ggpairs(cor_data,
                     axisLabels = "show") +
    theme_minimal()
  
  print("=== PAIRWISE RELATIONSHIPS ===")
  print(p_pairs)
}

#ggsave(filename = file.path(output_dir_plots, "pairs_plot.pdf"), plot = p_pairs, device = "pdf")

cat("Pairs plot has been written to output/plots/pairs_plot.pdf\n")

# Box plots of predictors by dependent variable
plot_list_box <- list()

# Create a mapping for the x-axis labels
x_labels <- c("green" = "High", "yellow" = "Moderate", "red" = "Low")

for (i in 1:length(existing_vars)) {
  p <- ggplot(df, aes_string(x = "dependent_variable", y = existing_vars[i], fill = "dependent_variable")) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    scale_fill_manual(values = c("green" = "#2E8B57", "yellow" = "#FFD700", "red" = "#DC143C")) +
    scale_x_discrete(labels = x_labels) +  # Apply the new labels
    labs(
         x = "Consideration of Climate Change",
         y = label_vars[i],
         fill = "Category") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12),  # Increase font size of x-axis labels
      axis.title.x = element_text(size = 14), # Increase font size of x-axis title
      axis.title.y = element_text(size = 14)  # Increase font size of y-axis title
    )
  plot_list_box[[i]] <- p
}

if (length(plot_list_box) > 0) {
  print("=== BOX PLOTS BY DEPENDENT VARIABLE ===")
  grid.arrange(grobs = plot_list_box, ncol = 3)
}

#ggsave(filename = file.path(output_dir_plots, "box_plots_by_dependent_variable.pdf"), plot = grid.arrange(grobs = plot_list_box, ncol = 3), width = 12, height = 8, device = "pdf")

cat("Box plots by dependent variable have been written to output/plots/box_plots_by_dependent_variable.pdf\n")

# =============================================================================
# 6. Model Fitting
# =============================================================================
# Fit an ordinal logistic regression model

df$dependent_variable <- factor(df$dependent_variable,
                                levels = c("green", "yellow", "red"),
                                ordered = TRUE)
str(df$dependent_variable)
table(df$dependent_variable)

# Fit ordinal logistic regression
model <- polr(dependent_variable ~ ndg_mean + gdp_mean + gcri_mean, data = df, Hess = TRUE)
summary(model)

# =============================================================================
# 7. Assumption Tests
# =============================================================================
# Conduct tests to validate model assumptions

# 1. Proportional Odds Assumption
brantt_test <- brant(model)
print("=== BRANT TEST FOR PROPORTIONAL ODDS ASSUMPTION ===")
print(brantt_test)

# 2. Multicollinearity
vif_model <- lm(as.numeric(dependent_variable) ~ ndg_mean + gdp_mean + gcri_mean, data = df)
print("=== VIF FOR MULTICOLLINEARITY ===")
vif(vif_model)

# 3. Linearity of Logits
# Recode your outcome into two binary variables
print("=== LINEARITY OF LOGITS ===")
df <- df %>%
  mutate(
    at_least_yellow = ifelse(dependent_variable %in% c("yellow", "red"), 1, 0),
    at_least_red = ifelse(dependent_variable == "red", 1, 0)
  )

# Define function to plot logits
plot_logit <- function(df, predictor, outcome, outcome_label) {
  # Create quartile groups
  df$group <- cut(df[[predictor]], breaks = quantile(df[[predictor]], probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)
  
  # Summarize counts and proportions
  summary_data <- df %>%
    group_by(group) %>%
    summarise(
      p = mean(get(outcome)),
      mid = median(get(predictor), na.rm = TRUE)
    ) %>%
    mutate(logit = log(p / (1 - p)))
  
  # Plot
  ggplot(summary_data, aes(x = mid, y = logit)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
    labs(
      title = paste("Linearity Check for", predictor, "vs", outcome_label),
      x = predictor,
      y = "Logit (log odds)"
    ) +
    theme_minimal()
}

# Example: ND-GAIN vs at least moderate consideration
plot_logit(df, "ndg_mean", "at_least_yellow", "Moderate or Worse")
plot_logit(df, "gdp_mean", "at_least_yellow", "Moderate or Worse")
plot_logit(df, "gcri_mean", "at_least_yellow", "Moderate or Worse")

plot_logit(df, "ndg_mean", "at_least_red", "Low Only")
plot_logit(df, "gdp_mean", "at_least_red", "Low Only")
plot_logit(df, "gcri_mean", "at_least_red", "Low Only")

# 4. Hosmer-Lemeshow Test for Goodness of Fit
print("=== GOODNESS OF FIT ===")
# Model 1: At least yellow (moderate or worse)
model_yellow <- glm(at_least_yellow ~ ndg_mean + gdp_mean + gcri_mean, data = df, family = binomial)
hl_yellow <- hoslem.test(df$at_least_yellow, fitted(model_yellow), g = 10)

# Model 2: At least red (low only)
model_red <- glm(at_least_red ~ ndg_mean + gdp_mean + gcri_mean, data = df, family = binomial)
hl_red <- hoslem.test(df$at_least_red, fitted(model_red), g = 10)

# View results
print("=== Hosmer-Lemeshow Test: at least yellow ===")
print(hl_yellow)

print("=== Hosmer-Lemeshow Test: at least red ===")
print(hl_red)

# =============================================================================
# 8. Model Results
# =============================================================================
# Generate and save model results

# Enhanced model summary table
print("=== MODEL SUMMARY TABLE ===")
tab_model(model, 
          show.ci = TRUE, 
          show.se = TRUE, 
          show.stat = TRUE, 
          show.p = TRUE,
          dv.labels = "Dependent Variable",
          file = "output/ordinal_regression_results.html")

# Simple CSV export of model results
model_summary <- summary(model)
coef_table <- model_summary$coefficients

# Check coefficient table structure
print("Available coefficient table columns:")
print(colnames(coef_table))
print("Coefficient table dimensions:")
print(dim(coef_table))
print("First few rows:")
print(head(coef_table))

# Create simple results table handling different polr output formats
n_cols <- ncol(coef_table)

# Extract coefficients and standard errors (these should always be present)
coefficients_vals <- coef_table[, 1]
std_errors <- coef_table[, 2]

# Extract t-values (usually 3rd column if present)
if (n_cols >= 3) {
  t_values <- coef_table[, 3]
} else {
  # Calculate t-values manually
  t_values <- coefficients_vals / std_errors
}

# Extract or calculate p-values
if (n_cols >= 4) {
  # P-values are provided
  p_values <- coef_table[, 4]
} else {
  # Calculate p-values manually using t-distribution
  # For large samples, use normal approximation
  p_values <- 2 * (1 - pnorm(abs(t_values)))
}

# Create simple results table
simple_results <- data.frame(
  Predictors = rownames(coef_table),
  Coefficients = round(coefficients_vals, 3),
  Odds_Ratios = round(exp(coefficients_vals), 3),
  Std_Error = round(std_errors, 3),
  t_Statistic = round(t_values, 2),
  p_value = round(p_values, 4),
  p_formatted = ifelse(p_values < 0.001, "<0.001", 
                      ifelse(p_values < 0.01, sprintf("%.3f", p_values),
                            sprintf("%.3f", p_values))),
  stringsAsFactors = FALSE
)
#write.csv(simple_results, "output/model_results_simple.csv", row.names = FALSE)
cat("Simple model results saved to output/model_results_simple.csv\n")

# Create comprehensive model results table for CSV export
model_summary <- summary(model)
coefficients <- model_summary$coefficients

# Get confidence intervals
conf_intervals <- confint(model)

# Extract model components using defensive approach
predictors <- rownames(coefficients)
coef_estimates <- coefficients[, 1]  # Use numeric index for coefficients
std_errors <- coefficients[, 2]      # Use numeric index for std errors

# Handle t-values and p-values defensively
n_coef_cols <- ncol(coefficients)
if (n_coef_cols >= 3) {
  t_values <- coefficients[, 3]
} else {
  t_values <- coef_estimates / std_errors
}

if (n_coef_cols >= 4) {
  p_values <- coefficients[, 4]
} else {
  # Calculate p-values using normal approximation
  p_values <- 2 * (1 - pnorm(abs(t_values)))
}

# Check dimensions for debugging
print("Debug: Checking dimensions...")
print(paste("Predictors length:", length(predictors)))
print(paste("Coefficients length:", length(coef_estimates)))
print(paste("Conf intervals dimensions:", dim(conf_intervals)))

# Calculate odds ratios
odds_ratios <- exp(coef_estimates)

# Handle confidence intervals properly for polr models
# In polr, confint() typically only returns CIs for predictors, not intercepts
print("Predictor names:")
print(predictors)
print("Confidence interval row names:")
print(rownames(conf_intervals))

# Create vectors for confidence intervals, initialized with NA
odds_ci_lower <- rep(NA, length(coef_estimates))
odds_ci_upper <- rep(NA, length(coef_estimates))

# Match confidence intervals to the correct predictors
if (nrow(conf_intervals) > 0) {
  # Get the names of parameters that have confidence intervals
  ci_names <- rownames(conf_intervals)
  
  # Find which positions in the coefficient table correspond to these names
  for (i in 1:length(predictors)) {
    if (predictors[i] %in% ci_names) {
      # Find the row in conf_intervals that matches this predictor
      ci_row <- which(ci_names == predictors[i])
      if (length(ci_row) > 0) {
        odds_ci_lower[i] <- exp(conf_intervals[ci_row, 1])
        odds_ci_upper[i] <- exp(conf_intervals[ci_row, 2])
      }
    }
  }
}

# Show which parameters have CIs
has_ci <- !is.na(odds_ci_lower)
print("Parameters with confidence intervals:")
print(data.frame(Predictor = predictors, Has_CI = has_ci))

# Format p-values
p_formatted <- ifelse(p_values < 0.001, "<0.001", 
                     ifelse(p_values < 0.01, sprintf("%.3f", p_values),
                           sprintf("%.3f", p_values)))

# Make sure all vectors have the same length
n_coefs <- length(coef_estimates)
predictors_matched <- predictors[1:n_coefs]  # Take only the first n coefficients

# Create comprehensive results table with matched lengths
model_results_table <- data.frame(
  Predictors = predictors_matched,
  Coefficients = round(coef_estimates, 3),
  Odds_Ratios = round(odds_ratios, 3),
  Std_Error = round(std_errors, 3),
  CI_Lower = round(odds_ci_lower, 3),
  CI_Upper = round(odds_ci_upper, 3),
  CI = paste0(round(odds_ci_lower, 3), " – ", round(odds_ci_upper, 3)),
  t_Statistic = round(t_values, 2),
  p_value = p_formatted,
  stringsAsFactors = FALSE
)

# Add model fit statistics
n_obs <- nobs(model)
aic_value <- AIC(model)
bic_value <- BIC(model)
log_lik_value <- as.numeric(logLik(model))

# Add empty rows and model statistics
model_stats <- data.frame(
  Predictors = c("", "Model Statistics:", "Observations", "AIC", "BIC", "Log-Likelihood"),
  Coefficients = c("", "", n_obs, round(aic_value, 2), round(bic_value, 2), round(log_lik_value, 2)),
  Odds_Ratios = c("", "", "", "", "", ""),
  Std_Error = c("", "", "", "", "", ""),
  CI_Lower = c("", "", "", "", "", ""),
  CI_Upper = c("", "", "", "", "", ""),
  CI = c("", "", "", "", "", ""),
  t_Statistic = c("", "", "", "", "", ""),
  p_value = c("", "", "", "", "", ""),
  stringsAsFactors = FALSE
)

# Combine results
complete_model_table <- rbind(model_results_table, model_stats)

# Write to CSV
#write.csv(complete_model_table, "output/ordinal_regression_model_table.csv", row.names = FALSE)

# Also create a publication-ready version with cleaner column names
pub_table <- complete_model_table
colnames(pub_table) <- c("Predictors", "Coefficients", "Odds Ratios", "Std. Error", 
                        "CI Lower", "CI Upper", "95% CI", "t Statistic", "p")

#write.csv(pub_table, "output/ordinal_regression_results_formatted.csv", row.names = FALSE)

cat("Model results tables saved to:\n")
cat("- output/ordinal_regression_model_table.csv\n")
cat("- output/ordinal_regression_results_formatted.csv\n")

# Display the table
print("=== ORDINAL LOGISTIC REGRESSION MODEL TABLE ===")
print(pub_table)

# =============================================================================
# 8. Model Visualizations and Diagnostics
# =============================================================================
# Create plots for model diagnostics and visualizations

# Coefficient plot with confidence intervals
model_tidy <- tidy(model, conf.int = TRUE, exponentiate = FALSE)
model_tidy

model_tidy$term<-c("ND-GAIN", "GDP per capita", "CRI","Intercept: High -> Moderate", "Intercept: Moderate -> Low")

model_tidy$term <- factor(model_tidy$term, levels = c("ND-GAIN", "GDP per capita", "CRI","Intercept: High -> Moderate", "Intercept: Moderate -> Low"))

p_coef <- ggplot(model_tidy, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  labs(
       x = "Coefficient Estimate",
       y = "Predictor Variables") +
  theme_minimal() 

print("=== COEFFICIENT PLOT ===")
print(p_coef)
# save as pdf
#ggsave(filename = file.path(output_dir_plots, "coefficient_plot.pdf"), plot = p_coef, device = "pdf")

cat("Coefficient plot has been written to output/plots/coefficient_plot.pdf\n")

# Odds ratio plot
model_tidy_or <- tidy(model, conf.int = TRUE, exponentiate = TRUE)

model_tidy_or$term<-c("ND-GAIN", "GDP per capita", "CRI","Intercept: High -> Moderate", "Intercept: Moderate -> Low")

model_tidy_or$term <- factor(model_tidy_or$term, levels = c("ND-GAIN", "GDP per capita", "CRI","Intercept: High -> Moderate", "Intercept: Moderate -> Low"))

# drop the intercepts
model_tidy_or <- model_tidy_or[!grepl("Intercept", model_tidy_or$term), ]

p_or <- ggplot(model_tidy_or, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  scale_x_log10() +
  labs(
       x = "Odds Ratio (log scale)",
       y = "Predictor Variables") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

print("=== ODDS RATIO PLOT ===")
print(p_or)

#ggsave(filename = file.path(output_dir_plots, "odds_ratio_plot.pdf"), plot = p_or, device = "pdf")

cat("Odds ratio plot has been written to output/plots/odds_ratio_plot.pdf\n")

# Model fit statistics
print("=== MODEL FIT STATISTICS ===")
cat("AIC:", AIC(model), "\n")
cat("BIC:", BIC(model), "\n")
cat("Log-likelihood:", logLik(model), "\n")
cat("Deviance:", deviance(model), "\n")

# Residuals analysis (for ordinal regression, we use Pearson residuals)
model_residuals <- residuals(model, type = "pearson")
fitted_values <- fitted(model)

# Residual plots
p_resid1 <- data.frame(
  fitted = as.numeric(fitted_values),
  residuals = model_residuals,
  category = df$dependent_variable[!is.na(df$dependent_variable) & 
                                   !is.na(df$ndg_mean) & 
                                   !is.na(df$gdp_mean) & 
                                   !is.na(df$gcri_mean)]
) %>%
  ggplot(aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  labs(
       x = "Fitted Values",
       y = "Pearson Residuals") +
  theme_minimal()

# Q-Q plot for residuals
p_qq <- data.frame(residuals = model_residuals) %>%
  ggplot(aes(sample = residuals)) +
  stat_qq(color = "steelblue", alpha = 0.6) +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

print("=== DIAGNOSTIC PLOTS ===")
grid.arrange(p_resid1, p_qq, ncol = 2)

# Confusion matrix (using highest predicted probability)
predicted_class <- predict(model, type = "class")
actual_class <- df$dependent_variable[!is.na(df$dependent_variable) & 
                                      !is.na(df$ndg_mean) & 
                                      !is.na(df$gdp_mean) & 
                                      !is.na(df$gcri_mean)]

confusion_matrix <- table(Predicted = predicted_class, Actual = actual_class)

print("=== CONFUSION MATRIX ===")
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("\nOverall Accuracy:", round(accuracy * 100, 2), "%\n")

print("=== ANALYSIS COMPLETE ===")
cat("All descriptive statistics, visualizations, and model diagnostics have been generated.\n")

# =============================================================================
# 10. Sensitivity Analyses
# =============================================================================
# Conduct sensitivity analyses to check the robustness of the model

# Step 1: Define a function to remove outliers above +3 SD
remove_outliers_3sd <- function(data, variables) {
  for (var in variables) {
    upper_limit <- mean(data[[var]], na.rm = TRUE) + 3 * sd(data[[var]], na.rm = TRUE)
    data <- data[data[[var]] <= upper_limit, ]
  }
  return(data)
}

# Step 2: Specify predictors to check for outliers
predictors <- c("ndg_mean", "gdp_mean", "gcri_mean")

# Step 3: Create a new cleaned dataset
df_cleaned <- remove_outliers_3sd(df, predictors)

# Step 4: Fit original model (if not already fitted)
model_original <- polr(dependent_variable ~ ndg_mean + gdp_mean + gcri_mean, data = df, Hess = TRUE)

# Step 5: Fit model on cleaned dataset
model_sensitivity <- polr(dependent_variable ~ ndg_mean + gdp_mean + gcri_mean, data = df_cleaned, Hess = TRUE)

# Step 6: Compare models (coefficients and AIC)
summary(model_original)
summary(model_sensitivity)

# Compare AICs
cat("Original model AIC:", AIC(model_original), "\n")
cat("Sensitivity model AIC (no +3SD outliers):", AIC(model_sensitivity), "\n")

# Step 7: Optional - compare side-by-side
coef_comparison <- data.frame(
  Variable = names(coef(model_original)),
  Coef_Original = round(coef(model_original), 3),
  Coef_Sensitivity = round(coef(model_sensitivity), 3)
)

print("=== Coefficient Comparison ===")
print(coef_comparison)

tab_model(model_sensitivity, 
          show.ci = TRUE, 
          show.se = TRUE, 
          show.stat = TRUE, 
          show.p = TRUE,
          dv.labels = "Dependent Variable",
          file = "output/sensitivity_ordinal_regression_results.html")

