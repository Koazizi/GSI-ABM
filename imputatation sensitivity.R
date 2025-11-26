# Load required packages
library(mice)
library(dplyr)
library(ggplot2)
library(tidyr)
library(naniar)  # for missing data visualization

# ==============================================================================
# 1. MISSINGNESS ASSESSMENT
# ==============================================================================

# Combine all construct items for analysis
construct_items <- sem_data %>%
  dplyr::select(
    attitude.1.1, attitude.1.2, attitude.1.3, attitude.1.4,
    aware.1.1, aware.1.2, aware.1.3, aware.1.4,
    pers.norm.1.1, pers.norm.1.2, pers.norm.1.3, pers.norm.1.4,
    soc.capital.1.1, soc.capital.1.2, soc.capital.1.3, 
    soc.capital.1.4, soc.capital.1.5
  )

# Calculate missingness statistics
missingness_summary <- data.frame(
  Variable = names(construct_items),
  N_Missing = colSums(is.na(construct_items)),
  Percent_Missing = round(colMeans(is.na(construct_items)) * 100, 2),
  N_Complete = colSums(!is.na(construct_items))
)

print("=== Missingness Summary ===")
print(missingness_summary)

# Overall missingness
cat("\nOverall missingness rate:", 
    round(mean(is.na(construct_items)) * 100, 2), "%\n")

# Visualize missing data pattern
vis_miss(construct_items) + 
  labs(title = "Missing Data Pattern Across Survey Items")

# Missing data pattern by construct
md_pattern <- md.pattern(construct_items, rotate.names = TRUE)

# ==============================================================================
# 2. IMPUTATION WITH DIAGNOSTICS
# ==============================================================================

# Function to perform imputation with full diagnostics
impute_with_diagnostics <- function(df, construct_name, n_imputations = 5) {
  
  cat("\n=== Imputing", construct_name, "===\n")
  
  # Perform imputation
  init <- mice(df, maxit = 0, printFlag = FALSE)
  meth <- init$method
  pred <- init$predictorMatrix
  
  # Run imputation with more imputations for sensitivity
  imp <- mice(df, method = 'pmm', m = n_imputations, 
              maxit = 20, printFlag = FALSE, seed = 123)
  
  # Check convergence
  plot(imp, main = paste("Convergence Plot -", construct_name))
  
  # Density plots comparing observed vs imputed
  densityplot(imp, main = paste("Density Plot -", construct_name))
  
  return(imp)
}

# Perform imputation for each construct
imp_attitude <- impute_with_diagnostics(
  sem_data %>% dplyr::select(attitude.1.1, attitude.1.2, attitude.1.3, attitude.1.4),
  "Attitude", n_imputations = 5
)

imp_awareness <- impute_with_diagnostics(
  sem_data %>% dplyr::select(aware.1.1, aware.1.2, aware.1.3, aware.1.4),
  "Awareness", n_imputations = 5
)

imp_socialcap <- impute_with_diagnostics(
  sem_data %>% dplyr::select(soc.capital.1.1, soc.capital.1.2, soc.capital.1.3,
                      soc.capital.1.4, soc.capital.1.5),
  "Social Capital", n_imputations = 5
)

imp_persnorms <- impute_with_diagnostics(
  sem_data %>% dplyr::select(pers.norm.1.1, pers.norm.1.2, pers.norm.1.3, pers.norm.1.4),
  "Personal Norms", n_imputations = 5
)

# ==============================================================================
# 3. DESCRIPTIVE STATISTICS COMPARISON
# ==============================================================================

# Function to compare descriptive statistics
compare_descriptives <- function(original_data, imputed_mice_object, construct_name) {
  
  # Complete case analysis
  complete_data <- na.omit(original_data)
  
  # Get one imputed dataset (first imputation)
  imputed_data <- complete(imputed_mice_object, 1)
  
  # Calculate statistics
  results <- data.frame(
    Construct = construct_name,
    Variable = names(original_data),
    Original_Mean = colMeans(original_data, na.rm = TRUE),
    Original_SD = apply(original_data, 2, sd, na.rm = TRUE),
    Complete_Case_Mean = colMeans(complete_data),
    Complete_Case_SD = apply(complete_data, 2, sd),
    Imputed_Mean = colMeans(imputed_data),
    Imputed_SD = apply(imputed_data, 2, sd),
    N_Original = nrow(original_data),
    N_Complete = nrow(complete_data),
    N_Imputed = sum(!is.na(original_data[,1]))
  )
  
  # Calculate differences
  results$Mean_Diff_Imputed_vs_Original <- 
    results$Imputed_Mean - results$Original_Mean
  results$Mean_Diff_Imputed_vs_Complete <- 
    results$Imputed_Mean - results$Complete_Case_Mean
  
  return(results)
}

# Compare all constructs
desc_attitude <- compare_descriptives(
  sem_data %>% dplyr::select(attitude.1.1, attitude.1.2, attitude.1.3, attitude.1.4),
  imp_attitude, "Attitude"
)

desc_awareness <- compare_descriptives(
  sem_data %>% dplyr::select(aware.1.1, aware.1.2, aware.1.3, aware.1.4),
  imp_awareness, "Awareness"
)

desc_socialcap <- compare_descriptives(
  sem_data %>% dplyr::select(soc.capital.1.1, soc.capital.1.2, soc.capital.1.3,
                      soc.capital.1.4, soc.capital.1.5),
  imp_socialcap, "Social Capital"
)

desc_persnorms <- compare_descriptives(
  sem_data %>% dplyr::select(pers.norm.1.1, pers.norm.1.2, pers.norm.1.3, pers.norm.1.4),
  imp_persnorms, "Personal Norms"
)

# Combine all results
descriptive_comparison <- bind_rows(
  desc_attitude, desc_awareness, desc_socialcap, desc_persnorms
)

print("=== Descriptive Statistics Comparison ===")
print(descriptive_comparison)

# Export to CSV for supplementary materials
write.csv(descriptive_comparison, 
          "sensitivity_descriptive_comparison.csv", 
          row.names = FALSE)

# ==============================================================================
# 4. VISUALIZATION OF DIFFERENCES
# ==============================================================================

# Plot mean differences
ggplot(descriptive_comparison, 
       aes(x = Variable, y = Mean_Diff_Imputed_vs_Original)) +
  geom_col(aes(fill = Construct)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(title = "Mean Differences: Imputed vs. Original Data",
       x = "Survey Item",
       y = "Difference in Means",
       fill = "Construct") +
  theme_minimal() +
  facet_wrap(~Construct, scales = "free_y")

# ==============================================================================
# 5. POOLED ESTIMATES ACROSS MULTIPLE IMPUTATIONS
# ==============================================================================

# Function to get pooled estimates across imputations
pool_estimates <- function(imputed_mice_object, construct_name) {
  
  # Get all imputed datasets
  n_imp <- imputed_mice_object$m
  
  pooled_stats <- lapply(1:n_imp, function(i) {
    imp_data <- complete(imputed_mice_object, i)
    data.frame(
      Imputation = i,
      Variable = names(imp_data),
      Mean = colMeans(imp_data),
      SD = apply(imp_data, 2, sd)
    )
  }) %>% bind_rows()
  
  # Calculate pooled means and between-imputation variance
  pooled_summary <- pooled_stats %>%
    group_by(Variable) %>%
    summarise(
      Construct = construct_name,
      Pooled_Mean = mean(Mean),
      Pooled_SD = mean(SD),
      Between_Imp_Var = var(Mean),
      Within_Imp_Var = mean(SD^2),
      .groups = 'drop'
    )
  
  # Total variance (Rubin's rules)
  pooled_summary$Total_Var <- 
    pooled_summary$Within_Imp_Var + 
    (1 + 1/n_imp) * pooled_summary$Between_Imp_Var
  
  return(pooled_summary)
}

# Pool estimates for each construct
pooled_attitude <- pool_estimates(imp_attitude, "Attitude")
pooled_awareness <- pool_estimates(imp_awareness, "Awareness")
pooled_socialcap <- pool_estimates(imp_socialcap, "Social Capital")
pooled_persnorms <- pool_estimates(imp_persnorms, "Personal Norms")

pooled_comparison <- bind_rows(
  pooled_attitude, pooled_awareness, pooled_socialcap, pooled_persnorms
)

print("=== Pooled Estimates Across Imputations ===")
print(pooled_comparison)

write.csv(pooled_comparison, 
          "sensitivity_pooled_estimates.csv", 
          row.names = FALSE)

# ==============================================================================
# 6. CORRELATION MATRIX COMPARISON
# ==============================================================================

# Function to compare correlation matrices
compare_correlations <- function(original_data, imputed_mice_object, construct_name) {
  
  # Original data correlations (pairwise complete)
  cor_original <- cor(original_data, use = "pairwise.complete.obs")
  
  # Complete case correlations
  cor_complete <- cor(na.omit(original_data))
  
  # Imputed data correlations
  cor_imputed <- cor(complete(imputed_mice_object, 1))
  
  # Calculate differences
  cor_diff_imputed_vs_original <- cor_imputed - cor_original
  cor_diff_imputed_vs_complete <- cor_imputed - cor_complete
  
  return(list(
    original = cor_original,
    complete_case = cor_complete,
    imputed = cor_imputed,
    diff_vs_original = cor_diff_imputed_vs_original,
    diff_vs_complete = cor_diff_imputed_vs_complete
  ))
}

# Compare correlations for each construct
cor_attitude <- compare_correlations(
  sem_data %>% dplyr::select(attitude.1.1, attitude.1.2, attitude.1.3, attitude.1.4),
  imp_attitude, "Attitude"
)

# Visualize correlation differences
library(reshape2)
cor_diff_melt <- melt(cor_attitude$diff_vs_original)

ggplot(cor_diff_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-0.1, 0.1)) +
  labs(title = "Correlation Differences: Imputed vs. Original (Attitude)",
       x = "", y = "", fill = "Difference") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==============================================================================
# 7. SEM COMPARISON (if you're using lavaan)
# ==============================================================================

# Example: Compare SEM results with complete cases vs. imputed data
# You'll need to adapt this to your actual SEM model specification

library(lavaan)

# Define your SEM model (example - adjust to your actual model)
sem_model <- '
  # Measurement model
  Attitude =~ attitude.1.1 + attitude.1.2 + attitude.1.3 + attitude.1.4
  Awareness =~ aware.1.1 + aware.1.2 + aware.1.3 + aware.1.4
  PersonalNorms =~ pers.norm.1.1 + pers.norm.1.2 + pers.norm.1.3 + pers.norm.1.4
  SocialCapital =~ soc.capital.1.1 + soc.capital.1.2 + soc.capital.1.3 + 
                   soc.capital.1.4 + soc.capital.1.5
'

# Complete case SEM
sem_complete <- sem(sem_model, data = na.omit(sem_data))

# Imputed data SEM (using first imputation)
sem_data_imputed <- sem_data
sem_data_imputed[names(df_attitude)] <- complete(imp_attitude, 1)
sem_data_imputed[names(df_awareness)] <- complete(imp_awareness, 1)
sem_data_imputed[names(df_persnorms)] <- complete(imp_persnorms, 1)
sem_data_imputed[names(df_socialcap)] <- complete(imp_socialcap, 1)

sem_imputed <- sem(sem_model, data = sem_data_imputed)

# Compare fit indices
fit_comparison <- data.frame(
  Analysis = c("Complete Case", "Imputed"),
  N = c(nobs(sem_complete), nobs(sem_imputed)),
  ChiSq = c(fitMeasures(sem_complete, "chisq"), 
            fitMeasures(sem_imputed, "chisq")),
  DF = c(fitMeasures(sem_complete, "df"), 
         fitMeasures(sem_imputed, "df")),
  CFI = c(fitMeasures(sem_complete, "cfi"), 
          fitMeasures(sem_imputed, "cfi")),
  RMSEA = c(fitMeasures(sem_complete, "rmsea"), 
            fitMeasures(sem_imputed, "rmsea")),
  SRMR = c(fitMeasures(sem_complete, "srmr"), 
           fitMeasures(sem_imputed, "srmr"))
)

print("=== SEM Fit Comparison ===")
print(fit_comparison)

# Compare parameter estimates
params_complete <- parameterEstimates(sem_complete)
params_imputed <- parameterEstimates(sem_imputed)

# ==============================================================================
# 8. SUMMARY REPORT FOR SUPPLEMENTARY MATERIALS
# ==============================================================================

# Create a comprehensive summary
sensitivity_summary <- list(
  missingness = missingness_summary,
  descriptive_comparison = descriptive_comparison,
  pooled_estimates = pooled_comparison,
  sem_fit_comparison = fit_comparison
)

# Save as RData for easy loading
save(sensitivity_summary, file = "imputation_sensitivity_analysis.RData")

# Create a formatted summary table
cat("\n=== SUMMARY FOR REVIEWER RESPONSE ===\n")
cat("\nMissingness Rate by Construct:\n")
missingness_by_construct <- descriptive_comparison %>%
  group_by(Construct) %>%
  summarise(
    N_Items = n(),
    Mean_Missing_Rate = mean((N_Original - N_Complete) / N_Original * 100),
    Max_Missing_Rate = max((N_Original - N_Complete) / N_Original * 100)
  )
print(missingness_by_construct)

cat("\nMaximum Mean Difference (Imputed vs Original):\n")
max_diff <- descriptive_comparison %>%
  group_by(Construct) %>%
  summarise(
    Max_Abs_Diff = max(abs(Mean_Diff_Imputed_vs_Original)),
    Avg_Abs_Diff = mean(abs(Mean_Diff_Imputed_vs_Original))
  )
print(max_diff)

cat("\nBetween-Imputation Variance (should be small):\n")
print(pooled_comparison %>% dplyr::select(Construct, Variable, Between_Imp_Var))