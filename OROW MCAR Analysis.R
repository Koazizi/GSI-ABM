# ==============================================================================
# MISSING COMPLETELY AT RANDOM (MCAR) TESTS
# ==============================================================================

library(mice)
library(naniar)
library(dplyr)
library(ggplot2)
library(tidyr)

# ==============================================================================
# 1. LITTLE'S MCAR TEST
# ==============================================================================

# Prepare data with all construct items
construct_items <- sem_data %>%
  dplyr::select(
    attitude.1.1, attitude.1.2, attitude.1.3, attitude.1.4,
    aware.1.1, aware.1.2, aware.1.3, aware.1.4,
    pers.norm.1.1, pers.norm.1.2, pers.norm.1.3, pers.norm.1.4,
    soc.capital.1.1, soc.capital.1.2, soc.capital.1.3, 
    soc.capital.1.4, soc.capital.1.5
  )

# Run Little's MCAR test
cat("\n=== LITTLE'S MCAR TEST ===\n")
mcar_test <- mcar_test(construct_items)
print(mcar_test)

# Interpret results
cat("\nInterpretation:\n")
if(mcar_test$p.value > 0.05) {
  cat("p-value =", round(mcar_test$p.value, 4), 
      "\nResult: CANNOT reject MCAR assumption (p > 0.05)\n")
  cat("Conclusion: Missing data appears to be Missing Completely At Random\n")
} else {
  cat("p-value =", round(mcar_test$p.value, 4), 
      "\nResult: REJECT MCAR assumption (p < 0.05)\n")
  cat("Conclusion: Missing data may NOT be completely random\n")
  cat("This suggests missingness may be related to observed or unobserved variables\n")
}

# ==============================================================================
# 2. CONSTRUCT-SPECIFIC MCAR TESTS
# ==============================================================================

# Function to test MCAR for each construct separately
test_construct_mcar <- function(data, construct_name, items) {
  construct_data <- data %>% dplyr::select(all_of(items))
  test_result <- mcar_test(construct_data)
  
  return(data.frame(
    Construct = construct_name,
    Chi_Square = test_result$statistic,
    DF = test_result$df,
    P_Value = test_result$p.value,
    MCAR = ifelse(test_result$p.value > 0.05, "Cannot Reject", "Reject")
  ))
}

# Test each construct
constructs_mcar <- bind_rows(
  test_construct_mcar(sem_data, "Attitude", 
                      c("attitude.1.1", "attitude.1.2", "attitude.1.3", "attitude.1.4")),
  test_construct_mcar(sem_data, "Awareness",
                      c("aware.1.1", "aware.1.2", "aware.1.3", "aware.1.4")),
  test_construct_mcar(sem_data, "Personal Norms",
                      c("pers.norm.1.1", "pers.norm.1.2", "pers.norm.1.3", "pers.norm.1.4")),
  test_construct_mcar(sem_data, "Social Capital",
                      c("soc.capital.1.1", "soc.capital.1.2", "soc.capital.1.3",
                        "soc.capital.1.4", "soc.capital.1.5"))
)

cat("\n=== CONSTRUCT-SPECIFIC MCAR TESTS ===\n")
print(constructs_mcar)

# Save results
write.csv(constructs_mcar, "mcar_test_results.csv", row.names = FALSE)

# ==============================================================================
# 3. DEMOGRAPHIC COMPARISON: COMPLETE VS MISSING PERSONAL NORMS
# ==============================================================================

# Create indicator for missing Personal Norms
sem_data <- sem_data %>%
  mutate(
    missing_persnorm = is.na(pers.norm.1.1) | is.na(pers.norm.1.2) | 
      is.na(pers.norm.1.3) | is.na(pers.norm.1.4),
    missing_persnorm_count = rowSums(is.na(dplyr::select(., starts_with("pers.norm"))))
  )

# Demographic variables to compare (adjust these to match your actual variable names)
# Assuming you have: income, education, district, race/ethnicity
demographic_vars <- c("income", "education", "district", "race_ethnicity")  # ADJUST THESE

cat("\n=== DEMOGRAPHIC COMPARISON: PERSONAL NORMS MISSINGNESS ===\n")

# Function to compare demographics
compare_demographics <- function(data, demo_var, missing_indicator) {
  
  # For continuous variables
  if(is.numeric(data[[demo_var]])) {
    comparison <- data %>%
      group_by(!!sym(missing_indicator)) %>%
      summarise(
        N = n(),
        Mean = mean(!!sym(demo_var), na.rm = TRUE),
        SD = sd(!!sym(demo_var), na.rm = TRUE),
        Median = median(!!sym(demo_var), na.rm = TRUE),
        .groups = 'drop'
      )
    
    # T-test
    ttest_result <- t.test(
      data[[demo_var]][data[[missing_indicator]] == FALSE],
      data[[demo_var]][data[[missing_indicator]] == TRUE]
    )
    
    cat("\n", demo_var, "(continuous):\n")
    print(comparison)
    cat("T-test p-value:", round(ttest_result$p.value, 4), "\n")
    
    return(data.frame(
      Variable = demo_var,
      Type = "Continuous",
      Test = "T-test",
      P_Value = ttest_result$p.value,
      Significant = ifelse(ttest_result$p.value < 0.05, "Yes", "No")
    ))
    
    # For categorical variables
  } else {
    comparison <- data %>%
      group_by(!!sym(missing_indicator), !!sym(demo_var)) %>%
      summarise(N = n(), .groups = 'drop') %>%
      group_by(!!sym(missing_indicator)) %>%
      mutate(Percent = round(N / sum(N) * 100, 2))
    
    # Chi-square test
    contingency_table <- table(data[[missing_indicator]], data[[demo_var]])
    chi_result <- chisq.test(contingency_table)
    
    cat("\n", demo_var, "(categorical):\n")
    print(comparison)
    cat("Chi-square p-value:", round(chi_result$p.value, 4), "\n")
    
    return(data.frame(
      Variable = demo_var,
      Type = "Categorical",
      Test = "Chi-square",
      P_Value = chi_result$p.value,
      Significant = ifelse(chi_result$p.value < 0.05, "Yes", "No")
    ))
  }
}

# Run comparisons for all demographic variables
demographic_comparison_results <- lapply(demographic_vars, function(var) {
  if(var %in% names(sem_data)) {
    compare_demographics(sem_data, var, "missing_persnorm")
  }
}) %>% bind_rows()

cat("\n=== SUMMARY OF DEMOGRAPHIC COMPARISONS ===\n")
print(demographic_comparison_results)

# Save results
write.csv(demographic_comparison_results, 
          "demographic_comparison_persnorms.csv", 
          row.names = FALSE)

# ==============================================================================
# 4. VISUALIZATIONS
# ==============================================================================

# Visualization 1: Income distribution by Personal Norms missingness
if("income" %in% names(sem_data)) {
  p1 <- ggplot(sem_data, aes(x = income, fill = missing_persnorm)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                      labels = c("FALSE" = "Complete", "TRUE" = "Missing"),
                      name = "Personal Norms") +
    labs(title = "Income Distribution by Personal Norms Missingness",
         x = "Income",
         y = "Density") +
    theme_minimal()
  
  print(p1)
  ggsave("income_by_persnorms_missing.png", p1, width = 8, height = 5)
}

# Visualization 2: District distribution
if("district" %in% names(sem_data)) {
  district_comparison <- sem_data %>%
    group_by(district, missing_persnorm) %>%
    summarise(N = n(), .groups = 'drop') %>%
    group_by(district) %>%
    mutate(Percent = N / sum(N) * 100)
  
  p2 <- ggplot(district_comparison, 
               aes(x = factor(district), y = Percent, fill = missing_persnorm)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                      labels = c("FALSE" = "Complete", "TRUE" = "Missing"),
                      name = "Personal Norms") +
    labs(title = "Personal Norms Missingness by District",
         x = "District",
         y = "Percentage") +
    theme_minimal()
  
  print(p2)
  ggsave("district_by_persnorms_missing.png", p2, width = 10, height = 5)
}

# Visualization 3: Education distribution
if("education" %in% names(sem_data)) {
  education_comparison <- sem_data %>%
    group_by(education, missing_persnorm) %>%
    summarise(N = n(), .groups = 'drop') %>%
    group_by(missing_persnorm) %>%
    mutate(Percent = N / sum(N) * 100)
  
  p3 <- ggplot(education_comparison, 
               aes(x = education, y = Percent, fill = missing_persnorm)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"),
                      labels = c("FALSE" = "Complete", "TRUE" = "Missing"),
                      name = "Personal Norms") +
    labs(title = "Education Distribution by Personal Norms Missingness",
         x = "Education Level",
         y = "Percentage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p3)
  ggsave("education_by_persnorms_missing.png", p3, width = 8, height = 5)
}

# ==============================================================================
# 5. MISSINGNESS PATTERN ANALYSIS
# ==============================================================================

# How many Personal Norms items are missing per case?
persnorm_missing_pattern <- sem_data %>%
  group_by(missing_persnorm_count) %>%
  summarise(
    N_Cases = n(),
    Percent = round(n() / nrow(sem_data) * 100, 2)
  )

cat("\n=== PERSONAL NORMS MISSING PATTERN ===\n")
cat("Number of Personal Norms items missing per respondent:\n")
print(persnorm_missing_pattern)

# Visualization
p4 <- ggplot(persnorm_missing_pattern, 
             aes(x = factor(missing_persnorm_count), y = N_Cases)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(N_Cases, " (", Percent, "%)")), 
            vjust = -0.5) +
  labs(title = "Pattern of Personal Norms Missingness",
       subtitle = "Number of missing items per respondent",
       x = "Number of Missing Personal Norms Items (out of 4)",
       y = "Number of Respondents") +
  theme_minimal()

print(p4)
ggsave("persnorms_missing_pattern.png", p4, width = 8, height = 5)

# ==============================================================================
# 6. COMPREHENSIVE SUMMARY REPORT
# ==============================================================================

cat("\n\n=== COMPREHENSIVE MISSINGNESS REPORT ===\n")
cat("\n1. OVERALL MCAR TEST:\n")
cat("   Chi-square:", round(mcar_test$statistic, 2), "\n")
cat("   df:", mcar_test$df, "\n")
cat("   p-value:", round(mcar_test$p.value, 4), "\n")
cat("   Conclusion:", ifelse(mcar_test$p.value > 0.05, 
                             "Cannot reject MCAR (data appears random)",
                             "Reject MCAR (data may not be random)"), "\n")

cat("\n2. CONSTRUCT-SPECIFIC TESTS:\n")
print(constructs_mcar)

cat("\n3. DEMOGRAPHIC ASSOCIATIONS:\n")
print(demographic_comparison_results)

cat("\n4. PERSONAL NORMS MISSING PATTERN:\n")
cat("   Cases with ANY missing Personal Norms:", 
    sum(sem_data$missing_persnorm), 
    paste0("(", round(mean(sem_data$missing_persnorm)*100, 1), "%)"), "\n")
cat("   Cases missing ALL 4 items:", 
    sum(sem_data$missing_persnorm_count == 4), "\n")
cat("   Cases missing 1-3 items:", 
    sum(sem_data$missing_persnorm_count > 0 & sem_data$missing_persnorm_count < 4), "\n")

# Save comprehensive report
sink("mcar_comprehensive_report.txt")
cat("=== COMPREHENSIVE MISSINGNESS REPORT ===\n")
cat("\nGenerated:", Sys.time(), "\n")
cat("\n1. OVERALL MCAR TEST:\n")
print(mcar_test)
cat("\n2. CONSTRUCT-SPECIFIC TESTS:\n")
print(constructs_mcar)
cat("\n3. DEMOGRAPHIC ASSOCIATIONS:\n")
print(demographic_comparison_results)
cat("\n4. PERSONAL NORMS MISSING PATTERN:\n")
print(persnorm_missing_pattern)
sink()

cat("\n\nAnalysis complete. Files saved:\n")
cat("- mcar_test_results.csv\n")
cat("- demographic_comparison_persnorms.csv\n")
cat("- mcar_comprehensive_report.txt\n")
cat("- Various PNG visualizations\n")