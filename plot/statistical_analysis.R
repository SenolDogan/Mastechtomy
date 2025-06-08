# Load required libraries
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(grid)

# Read the data with proper format and encoding
data <- read.csv("Test.csv", sep=";", dec=",", encoding="UTF-8")

# Print diagnostic information
print("Column names in the dataset:")
print(names(data))

print("\nChecking for 'Previous surgery' column variations:")
possible_names <- c("Previous surgery", "Previous.surgery", "Previous_surgery", "Previoussurgery")
for(name in possible_names) {
  if(name %in% names(data)) {
    print(paste("Found column:", name))
  }
}

print("\nFirst few rows of the data:")
print(head(data))

# Get the exact column name that contains "Previous surgery"
prev_surgery_col <- names(data)[grep("Previous.surgery|Previous surgery", names(data), value=FALSE)]
print("\nMatched column name:")
print(prev_surgery_col)

print("\nUnique values in Previous surgery column:")
print(unique(data[[prev_surgery_col]]))
print("\nStructure of Previous surgery column:")
print(str(data[[prev_surgery_col]]))

# Clinical Characteristics Analysis
# Initialize lists for clinical results and plots
clinical_results <- list()
clinical_plots <- list()

# Function to format p-values
format_pvalue <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste("p =", sprintf("%.3f", p)))
  }
}

# Function to perform t-test and create boxplot
analyze_clinical_factor <- function(data, factor_name) {
  # Create formula for t-test
  formula <- as.formula(paste("Total ~", factor_name))
  
  # Perform t-test
  t_result <- t.test(formula, data = data)
  
  # Create boxplot
  plot <- ggplot(data, aes_string(x = paste0("factor(", factor_name, ")"), 
                                 y = "Total", 
                                 fill = paste0("factor(", factor_name, ")"))) +
    geom_boxplot() +
    labs(x = factor_name,
         y = "Total") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8)
    ) +
    ggtitle(paste0(factor_name, "\n", format_pvalue(t_result$p.value)))
  
  return(list(test = t_result, plot = plot))
}

# Analyze each clinical factor
clinical_factors <- c("Hypertension", prev_surgery_col, "Diabetes")
for(factor in clinical_factors) {
  result <- analyze_clinical_factor(data, factor)
  clinical_results[[factor]] <- result$test
  clinical_plots[[factor]] <- result$plot
}

# Combine clinical plots
clinical_combined <- arrangeGrob(
  grobs = clinical_plots,
  ncol = 3,
  top = textGrob("Clinical Characteristics", 
                 gp = gpar(fontsize = 12, fontface = "bold"))
)

# Save clinical characteristics plot
ggsave("clinical_characteristics.pdf", clinical_combined, width = 15, height = 5)
ggsave("clinical_characteristics.png", clinical_combined, width = 15, height = 5, dpi = 300)

# Save t-test results to a text file
sink("clinical_characteristics_results.txt")
cat("Clinical Characteristics T-Test Results:\n\n")
for(factor in clinical_factors) {
  cat(paste("\n", factor, ":\n"))
  print(clinical_results[[factor]])
}
sink()

# Function to perform ANOVA and create boxplot
analyze_anova_factor <- function(data, factor_name, factor_label) {
  # Create formula for ANOVA
  formula <- as.formula(paste("Total ~", factor_name))
  
  # Perform ANOVA
  anova_result <- summary(aov(formula, data = data))
  p_value <- anova_result[[1]]$`Pr(>F)`[1]
  
  # Create boxplot
  plot <- ggplot(data, aes_string(x = paste0("factor(", factor_name, ")"), 
                                 y = "Total", 
                                 fill = paste0("factor(", factor_name, ")"))) +
    geom_boxplot() +
    labs(x = factor_label,
         y = "Total") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8)
    ) +
    ggtitle(paste0(factor_label, "\n", format_pvalue(p_value)))
  
  return(list(test = anova_result, plot = plot))
}

# Initialize lists for results and plots
anova_results <- list()
anova_plots <- list()

# Define the factors to analyze
anova_factors <- list(
  list(name = "BRCA", label = "BRCA"),
  list(name = "Histology..CNB.", label = "Histology (CNB)"),
  list(name = "Histology..P.", label = "Histology (P)")
)

# Analyze each factor
for(factor in anova_factors) {
  result <- analyze_anova_factor(data, factor$name, factor$label)
  anova_results[[factor$name]] <- result$test
  anova_plots[[factor$name]] <- result$plot
}

# Combine plots
anova_combined <- arrangeGrob(
  grobs = anova_plots,
  ncol = 3,
  top = textGrob("Clinical Characteristics", 
                 gp = gpar(fontsize = 12, fontface = "bold"))
)

# Save plots
ggsave("anova_analysis.pdf", anova_combined, width = 15, height = 5)
ggsave("anova_analysis.png", anova_combined, width = 15, height = 5, dpi = 300)

# Save ANOVA results to a text file
sink("anova_results.txt")
cat("ANOVA Results:\n\n")
for(factor in anova_factors) {
  cat(paste("\n", factor$label, ":\n"))
  print(anova_results[[factor$name]])
}
sink()

# Function to perform Spearman correlation and create scatter plot
analyze_spearman_correlation <- function(data, x_var, x_label) {
  # Perform Spearman correlation
  cor_test <- cor.test(data[[x_var]], data$Total, method = "spearman", exact = FALSE)
  rho <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Create scatter plot
  plot_data <- data.frame(x = data[[x_var]], y = data$Total)
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(x = x_label,
         y = "Total") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8)
    ) +
    ggtitle(paste0(x_label, "\nrho = ", sprintf("%.3f", rho), "\n", format_pvalue(p_value)))
  
  return(list(test = cor_test, plot = plot))
}

# Initialize lists for results and plots
spearman_results <- list()
spearman_plots <- list()

# Define the variables to analyze
spearman_vars <- list(
  list(name = "Duration", label = "Duration"),
  list(name = "Size", label = "Size"),
  list(name = "Mamma.24h", label = "Mamma 24h"),
  list(name = "Axilla.24h", label = "Axilla 24h"),
  list(name = "Drainage...M.", label = "Drainage (M)"),
  list(name = "Drainage..A.", label = "Drainage (A)"),
  list(name = "Follow.Up", label = "Follow-Up"),
  list(name = "Lenght.of.stay", label = "Length of stay")
)

# Analyze each variable
for(var in spearman_vars) {
  result <- analyze_spearman_correlation(data, var$name, var$label)
  spearman_results[[var$name]] <- result$test
  spearman_plots[[var$name]] <- result$plot
}

# Arrange plots in a grid (3 rows, 3 columns)
spearman_combined <- arrangeGrob(
  grobs = spearman_plots,
  ncol = 3,
  nrow = 3,
  top = textGrob("Therapeutic Factors", 
                 gp = gpar(fontsize = 12, fontface = "bold"))
)

# Save plots
ggsave("therapeutic_correlations.pdf", spearman_combined, width = 15, height = 15)
ggsave("therapeutic_correlations.png", spearman_combined, width = 15, height = 15, dpi = 300)

# Save Spearman correlation results to a text file
sink("therapeutic_correlations_results.txt")
cat("Therapeutic Factors - Spearman Correlation Results:\n\n")
for(var in spearman_vars) {
  cat(paste("\n", var$label, ":\n"))
  print(spearman_results[[var$name]])
}
sink()

# Function to create boxplot with either ANOVA or t-test
analyze_therapeutic_factor <- function(data, factor_name, test_type = "anova") {
  # Create formula for both tests
  formula <- as.formula(paste("Total ~", factor_name))
  
  # Perform statistical test
  if(test_type == "anova") {
    test_result <- summary(aov(formula, data = data))
    p_value <- test_result[[1]]$`Pr(>F)`[1]
    test_label <- "ANOVA"
  } else {
    test_result <- t.test(formula, data = data)
    p_value <- test_result$p.value
    test_label <- "t-test"
  }
  
  # Create boxplot
  plot <- ggplot(data, aes_string(x = paste0("factor(", factor_name, ")"), 
                                 y = "Total", 
                                 fill = paste0("factor(", factor_name, ")"))) +
    geom_boxplot() +
    labs(x = factor_name,
         y = "Total") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8)
    ) +
    ggtitle(paste0(factor_name, "\n", test_label, ": ", format_pvalue(p_value)))
  
  return(list(test = test_result, plot = plot))
}

# Initialize lists for results and plots
therapeutic_results <- list()
therapeutic_plots <- list()

# Define the factors to analyze with their test types
therapeutic_factors <- list(
  list(name = "CTX", test = "anova"),
  list(name = "Axilla", test = "anova"),
  list(name = "Mastectomy", test = "t.test")
)

# Analyze each factor
for(factor in therapeutic_factors) {
  result <- analyze_therapeutic_factor(data, factor$name, factor$test)
  therapeutic_results[[factor$name]] <- result$test
  therapeutic_plots[[factor$name]] <- result$plot
}

# Combine plots
therapeutic_combined <- arrangeGrob(
  grobs = therapeutic_plots,
  ncol = 3,
  top = textGrob("Therapeutic Factors", 
                 gp = gpar(fontsize = 12, fontface = "bold"))
)

# Save plots
ggsave("therapeutic_analysis.pdf", therapeutic_combined, width = 15, height = 5)
ggsave("therapeutic_analysis.png", therapeutic_combined, width = 15, height = 5, dpi = 300)

# Save statistical test results to a text file
sink("therapeutic_results.txt")
cat("Statistical Test Results:\n\n")
for(factor in therapeutic_factors) {
  cat(paste("\n", factor$name, " (", factor$test, "):\n", sep=""))
  print(therapeutic_results[[factor$name]])
}
sink()