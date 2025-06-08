# Load required libraries
library(readr)

# Read the CSV file
data <- read.csv2("Test.csv")
colnames(data) <- trimws(colnames(data))

# Convert variables to numeric if needed
num_vars <- c("Total", "CRP", "Follow.Up")
for (v in num_vars) {
  if (!is.numeric(data[[v]])) {
    data[[v]] <- as.numeric(as.character(data[[v]]))
  }
}

# 1. Spearman: Total vs CRP
spearman_crp <- cor.test(data$Total, data$CRP, method = "spearman")

# 2. ANOVA: Total vs Histology (CNB)
anova_total_hist <- aov(Total ~ factor(Histology..CNB.), data=data)
anova_total_hist_sum <- summary(anova_total_hist)

# 3. ANOVA: Follow-Up vs Histology (CNB)
anova_fu_hist <- aov(Follow.Up ~ factor(Histology..CNB.), data=data)
anova_fu_hist_sum <- summary(anova_fu_hist)

# 4. T-Test: Total vs Previous surgery
ttest_total_prev <- t.test(Total ~ Previous.surgery, data=data)

# 5. T-Test: Follow-Up vs Previous surgery
ttest_fu_prev <- t.test(Follow.Up ~ Previous.surgery, data=data)

# 6. T-Test: Total vs Hypertension
ttest_total_hyp <- t.test(Total ~ Hypertension, data=data)

# 7. T-Test: Total vs Diabetes
ttest_total_diab <- t.test(Total ~ Diabetes, data=data)

# Prepare results table
results <- data.frame(
  y_Axis = c("Total", "Total", "Follow-Up", "Total", "Follow-Up", "Total", "Total"),
  x_Axis = c("CRP", "Histology (CNB)", "Histology (CNB)", "Previous surgery", "Previous surgery", "Hypertension", "Diabetes"),
  Test = c("Spearman", "ANOVA", "ANOVA", "T-Test", "T-Test", "T-Test", "T-Test"),
  P_value = c(
    format.pval(spearman_crp$p.value, digits=3),
    format.pval(anova_total_hist_sum[[1]]$"Pr(>F)"[1], digits=3),
    format.pval(anova_fu_hist_sum[[1]]$"Pr(>F)"[1], digits=3),
    format.pval(ttest_total_prev$p.value, digits=3),
    format.pval(ttest_fu_prev$p.value, digits=3),
    format.pval(ttest_total_hyp$p.value, digits=3),
    format.pval(ttest_total_diab$p.value, digits=3)
  ),
  Statistic = c(
    round(spearman_crp$estimate, 3),
    round(anova_total_hist_sum[[1]]$"F value"[1], 3),
    round(anova_fu_hist_sum[[1]]$"F value"[1], 3),
    round(ttest_total_prev$statistic, 3),
    round(ttest_fu_prev$statistic, 3),
    round(ttest_total_hyp$statistic, 3),
    round(ttest_total_diab$statistic, 3)
  ),
  stringsAsFactors = FALSE
)

# Print results as a table
print(results, row.names = FALSE) 