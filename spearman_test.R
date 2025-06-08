# Load required libraries
library(ggplot2)
library(readr)

# Read the CSV file
data <- read.csv2("Test.csv")

# Print structure of the data
print(str(data))

# Print summary of relevant columns
cat("\nSummary of Total column:\n")
print(summary(data$Total))

cat("\nSummary of Follow-Up column:\n")
print(summary(data$Follow.Up))

# Calculate correlation if data is available
if(!all(is.na(data$Total)) && !all(is.na(data$Follow.Up))) {
  cat("\nCalculating Spearman correlation...\n")
  result <- cor.test(data$Total, data$Follow.Up, 
                    method="spearman",
                    exact=FALSE,
                    use="complete.obs")
  
  print(result)
} else {
  cat("\nError: One or both columns contain only NA values\n")
}

# Convert to data frame
data <- as.data.frame(data)

# Print column names to verify
cat("\nColumn names in the dataset:\n")
print(names(data))

# Handle Drainage (M) column more carefully
# First, check if the column exists
if ("Drainage..M." %in% names(data)) {
  # Convert to character first to handle any potential formatting issues
  drainage_values <- as.character(data$`Drainage..M.`)
  
  # Remove any whitespace and replace commas with periods
  drainage_values <- gsub("\\s+", "", drainage_values)
  drainage_values <- gsub(",", ".", drainage_values)
  
  # Convert to numeric, with warning suppression
  data$`Drainage..M.` <- suppressWarnings(as.numeric(drainage_values))
  
  # Print summary to check the conversion
  cat("\nSummary of Drainage (M) column after conversion:\n")
  print(summary(data$`Drainage..M.`))
}

# Remove NA values for the correlation test
data_clean_drainage <- na.omit(data[, c("Total", "Drainage..M.")])

# Print number of complete cases
cat("\nNumber of complete cases for correlation:", nrow(data_clean_drainage), "\n")

# Perform Spearman correlation test for Total vs Drainage (M)
spearman_drainageM <- cor.test(data_clean_drainage$Total, data_clean_drainage$`Drainage..M.`, method="spearman", exact=FALSE)

# Print Spearman correlation results for Drainage (M)
cat("\nSpearman Correlation Results (Total vs Drainage M):\n")
cat("rho:", spearman_drainageM$estimate, "\n")
cat("p-value:", spearman_drainageM$p.value, "\n\n")

# Create scatter plot with regression line for Drainage (M)
p21 <- ggplot(data_clean_drainage, aes(x = `Drainage..M.`, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("Drainage (M) vs Total\n(Spearman rho =", 
                    round(spearman_drainageM$estimate, 3),
                    ", p =", round(spearman_drainageM$p.value, 3), ")"),
       x = "Drainage (M)",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("spearman_drainageM_regression.png", p21, width = 10, height = 8)

# Remove any NA values for the analysis
data_clean <- na.omit(data[, c("Total", "Histology..CNB.")])

# Diabetes is binary (0=No, 1=Yes)
data$Diabetes <- data$Diabetes

# Perform t-test
t_test_result <- t.test(Total ~ Diabetes, data=data)

# Print results
cat("\nT-Test Results (Total vs Diabetes):\n")
cat("t-statistic:", t_test_result$statistic, "\n")
cat("p-value:", t_test_result$p.value, "\n")
cat("95% Confidence Interval:", t_test_result$conf.int[1], "to", t_test_result$conf.int[2], "\n\n")

# Calculate means for each group
mean_no_diabetes <- mean(data$Total[data$Diabetes == 0], na.rm=TRUE)
mean_with_diabetes <- mean(data$Total[data$Diabetes == 1], na.rm=TRUE)

cat("Mean Total for No Diabetes:", mean_no_diabetes, "\n")
cat("Mean Total for Diabetes:", mean_with_diabetes, "\n\n")

# Create scatter plot with regression line
p1 <- ggplot(data, aes(x = factor(Diabetes), y = Total)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("Total Values by Diabetes Status\n(p =", round(t_test_result$p.value, 3), ")"),
       x = "Diabetes (0=No, 1=Yes)",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("t_test_plot_diabetes.png", p1, width = 10, height = 8)

# Create box plot with points
p2 <- ggplot(data, aes(x = factor(Diabetes), y = Total)) +
  geom_boxplot(fill = c("lightblue", "lightgreen"), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Box Plot of Total Values by Diabetes Status",
       x = "Diabetes (0=No, 1=Yes)",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the second plot
ggsave("boxplot_with_points_diabetes.png", p2, width = 10, height = 8)

# Calculate Spearman correlations
spearman_age <- cor.test(data$Age, data$Total, method="spearman", exact=FALSE)
spearman_bmi <- cor.test(data$BMI, data$Total, method="spearman", exact=FALSE)
spearman_weight <- cor.test(data$Weight, data$Total, method="spearman", exact=FALSE)

# Print Spearman correlation results
cat("\nSpearman Correlation Results:\n")
cat("\nAge vs Total:\n")
cat("rho:", spearman_age$estimate, "\n")
cat("p-value:", spearman_age$p.value, "\n")

cat("\nBMI vs Total:\n")
cat("rho:", spearman_bmi$estimate, "\n")
cat("p-value:", spearman_bmi$p.value, "\n")

cat("\nWeight vs Total:\n")
cat("rho:", spearman_weight$estimate, "\n")
cat("p-value:", spearman_weight$p.value, "\n")

# Create scatter plot with regression line for Age
p3 <- ggplot(data, aes(x = Age, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("Age vs Total (Spearman rho =", round(spearman_age$estimate, 3), 
                    ", p =", round(spearman_age$p.value, 3), ")"),
       x = "Age",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5, size = 11))

# Save Age plot
ggsave("spearman_age_regression.png", p3, width = 10, height = 8)

# Create scatter plot with regression line for BMI
p4 <- ggplot(data, aes(x = BMI, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("BMI vs Total (Spearman rho =", round(spearman_bmi$estimate, 3),
                    ", p =", round(spearman_bmi$p.value, 3), ")"),
       x = "BMI",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5, size = 11))

# Save BMI plot
ggsave("spearman_bmi_regression.png", p4, width = 10, height = 8)

# Create scatter plot with regression line for Weight
p5 <- ggplot(data, aes(x = Weight, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("Weight vs Total (Spearman rho =", round(spearman_weight$estimate, 3),
                    ", p =", round(spearman_weight$p.value, 3), ")"),
       x = "Weight",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5, size = 11))

# Save Weight plot
ggsave("spearman_weight_regression.png", p5, width = 10, height = 8)

# Perform ANOVA test for Total vs BRCA
anova_result <- aov(Total ~ factor(BRCA), data=data)
anova_summary <- summary(anova_result)

# Print ANOVA results
cat("\nANOVA Test Results (Total vs BRCA):\n")
cat("F-statistic:", anova_summary[[1]]$"F value"[1], "\n")
cat("p-value:", anova_summary[[1]]$"Pr(>F)"[1], "\n\n")

# Calculate means for each BRCA group
brca_means <- tapply(data$Total, data$BRCA, mean, na.rm=TRUE)
cat("Mean Total for each BRCA group:\n")
print(brca_means)
cat("\n")

# Create box plot with points for BRCA groups
p6 <- ggplot(data, aes(x = factor(BRCA), y = Total)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = paste("Total Values by BRCA Groups\n(F =", round(anova_summary[[1]]$"F value"[1], 3),
                    ", p =", round(anova_summary[[1]]$"Pr(>F)"[1], 3), ")"),
       x = "BRCA",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("anova_plot_brca.png", p6, width = 10, height = 8)

# Perform Spearman correlation test for Total vs Tumor size (T)
spearman_tumor <- cor.test(data$Total, data$pT, method="spearman", exact=FALSE)

# Print Spearman correlation results for Tumor size
cat("\nSpearman Correlation Results (Total vs Tumor Size):\n")
cat("rho:", spearman_tumor$estimate, "\n")
cat("p-value:", spearman_tumor$p.value, "\n\n")

# Create scatter plot with regression line for Tumor size
p1 <- ggplot(data, aes(x = pT, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("Tumor Size vs Total\n(Spearman rho =", 
                    round(spearman_tumor$estimate, 3),
                    ", p =", round(spearman_tumor$p.value, 3), ")"),
       x = "Tumor Size (pT)",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("spearman_tumor_regression.png", p1, width = 10, height = 8)

# Perform ANOVA test for Total vs Histology (CNB)
anova_histology <- aov(Total ~ factor(Histology..CNB.), data=data_clean)
anova_summary_hist <- summary(anova_histology)

# Print ANOVA results
cat("\nANOVA Test Results (Total vs Histology CNB):\n")
cat("F-statistic:", anova_summary_hist[[1]]$"F value"[1], "\n")
cat("p-value:", anova_summary_hist[[1]]$"Pr(>F)"[1], "\n\n")

# Calculate means for each Histology group
histology_means <- tapply(data_clean$Total, data_clean$Histology..CNB., mean, na.rm=TRUE)
cat("Mean Total for each Histology (CNB) group:\n")
print(histology_means)
cat("\n")

# Create box plot with points for Histology groups
p7 <- ggplot(data_clean, aes(x = factor(Histology..CNB.), y = Total)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = paste("Total Values by Histology (CNB) Groups\n(F =", 
                    round(anova_summary_hist[[1]]$"F value"[1], 3),
                    ", p =", round(anova_summary_hist[[1]]$"Pr(>F)"[1], 3), ")"),
       x = "Histology (CNB)",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("anova_plot_histology.png", p7, width = 12, height = 8)

# Perform Spearman correlation test for Total vs pN
spearman_pn <- cor.test(data$Total, data$pN, method="spearman", exact=FALSE)

# Print Spearman correlation results for pN
cat("\nSpearman Correlation Results (Total vs pN):\n")
cat("rho:", spearman_pn$estimate, "\n")
cat("p-value:", spearman_pn$p.value, "\n\n")

# Create scatter plot with regression line for pN
p8 <- ggplot(data, aes(x = pN, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("Lymph Node Status (pN) vs Total\n(Spearman rho =", 
                    round(spearman_pn$estimate, 3),
                    ", p =", round(spearman_pn$p.value, 3), ")"),
       x = "Lymph Node Status (pN)",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("spearman_pn_regression.png", p8, width = 10, height = 8)

# Perform Spearman correlation test for Total vs N(X/x)
spearman_nx <- cor.test(data$Total, data$N.X.x., method="spearman", exact=FALSE)

# Print Spearman correlation results for N(X/x)
cat("\nSpearman Correlation Results (Total vs N(X/x)):\n")
cat("rho:", spearman_nx$estimate, "\n")
cat("p-value:", spearman_nx$p.value, "\n\n")

# Create scatter plot with regression line for N(X/x)
p8 <- ggplot(data, aes(x = N.X.x., y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("N(X/x) vs Total\n(Spearman rho =", 
                    round(spearman_nx$estimate, 3),
                    ", p =", round(spearman_nx$p.value, 3), ")"),
       x = "N(X/x)",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("spearman_nx_regression.png", p8, width = 10, height = 8)

# Perform Spearman correlation test for Total vs N (x/X)
spearman_nxx <- cor.test(data$Total, data$`N..x.X.`, method="spearman", exact=FALSE)

# Print Spearman correlation results for N (x/X)
cat("\nSpearman Correlation Results (Total vs N (x/X)):\n")
cat("rho:", spearman_nxx$estimate, "\n")
cat("p-value:", spearman_nxx$p.value, "\n\n")

# Create scatter plot with regression line for N (x/X)
p10 <- ggplot(data, aes(x = `N..x.X.`, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("N (x/X) vs Total\n(Spearman rho =", 
                    round(spearman_nxx$estimate, 3),
                    ", p =", round(spearman_nxx$p.value, 3), ")"),
       x = "N (x/X)",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("spearman_nxx_regression.png", p10, width = 10, height = 8)

# Perform Spearman correlation test for Total vs L
spearman_l <- cor.test(data$Total, data$L, method="spearman", exact=FALSE)

# Print Spearman correlation results for L
cat("\nSpearman Correlation Results (Total vs L):\n")
cat("rho:", spearman_l$estimate, "\n")
cat("p-value:", spearman_l$p.value, "\n\n")

# Create scatter plot with regression line for L
p11 <- ggplot(data, aes(x = L, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("L vs Total\n(Spearman rho =", 
                    round(spearman_l$estimate, 3),
                    ", p =", round(spearman_l$p.value, 3), ")"),
       x = "L",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("spearman_l_regression.png", p11, width = 10, height = 8)

# Convert CRP to numeric, replacing any commas with dots
data$CRP <- as.numeric(gsub(",", ".", data$CRP))

# Perform Spearman correlation test for Total vs CRP
spearman_crp <- cor.test(data$Total, data$CRP, method="spearman", exact=FALSE)

# Print Spearman correlation results for CRP
cat("\nSpearman Correlation Results (Total vs CRP):\n")
cat("rho:", spearman_crp$estimate, "\n")
cat("p-value:", spearman_crp$p.value, "\n\n")

# Create scatter plot with regression line for CRP
p12 <- ggplot(data, aes(x = CRP, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("CRP vs Total\n(Spearman rho =", 
                    round(spearman_crp$estimate, 3),
                    ", p =", round(spearman_crp$p.value, 3), ")"),
       x = "CRP",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("spearman_crp_regression.png", p12, width = 10, height = 8)

# Remove any NA values for Histology (P) analysis
data_clean_histp <- na.omit(data[, c("Total", "Histology..P.")])

# Perform ANOVA test for Total vs Histology (P)
anova_histp <- aov(Total ~ factor(Histology..P.), data=data_clean_histp)
anova_summary_histp <- summary(anova_histp)

# Print ANOVA results
cat("\nANOVA Test Results (Total vs Histology P):\n")
cat("F-statistic:", anova_summary_histp[[1]]$"F value"[1], "\n")
cat("p-value:", anova_summary_histp[[1]]$"Pr(>F)"[1], "\n\n")

# Calculate means for each Histology (P) group
histp_means <- tapply(data_clean_histp$Total, data_clean_histp$Histology..P., mean, na.rm=TRUE)
cat("Mean Total for each Histology (P) group:\n")
print(histp_means)
cat("\n")

# Create box plot with points for Histology (P) groups
p13 <- ggplot(data_clean_histp, aes(x = factor(Histology..P.), y = Total)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = paste("Total Values by Histology (P) Groups\n(F =", 
                    round(anova_summary_histp[[1]]$"F value"[1], 3),
                    ", p =", round(anova_summary_histp[[1]]$"Pr(>F)"[1], 3), ")"),
       x = "Histology (P)",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("anova_plot_histology_p.png", p13, width = 12, height = 8)

# Remove any NA values for CTX analysis
data_clean_ctx <- na.omit(data[, c("Total", "CTX")])

# Perform ANOVA test for Total vs CTX
anova_ctx <- aov(Total ~ factor(CTX), data=data_clean_ctx)
anova_summary_ctx <- summary(anova_ctx)

# Print ANOVA results
cat("\nANOVA Test Results (Total vs CTX):\n")
cat("F-statistic:", anova_summary_ctx[[1]]$"F value"[1], "\n")
cat("p-value:", anova_summary_ctx[[1]]$"Pr(>F)"[1], "\n\n")

# Calculate means for each CTX group
ctx_means <- tapply(data_clean_ctx$Total, data_clean_ctx$CTX, mean, na.rm=TRUE)
cat("Mean Total for each CTX group:\n")
print(ctx_means)
cat("\n")

# Create box plot with points for CTX groups
p14 <- ggplot(data_clean_ctx, aes(x = factor(CTX), y = Total)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = paste("Total Values by CTX Groups\n(F =", 
                    round(anova_summary_ctx[[1]]$"F value"[1], 3),
                    ", p =", round(anova_summary_ctx[[1]]$"Pr(>F)"[1], 3), ")"),
       x = "CTX",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("anova_plot_ctx.png", p14, width = 10, height = 8)

# Perform Spearman correlation test for Total vs Duration
spearman_duration <- cor.test(data$Total, data$Duration, method="spearman", exact=FALSE)

# Print Spearman correlation results for Duration
cat("\nSpearman Correlation Results (Total vs Duration):\n")
cat("rho:", spearman_duration$estimate, "\n")
cat("p-value:", spearman_duration$p.value, "\n\n")

# Create scatter plot with regression line for Duration
p15 <- ggplot(data, aes(x = Duration, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("Duration vs Total\n(Spearman rho =", 
                    round(spearman_duration$estimate, 3),
                    ", p =", round(spearman_duration$p.value, 3), ")"),
       x = "Duration",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("spearman_duration_regression.png", p15, width = 10, height = 8)

# Perform Spearman correlation test for Total vs Size
spearman_size <- cor.test(data$Total, data$Size, method="spearman", exact=FALSE)

# Print Spearman correlation results for Size
cat("\nSpearman Correlation Results (Total vs Size):\n")
cat("rho:", spearman_size$estimate, "\n")
cat("p-value:", spearman_size$p.value, "\n\n")

# Create scatter plot with regression line for Size
p16 <- ggplot(data, aes(x = Size, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("Size vs Total\n(Spearman rho =", 
                    round(spearman_size$estimate, 3),
                    ", p =", round(spearman_size$p.value, 3), ")"),
       x = "Size",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("spearman_size_regression.png", p16, width = 10, height = 8)

# Perform t-test for Total vs Mastectomy
t_test_mastectomy <- t.test(Total ~ Mastectomy, data=data)

# Print results
cat("\nT-Test Results (Total vs Mastectomy):\n")
cat("t-statistic:", t_test_mastectomy$statistic, "\n")
cat("p-value:", t_test_mastectomy$p.value, "\n")
cat("95% Confidence Interval:", t_test_mastectomy$conf.int[1], "to", t_test_mastectomy$conf.int[2], "\n\n")

# Calculate means for each Mastectomy group
mean_no_mastectomy <- mean(data$Total[data$Mastectomy == 0], na.rm=TRUE)
mean_with_mastectomy <- mean(data$Total[data$Mastectomy == 1], na.rm=TRUE)

cat("Mean Total for No Mastectomy:", mean_no_mastectomy, "\n")
cat("Mean Total for Mastectomy:", mean_with_mastectomy, "\n\n")

# Create box plot with points for Mastectomy groups
p17 <- ggplot(data, aes(x = factor(Mastectomy), y = Total)) +
  geom_boxplot(fill = c("lightblue", "lightgreen"), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = paste("Total Values by Mastectomy Status\n(t =", 
                    round(t_test_mastectomy$statistic, 3),
                    ", p =", round(t_test_mastectomy$p.value, 3), ")"),
       x = "Mastectomy (0=No, 1=Yes)",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("t_test_plot_mastectomy.png", p17, width = 10, height = 8)

# Remove any NA values for Axilla analysis
data_clean_axilla <- na.omit(data[, c("Total", "Axilla")])

# Perform ANOVA test for Total vs Axilla
anova_axilla <- aov(Total ~ factor(Axilla), data=data_clean_axilla)
anova_summary_axilla <- summary(anova_axilla)

# Print ANOVA results
cat("\nANOVA Test Results (Total vs Axilla):\n")
cat("F-statistic:", anova_summary_axilla[[1]]$"F value"[1], "\n")
cat("p-value:", anova_summary_axilla[[1]]$"Pr(>F)"[1], "\n\n")

# Calculate means for each Axilla group
axilla_means <- tapply(data_clean_axilla$Total, data_clean_axilla$Axilla, mean, na.rm=TRUE)
cat("Mean Total for each Axilla group:\n")
print(axilla_means)
cat("\n")

# Create box plot with points for Axilla groups
p18 <- ggplot(data_clean_axilla, aes(x = factor(Axilla), y = Total)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = paste("Total Values by Axilla Groups\n(F =", 
                    round(anova_summary_axilla[[1]]$"F value"[1], 3),
                    ", p =", round(anova_summary_axilla[[1]]$"Pr(>F)"[1], 3), ")"),
       x = "Axilla",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("anova_plot_axilla.png", p18, width = 10, height = 8)

# Perform Spearman correlation test for Total vs Mamma 24h
spearman_mamma24h <- cor.test(data$Total, data$`Mamma.24h`, method="spearman", exact=FALSE)

# Print Spearman correlation results for Mamma 24h
cat("\nSpearman Correlation Results (Total vs Mamma 24h):\n")
cat("rho:", spearman_mamma24h$estimate, "\n")
cat("p-value:", spearman_mamma24h$p.value, "\n\n")

# Create scatter plot with regression line for Mamma 24h
p19 <- ggplot(data, aes(x = `Mamma.24h`, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("Mamma 24h vs Total\n(Spearman rho =", 
                    round(spearman_mamma24h$estimate, 3),
                    ", p =", round(spearman_mamma24h$p.value, 3), ")"),
       x = "Mamma 24h",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("spearman_mamma24h_regression.png", p19, width = 10, height = 8)

# Perform Spearman correlation test for Total vs Axilla 24h
spearman_axilla24h <- cor.test(data$Total, data$`Axilla.24h`, method="spearman", exact=FALSE)

# Print Spearman correlation results for Axilla 24h
cat("\nSpearman Correlation Results (Total vs Axilla 24h):\n")
cat("rho:", spearman_axilla24h$estimate, "\n")
cat("p-value:", spearman_axilla24h$p.value, "\n\n")

# Create scatter plot with regression line for Axilla 24h
p20 <- ggplot(data, aes(x = `Axilla.24h`, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("Axilla 24h vs Total\n(Spearman rho =", 
                    round(spearman_axilla24h$estimate, 3),
                    ", p =", round(spearman_axilla24h$p.value, 3), ")"),
       x = "Axilla 24h",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("spearman_axilla24h_regression.png", p20, width = 10, height = 8)

# Remove any NA values for the correlation test
data_clean <- na.omit(data[, c("Total", "Follow.Up")])

# Calculate Spearman correlation
result <- cor.test(data_clean$Total, 
                   data_clean$Follow.Up, 
                   method = "spearman", 
                   exact = FALSE,
                   use = "complete.obs")

# Print results with clear formatting
cat("\n=================================================")
cat("\nSpearman Correlation Results: Follow-Up vs Total")
cat("\n=================================================")
cat("\nNumber of observations:", nrow(data_clean))
cat("\nSpearman's rho coefficient:", round(result$estimate, 3))
cat("\np-value:", format.pval(result$p.value, digits = 3))
cat("\n=================================================\n\n")

# Create scatter plot
ggplot(data_clean, aes(x = Follow.Up, y = Total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = paste("Follow-Up vs Total\n(Spearman rho =", 
                    round(result$estimate, 3),
                    ", p =", round(result$p.value, 3), ")"),
       x = "Follow-Up",
       y = "Total") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Karl/spearman_followup_regression.png", width = 10, height = 8)

# Print summary statistics
cat("\nSummary of Length of stay:\n")
print(summary(data$Lenght.of.stay))

cat("\nSummary of Total:\n")
print(summary(data$Total))

# Calculate correlation
result <- cor.test(data$Total, data$Lenght.of.stay, 
                  method = "spearman",
                  exact = FALSE)

# Print results
cat("\n=================================================")
cat("\nSpearman Correlation Results: Length of stay vs Total")
cat("\n=================================================")
cat("\nNumber of observations:", length(na.omit(data$Total)))
cat("\nSpearman's rho coefficient:", round(result$estimate, 3))
cat("\np-value:", format.pval(result$p.value, digits = 3))
cat("\n=================================================\n")

# Perform Spearman correlation test for Follow-Up vs Age
spearman_followup_age <- cor.test(data$Follow.Up, data$Age, method="spearman", exact=FALSE)

# Print Spearman correlation results for Follow-Up vs Age
cat("\nSpearman Correlation Results (Follow-Up vs Age):\n")
cat("rho:", spearman_followup_age$estimate, "\n")
cat("p-value:", spearman_followup_age$p.value, "\n\n")

# Create scatter plot with regression line for Follow-Up vs Age
p_followup_age <- ggplot(data, aes(x = Age, y = Follow.Up)) +
  geom_point(alpha = 0.6, color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = paste("Age vs Follow-Up\n(Spearman rho =", 
                    round(spearman_followup_age$estimate, 3),
                    ", p =", round(spearman_followup_age$p.value, 3), ")"),
       x = "Age",
       y = "Follow-Up") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Save the plot
ggsave("Karl/spearman_followup_age_regression.png", p_followup_age, width = 10, height = 8, dpi = 300)

# Perform Spearman correlation test for Follow-Up vs BMI
spearman_followup_bmi <- cor.test(data$Follow.Up, data$BMI, method="spearman", exact=FALSE)

# Print Spearman correlation results for Follow-Up vs BMI
cat("\nSpearman Correlation Results (Follow-Up vs BMI):\n")
cat("rho:", spearman_followup_bmi$estimate, "\n")
cat("p-value:", spearman_followup_bmi$p.value, "\n\n")

# Create scatter plot with regression line for Follow-Up vs BMI
p_followup_bmi <- ggplot(data, aes(x = BMI, y = Follow.Up)) +
  geom_point(alpha = 0.6, color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = paste("BMI vs Follow-Up\n(Spearman rho =", 
                    round(spearman_followup_bmi$estimate, 3),
                    ", p =", round(spearman_followup_bmi$p.value, 3), ")"),
       x = "BMI",
       y = "Follow-Up") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Save the plot
ggsave("Karl/spearman_followup_bmi_regression.png", p_followup_bmi, width = 10, height = 8, dpi = 300)

# Perform Spearman correlation test for Follow-Up vs Duration
spearman_followup_duration <- cor.test(data$Follow.Up, data$Duration, method="spearman", exact=FALSE)

# Print Spearman correlation results for Follow-Up vs Duration
cat("\nSpearman Correlation Results (Follow-Up vs Duration):\n")
cat("rho:", spearman_followup_duration$estimate, "\n")
cat("p-value:", spearman_followup_duration$p.value, "\n\n")

# Create scatter plot with regression line for Follow-Up vs Duration
p_followup_duration <- ggplot(data, aes(x = Duration, y = Follow.Up)) +
  geom_point(alpha = 0.6, color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = paste("Duration vs Follow-Up\n(Spearman rho =", 
                    round(spearman_followup_duration$estimate, 3),
                    ", p =", round(spearman_followup_duration$p.value, 3), ")"),
       x = "Duration",
       y = "Follow-Up") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Save the plot
ggsave("Karl/spearman_followup_duration_regression.png", p_followup_duration, width = 10, height = 8, dpi = 300) 