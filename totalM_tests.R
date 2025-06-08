# Load required libraries
library(ggplot2)
library(readr)

# Read the CSV file
data <- read.csv2("Test.csv")

# Perform Spearman correlation test for Total (M) vs Size
cor_test_size <- cor.test(data$Total..M., data$Size, method = "spearman")

# Print correlation results
cat("\nSpearman Correlation Results (Total (M) vs Size):\n")
cat("rho:", cor_test_size$estimate, "\n")
cat("p-value:", cor_test_size$p.value, "\n\n")

# Create scatter plot for Total (M) vs Size
p_size <- ggplot(data, aes(x = Size, y = Total..M.)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_bw() +
  labs(title = paste("Total (M) vs Size\nSpearman's rho =", 
                    round(cor_test_size$estimate, 3),
                    ", p =", format.pval(cor_test_size$p.value, digits = 3)),
       x = "Size",
       y = "Total (M)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

# Save the plot
ggsave("Karl/spearman_totalM_size.png", p_size, width = 10, height = 8, dpi = 300)

# Perform Spearman correlation test for Total (M) vs Duration
cor_test_duration <- cor.test(data$Total..M., data$Duration, method = "spearman")

# Print correlation results
cat("\nSpearman Correlation Results (Total (M) vs Duration):\n")
cat("rho:", cor_test_duration$estimate, "\n")
cat("p-value:", cor_test_duration$p.value, "\n\n")

# Create scatter plot for Total (M) vs Duration
p_duration <- ggplot(data, aes(x = Duration, y = Total..M.)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_bw() +
  labs(title = paste("Total (M) vs Duration\nSpearman's rho =", 
                    round(cor_test_duration$estimate, 3),
                    ", p =", format.pval(cor_test_duration$p.value, digits = 3)),
       x = "Duration",
       y = "Total (M)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

# Save the plot
ggsave("Karl/spearman_totalM_duration.png", p_duration, width = 10, height = 8, dpi = 300)

# Perform ANOVA test for Total (M) vs BRCA
anova_brca <- aov(Total..M. ~ factor(BRCA), data=data)
anova_summary <- summary(anova_brca)

# Print ANOVA results
cat("\nANOVA Test Results (Total (M) vs BRCA):\n")
cat("F-statistic:", anova_summary[[1]]$"F value"[1], "\n")
cat("p-value:", anova_summary[[1]]$"Pr(>F)"[1], "\n\n")

# Calculate means for each BRCA group
brca_means <- tapply(data$Total..M., data$BRCA, mean, na.rm=TRUE)
cat("Mean Total (M) for each BRCA group:\n")
print(brca_means)
cat("\n")

# Create box plot with points for BRCA groups
p_brca <- ggplot(data, aes(x = factor(BRCA), y = Total..M.)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_bw() +
  labs(title = paste("Total (M) Values by BRCA Groups\n(F =", 
                    round(anova_summary[[1]]$"F value"[1], 3),
                    ", p =", format.pval(anova_summary[[1]]$"Pr(>F)"[1], digits = 3), ")"),
       x = "BRCA",
       y = "Total (M)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

# Save the plot
ggsave("Karl/anova_plot_brca_totalM.png", p_brca, width = 10, height = 8, dpi = 300)

# Spearman correlation: Total (M) vs Follow-Up
cor_test_followup <- cor.test(data$Total..M., data$Follow.Up, method = "spearman")
cat("\nSpearman Correlation Results (Total (M) vs Follow-Up):\n")
cat("rho:", cor_test_followup$estimate, "\n")
cat("p-value:", cor_test_followup$p.value, "\n\n")

# Spearman correlation: Total (M) vs Length of stay
# Convert Length.of.stay to numeric if not already
if (!is.numeric(data$Length.of.stay)) {
  data$Length.of.stay <- as.numeric(as.character(data$Length.of.stay))
}
cor_test_los <- cor.test(data$Total..M., data$Length.of.stay, method = "spearman")
cat("\nSpearman Correlation Results (Total (M) vs Length of stay):\n")
cat("rho:", cor_test_los$estimate, "\n")
cat("p-value:", cor_test_los$p.value, "\n\n") 