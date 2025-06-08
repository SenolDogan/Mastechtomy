# Load required libraries
library(ggplot2)
library(readr)

# Read the CSV file
data <- read.csv2("Test.csv")

# Perform t-test for Total (A) vs Mastectomy
t_test_mastectomy <- t.test(Total..A. ~ Mastectomy, data=data)

# Print t-test results
cat("\nT-Test Results (Total (A) vs Mastectomy):\n")
cat("t-statistic:", t_test_mastectomy$statistic, "\n")
cat("p-value:", t_test_mastectomy$p.value, "\n")
cat("95% Confidence Interval:", t_test_mastectomy$conf.int[1], "to", t_test_mastectomy$conf.int[2], "\n\n")

# Calculate means for each Mastectomy group
mean_no_mastectomy <- mean(data$Total..A.[data$Mastectomy == 0], na.rm=TRUE)
mean_with_mastectomy <- mean(data$Total..A.[data$Mastectomy == 1], na.rm=TRUE)

cat("Mean Total (A) for No Mastectomy:", mean_no_mastectomy, "\n")
cat("Mean Total (A) for Mastectomy:", mean_with_mastectomy, "\n\n")

# Create box plot with points for Mastectomy groups
p_mastectomy <- ggplot(data, aes(x = factor(Mastectomy), y = Total..A.)) +
  geom_boxplot(fill = c("lightblue", "lightgreen"), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_bw() +
  labs(title = paste("Total (A) Values by Mastectomy Status\n(t =", 
                    round(t_test_mastectomy$statistic, 3),
                    ", p =", format.pval(t_test_mastectomy$p.value, digits = 3), ")"),
       x = "Mastectomy (0=No, 1=Yes)",
       y = "Total (A)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

# Save the plot
ggsave("Karl/t_test_plot_mastectomy_totalA.png", p_mastectomy, width = 10, height = 8, dpi = 300)

# Perform ANOVA test for Total (A) vs BRCA
anova_brca <- aov(Total..A. ~ factor(BRCA), data=data)
anova_summary <- summary(anova_brca)

# Print ANOVA results
cat("\nANOVA Test Results (Total (A) vs BRCA):\n")
cat("F-statistic:", anova_summary[[1]]$"F value"[1], "\n")
cat("p-value:", anova_summary[[1]]$"Pr(>F)"[1], "\n\n")

# Calculate means for each BRCA group
brca_means <- tapply(data$Total..A., data$BRCA, mean, na.rm=TRUE)
cat("Mean Total (A) for each BRCA group:\n")
print(brca_means)
cat("\n")

# Create box plot with points for BRCA groups
p_brca <- ggplot(data, aes(x = factor(BRCA), y = Total..A.)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_bw() +
  labs(title = paste("Total (A) Values by BRCA Groups\n(F =", 
                    round(anova_summary[[1]]$"F value"[1], 3),
                    ", p =", format.pval(anova_summary[[1]]$"Pr(>F)"[1], digits = 3), ")"),
       x = "BRCA",
       y = "Total (A)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

# Save the plot
ggsave("Karl/anova_plot_brca_totalA.png", p_brca, width = 10, height = 8, dpi = 300) 