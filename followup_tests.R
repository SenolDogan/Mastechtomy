# Load required libraries
library(ggplot2)
library(readr)

# Read the CSV file
data <- read.csv2("Test.csv")

# Perform ANOVA test for Follow-Up vs Histology (CNB)
anova_histology <- aov(Follow.Up ~ factor(Histology..CNB.), data=data)
anova_summary <- summary(anova_histology)

# Print ANOVA results
cat("\nANOVA Test Results (Follow-Up vs Histology (CNB)):\n")
cat("F-statistic:", anova_summary[[1]]$"F value"[1], "\n")
cat("p-value:", anova_summary[[1]]$"Pr(>F)"[1], "\n\n")

# Calculate means for each Histology group
histology_means <- tapply(data$Follow.Up, data$Histology..CNB., mean, na.rm=TRUE)
cat("Mean Follow-Up for each Histology group:\n")
print(histology_means)
cat("\n")

# Create box plot with points for Histology groups
p_histology <- ggplot(data, aes(x = factor(Histology..CNB.), y = Follow.Up)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_bw() +
  labs(title = paste("Follow-Up Values by Histology Groups\n(F =", 
                    round(anova_summary[[1]]$"F value"[1], 3),
                    ", p =", format.pval(anova_summary[[1]]$"Pr(>F)"[1], digits = 3), ")"),
       x = "Histology (CNB)",
       y = "Follow-Up") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

# Save the plot
ggsave("Karl/anova_plot_histology_followup.png", p_histology, width = 10, height = 8, dpi = 300)

# Perform t-test for Follow-Up vs Previous surgery
t_test_surgery <- t.test(Follow.Up ~ Previous.surgery, data=data)

# Print t-test results
cat("\nT-Test Results (Follow-Up vs Previous surgery):\n")
cat("t-statistic:", t_test_surgery$statistic, "\n")
cat("p-value:", t_test_surgery$p.value, "\n")
cat("95% Confidence Interval:", t_test_surgery$conf.int[1], "to", t_test_surgery$conf.int[2], "\n\n")

# Calculate means for each Previous surgery group
mean_no_surgery <- mean(data$Follow.Up[data$Previous.surgery == 0], na.rm=TRUE)
mean_with_surgery <- mean(data$Follow.Up[data$Previous.surgery == 1], na.rm=TRUE)

cat("Mean Follow-Up for No Previous surgery:", mean_no_surgery, "\n")
cat("Mean Follow-Up for Previous surgery:", mean_with_surgery, "\n\n")

# Create box plot with points for Previous surgery groups
p_surgery <- ggplot(data, aes(x = factor(Previous.surgery), y = Follow.Up)) +
  geom_boxplot(fill = c("lightblue", "lightgreen"), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +
  theme_bw() +
  labs(title = paste("Follow-Up Values by Previous Surgery Status\n(t =", 
                    round(t_test_surgery$statistic, 3),
                    ", p =", format.pval(t_test_surgery$p.value, digits = 3), ")"),
       x = "Previous Surgery (0=No, 1=Yes)",
       y = "Follow-Up") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

# Save the plot
ggsave("Karl/t_test_plot_surgery_followup.png", p_surgery, width = 10, height = 8, dpi = 300) 