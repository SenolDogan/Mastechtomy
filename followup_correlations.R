# Load required libraries
library(ggplot2)
library(readr)

# Read the CSV file
data <- read.csv2("Test.csv")

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
  theme_bw() +
  labs(title = paste("Age vs Follow-Up\n(Spearman rho =", 
                    round(spearman_followup_age$estimate, 3),
                    ", p =", format.pval(spearman_followup_age$p.value, digits = 3), ")"),
       x = "Age",
       y = "Follow-Up") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

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
  theme_bw() +
  labs(title = paste("BMI vs Follow-Up\n(Spearman rho =", 
                    round(spearman_followup_bmi$estimate, 3),
                    ", p =", format.pval(spearman_followup_bmi$p.value, digits = 3), ")"),
       x = "BMI",
       y = "Follow-Up") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

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
  theme_bw() +
  labs(title = paste("Duration vs Follow-Up\n(Spearman rho =", 
                    round(spearman_followup_duration$estimate, 3),
                    ", p =", format.pval(spearman_followup_duration$p.value, digits = 3), ")"),
       x = "Duration",
       y = "Follow-Up") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

# Save the plot
ggsave("Karl/spearman_followup_duration_regression.png", p_followup_duration, width = 10, height = 8, dpi = 300) 