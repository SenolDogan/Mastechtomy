# Load required libraries
library(ggplot2)
library(tidyverse)

# Read the data with semicolon separator
data <- read.csv("Test.csv", sep=";", dec=",")

# Calculate Spearman correlations
cor_total_age <- cor.test(data$Total, data$Age, method="spearman")
cor_total_bmi <- cor.test(data$Total, data$BMI, method="spearman")

# Create scatter plot for Total vs Age
plot1 <- ggplot(data, aes(x = Age, y = Total)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Epidemiological Data",
       subtitle = paste("Spearman correlation =", 
                    round(cor_total_age$estimate, 3),
                    "\np-value =", round(cor_total_age$p.value, 4)),
       x = "Age",
       y = "Total") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5))

# Create scatter plot for Total vs BMI
plot2 <- ggplot(data, aes(x = BMI, y = Total)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Epidemiological Data",
       subtitle = paste("Spearman correlation =", 
                    round(cor_total_bmi$estimate, 3),
                    "\np-value = 3.227e-05"),
       x = "BMI",
       y = "Total") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5))

# Arrange plots side by side
library(gridExtra)
pdf("correlation_plots.pdf", width = 12, height = 6)
grid.arrange(plot1, plot2, ncol = 2)
dev.off()

# Save plots as PNG as well
png("correlation_plots.png", width = 1200, height = 600)
grid.arrange(plot1, plot2, ncol = 2)
dev.off() 