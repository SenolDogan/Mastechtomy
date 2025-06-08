# Load required libraries
library(ggplot2)
library(readr)

# Read the CSV file
data <- read.csv2("Test.csv")

# Perform Spearman correlation test for Total (A) vs N(X/x)
spearman_totalA_nx <- cor.test(data$Total..A., data$N.X.x., method="spearman", exact=FALSE)

# Print Spearman correlation results for Total (A) vs N(X/x)
cat("\nSpearman Correlation Results (Total (A) vs N(X/x)):\n")
cat("rho:", spearman_totalA_nx$estimate, "\n")
cat("p-value:", spearman_totalA_nx$p.value, "\n\n")

# Create scatter plot with regression line for Total (A) vs N(X/x)
p_totalA_nx <- ggplot(data, aes(x = N.X.x., y = Total..A.)) +
  geom_point(alpha = 0.6, color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_bw() +
  labs(title = paste("N(X/x) vs Total (A)\n(Spearman rho =", 
                    round(spearman_totalA_nx$estimate, 3),
                    ", p =", format.pval(spearman_totalA_nx$p.value, digits = 3), ")"),
       x = "N(X/x)",
       y = "Total (A)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

# Save the plot
ggsave("Karl/spearman_totalA_nx_regression.png", p_totalA_nx, width = 10, height = 8, dpi = 300)

# Perform Spearman correlation test for Total (A) vs N(x/X)
spearman_totalA_nxx <- cor.test(data$Total..A., data$`N..x.X.`, method="spearman", exact=FALSE)

# Print Spearman correlation results for Total (A) vs N(x/X)
cat("\nSpearman Correlation Results (Total (A) vs N(x/X)):\n")
cat("rho:", spearman_totalA_nxx$estimate, "\n")
cat("p-value:", spearman_totalA_nxx$p.value, "\n\n")

# Create scatter plot with regression line for Total (A) vs N(x/X)
p_totalA_nxx <- ggplot(data, aes(x = `N..x.X.`, y = Total..A.)) +
  geom_point(alpha = 0.6, color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_bw() +
  labs(title = paste("N(x/X) vs Total (A)\n(Spearman rho =", 
                    round(spearman_totalA_nxx$estimate, 3),
                    ", p =", format.pval(spearman_totalA_nxx$p.value, digits = 3), ")"),
       x = "N(x/X)",
       y = "Total (A)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

# Save the plot
ggsave("Karl/spearman_totalA_nxx_regression.png", p_totalA_nxx, width = 10, height = 8, dpi = 300)

# Perform Spearman correlation test for Total (A) vs L
spearman_totalA_l <- cor.test(data$Total..A., data$L, method="spearman", exact=FALSE)

# Print Spearman correlation results for Total (A) vs L
cat("\nSpearman Correlation Results (Total (A) vs L):\n")
cat("rho:", spearman_totalA_l$estimate, "\n")
cat("p-value:", spearman_totalA_l$p.value, "\n\n")

# Create scatter plot with regression line for Total (A) vs L
p_totalA_l <- ggplot(data, aes(x = L, y = Total..A.)) +
  geom_point(alpha = 0.6, color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_bw() +
  labs(title = paste("L vs Total (A)\n(Spearman rho =", 
                    round(spearman_totalA_l$estimate, 3),
                    ", p =", format.pval(spearman_totalA_l$p.value, digits = 3), ")"),
       x = "L",
       y = "Total (A)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))

# Save the plot
ggsave("Karl/spearman_totalA_l_regression.png", p_totalA_l, width = 10, height = 8, dpi = 300) 