# Load required libraries
library(ggplot2)
library(patchwork)

# Read the CSV file
data <- read.csv2("Test.csv")

# Convert Lenght.of.stay and Follow.Up to numeric if needed
if (!is.numeric(data$Lenght.of.stay)) {
  data$Lenght.of.stay <- as.numeric(as.character(data$Lenght.of.stay))
}
if (!is.numeric(data$Follow.Up)) {
  data$Follow.Up <- as.numeric(as.character(data$Follow.Up))
}
if (!is.numeric(data$Total)) {
  data$Total <- as.numeric(as.character(data$Total))
}

# Spearman for Follow-Up
cor_fu <- cor.test(data$Follow.Up, data$Total, method = "spearman")
label_fu <- paste0("Follow-Up: rho = ", round(cor_fu$estimate, 3), ", p = ", format.pval(cor_fu$p.value, digits = 3))

# Spearman for Lenght.of.stay
cor_los <- cor.test(data$Lenght.of.stay, data$Total, method = "spearman")
label_los <- paste0("Length of stay: rho = ", round(cor_los$estimate, 3), ", p = ", format.pval(cor_los$p.value, digits = 3))

# Plot 1: Total vs Follow-Up
p1 <- ggplot(data, aes(x = Follow.Up, y = Total)) +
  geom_point(color = "#0072B2", alpha = 0.6) +
  geom_smooth(method = "lm", color = "#0072B2", se = FALSE) +
  labs(title = "Total vs Follow-Up",
       subtitle = label_fu,
       x = "Follow-Up",
       y = "Total") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Plot 2: Total vs Lenght.of.stay
p2 <- ggplot(data, aes(x = Lenght.of.stay, y = Total)) +
  geom_point(color = "#D55E00", alpha = 0.6) +
  geom_smooth(method = "lm", color = "#D55E00", se = FALSE) +
  labs(title = "Total vs Length of stay",
       subtitle = label_los,
       x = "Length of stay",
       y = "Total") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Combine with patchwork
combined <- (p1 | p2) +
  plot_annotation(title = "Complication",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

ggsave("Karl/complication_total_separate.png", combined, width = 16, height = 8, dpi = 300) 