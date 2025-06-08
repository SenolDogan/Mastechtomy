# Load required libraries
library(ggplot2)
library(patchwork)

# Read the data
data <- read.csv2("Test.csv", stringsAsFactors = FALSE)
colnames(data) <- trimws(colnames(data))

# Clean numeric columns
data$Total <- as.numeric(gsub(",", ".", data$Total))
data$Follow.Up <- as.numeric(gsub(",", ".", data$Follow.Up))
data$Age <- as.numeric(gsub(",", ".", data$Age))
data$BMI <- as.numeric(gsub(",", ".", data$BMI))

# 1. Spearman: Total vs Age
complete1 <- complete.cases(data$Total, data$Age)
s1 <- cor.test(data$Total[complete1], data$Age[complete1], method = "spearman")
p1 <- ggplot(data[complete1,], aes(x = Age, y = Total)) +
  geom_point(color = "#0072B2", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Total vs Age",
    subtitle = paste0("Spearman: rho = ", round(s1$estimate, 2), ", p = ", format.pval(s1$p.value, digits = 5)),
    x = "Age", y = "Total"
  ) + theme_minimal()

# 2. Spearman: Follow-Up vs Age
complete2 <- complete.cases(data$Follow.Up, data$Age)
s2 <- cor.test(data$Follow.Up[complete2], data$Age[complete2], method = "spearman")
p2 <- ggplot(data[complete2,], aes(x = Age, y = Follow.Up)) +
  geom_point(color = "#D55E00", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Follow-Up vs Age",
    subtitle = paste0("Spearman: rho = ", round(s2$estimate, 2), ", p = ", format.pval(s2$p.value, digits = 5)),
    x = "Age", y = "Follow-Up"
  ) + theme_minimal()

# 3. Spearman: Total vs BMI
complete3 <- complete.cases(data$Total, data$BMI)
s3 <- cor.test(data$Total[complete3], data$BMI[complete3], method = "spearman")
p3 <- ggplot(data[complete3,], aes(x = BMI, y = Total)) +
  geom_point(color = "#009E73", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Total vs BMI",
    subtitle = paste0("Spearman: rho = ", round(s3$estimate, 2), ", p = ", format.pval(s3$p.value, digits = 5)),
    x = "BMI", y = "Total"
  ) + theme_minimal()

# 4. Spearman: Follow-Up vs BMI
complete4 <- complete.cases(data$Follow.Up, data$BMI)
s4 <- cor.test(data$Follow.Up[complete4], data$BMI[complete4], method = "spearman")
p4 <- ggplot(data[complete4,], aes(x = BMI, y = Follow.Up)) +
  geom_point(color = "#CC79A7", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Follow-Up vs BMI",
    subtitle = paste0("Spearman: rho = ", round(s4$estimate, 2), ", p = ", format.pval(s4$p.value, digits = 5)),
    x = "BMI", y = "Follow-Up"
  ) + theme_minimal()

# Combine all 4 plots in a 2x2 grid
combined <- (
  (p1 | p2) /
  (p3 | p4)
) +
  plot_annotation(title = "Epidemiological Data",
                  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))

ggsave("Karl/epidemiological_data_combined.png", combined, width = 16, height = 12, dpi = 300) 