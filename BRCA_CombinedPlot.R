# Load required libraries
library(ggplot2)
library(patchwork)

# Read the data
data <- read.csv2("Test.csv", stringsAsFactors = FALSE)
colnames(data) <- trimws(colnames(data))

# Clean numeric columns
data$`Total..M.` <- as.numeric(gsub(",", ".", data$`Total..M.`))
data$`Total..A.` <- as.numeric(gsub(",", ".", data$`Total..A.`))
data$Total <- as.numeric(gsub(",", ".", data$Total))
data$BRCA <- as.factor(data$BRCA)

# 1. ANOVA: Total (M) vs BRCA
anova_M <- aov(`Total..M.` ~ BRCA, data=data)
sum_M <- summary(anova_M)
p1 <- ggplot(data, aes(x = BRCA, y = `Total..M.`)) +
  geom_boxplot(fill = "#0072B2", alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "#0072B2") +
  labs(
    title = "Total (M) vs BRCA",
    subtitle = paste0("ANOVA: F = ", round(sum_M[[1]]$"F value"[1], 2), ", p = ", format.pval(sum_M[[1]]$"Pr(>F)"[1], digits = 5)),
    x = "BRCA", y = "Total (M)"
  ) + theme_minimal()

# 2. ANOVA: Total (A) vs BRCA
anova_A <- aov(`Total..A.` ~ BRCA, data=data)
sum_A <- summary(anova_A)
p2 <- ggplot(data, aes(x = BRCA, y = `Total..A.`)) +
  geom_boxplot(fill = "#D55E00", alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "#D55E00") +
  labs(
    title = "Total (A) vs BRCA",
    subtitle = paste0("ANOVA: F = ", round(sum_A[[1]]$"F value"[1], 2), ", p = ", format.pval(sum_A[[1]]$"Pr(>F)"[1], digits = 5)),
    x = "BRCA", y = "Total (A)"
  ) + theme_minimal()

# 3. ANOVA: Total vs BRCA
anova_T <- aov(Total ~ BRCA, data=data)
sum_T <- summary(anova_T)
p3 <- ggplot(data, aes(x = BRCA, y = Total)) +
  geom_boxplot(fill = "#009E73", alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "#009E73") +
  labs(
    title = "Total vs BRCA",
    subtitle = paste0("ANOVA: F = ", round(sum_T[[1]]$"F value"[1], 2), ", p = ", format.pval(sum_T[[1]]$"Pr(>F)"[1], digits = 5)),
    x = "BRCA", y = "Total"
  ) + theme_minimal()

# Combine all 3 plots in a single row
combined <- (p1 | p2 | p3) +
  plot_annotation(title = "BRCA",
                  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))

ggsave("Karl/brca_combined.png", combined, width = 18, height = 6, dpi = 300) 