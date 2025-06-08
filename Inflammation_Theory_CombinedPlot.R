# Load required libraries
library(ggplot2)
library(patchwork)
library(readr)

# Read the data
data <- read.csv2("Test.csv", stringsAsFactors = FALSE)
colnames(data) <- trimws(colnames(data))

# Clean numeric columns
clean_numeric <- function(x) {
  x <- as.character(x)
  x <- gsub(",", ".", x)
  x <- gsub("\\s+", "", x)
  as.numeric(x)
}
data$Total <- clean_numeric(data$Total)
data$CRP <- clean_numeric(gsub("<", "", data$CRP))
data$Follow.Up <- clean_numeric(data$Follow.Up)

# Helper for plot theme
my_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black", size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

# 1. Spearman: Total vs CRP
complete_crp <- complete.cases(data$Total, data$CRP)
spearman_crp <- cor.test(data$Total[complete_crp], data$CRP[complete_crp], method = "spearman")
p1 <- ggplot(data[complete_crp,], aes(x = CRP, y = Total)) +
  geom_point(color = "#0072B2", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Total vs CRP",
    subtitle = paste0("Spearman: rho = ", round(spearman_crp$estimate, 2), ", p = ", format.pval(spearman_crp$p.value, digits = 5)),
    x = "CRP", y = "Total"
  ) + my_theme

# 2. ANOVA: Total vs Histology (CNB)
if("Histology..CNB." %in% names(data)) {
  anova_total_hist <- aov(Total ~ factor(Histology..CNB.), data=data)
  anova_total_hist_sum <- summary(anova_total_hist)
  p2 <- ggplot(data, aes(x = factor(Histology..CNB.), y = Total)) +
    geom_boxplot(fill = "#D55E00", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.4, color = "#D55E00") +
    labs(
      title = "Total vs Histology (CNB)",
      subtitle = paste0("ANOVA: F = ", round(anova_total_hist_sum[[1]]$"F value"[1], 2), ", p = ", format.pval(anova_total_hist_sum[[1]]$"Pr(>F)"[1], digits = 5)),
      x = "Histology (CNB)", y = "Total"
    ) + my_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
} else { p2 <- NULL }

# 3. ANOVA: Follow-Up vs Histology (CNB)
if("Histology..CNB." %in% names(data)) {
  anova_fu_hist <- aov(Follow.Up ~ factor(Histology..CNB.), data=data)
  anova_fu_hist_sum <- summary(anova_fu_hist)
  p3 <- ggplot(data, aes(x = factor(Histology..CNB.), y = Follow.Up)) +
    geom_boxplot(fill = "#009E73", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.4, color = "#009E73") +
    labs(
      title = "Follow-Up vs Histology (CNB)",
      subtitle = paste0("ANOVA: F = ", round(anova_fu_hist_sum[[1]]$"F value"[1], 2), ", p = ", format.pval(anova_fu_hist_sum[[1]]$"Pr(>F)"[1], digits = 5)),
      x = "Histology (CNB)", y = "Follow-Up"
    ) + my_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
} else { p3 <- NULL }

# 4. T-Test: Total vs Previous surgery
if("Previous.surgery" %in% names(data)) {
  ttest_total_prev <- t.test(Total ~ Previous.surgery, data=data)
  p4 <- ggplot(data, aes(x = factor(Previous.surgery), y = Total)) +
    geom_boxplot(fill = "#CC79A7", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.4, color = "#CC79A7") +
    labs(
      title = "Total vs Previous surgery",
      subtitle = paste0("T-Test: t = ", round(ttest_total_prev$statistic, 2), ", p = ", format.pval(ttest_total_prev$p.value, digits = 5)),
      x = "Previous surgery", y = "Total"
    ) + my_theme
} else { p4 <- NULL }

# 5. T-Test: Follow-Up vs Previous surgery
if("Previous.surgery" %in% names(data)) {
  ttest_fu_prev <- t.test(Follow.Up ~ Previous.surgery, data=data)
  p5 <- ggplot(data, aes(x = factor(Previous.surgery), y = Follow.Up)) +
    geom_boxplot(fill = "#F0E442", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.4, color = "#F0E442") +
    labs(
      title = "Follow-Up vs Previous surgery",
      subtitle = paste0("T-Test: t = ", round(ttest_fu_prev$statistic, 2), ", p = ", format.pval(ttest_fu_prev$p.value, digits = 5)),
      x = "Previous surgery", y = "Follow-Up"
    ) + my_theme
} else { p5 <- NULL }

# 6. T-Test: Total vs Hypertension
if("Hypertension" %in% names(data)) {
  ttest_total_hyp <- t.test(Total ~ Hypertension, data=data)
  p6 <- ggplot(data, aes(x = factor(Hypertension), y = Total)) +
    geom_boxplot(fill = "#56B4E9", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.4, color = "#56B4E9") +
    labs(
      title = "Total vs Hypertension",
      subtitle = paste0("T-Test: t = ", round(ttest_total_hyp$statistic, 2), ", p = ", format.pval(ttest_total_hyp$p.value, digits = 5)),
      x = "Hypertension", y = "Total"
    ) + my_theme
} else { p6 <- NULL }

# 7. T-Test: Total vs Diabetes
if("Diabetes" %in% names(data)) {
  ttest_total_diab <- t.test(Total ~ Diabetes, data=data)
  p7 <- ggplot(data, aes(x = factor(Diabetes), y = Total)) +
    geom_boxplot(fill = "#E69F00", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.4, color = "#E69F00") +
    labs(
      title = "Total vs Diabetes",
      subtitle = paste0("T-Test: t = ", round(ttest_total_diab$statistic, 2), ", p = ", format.pval(ttest_total_diab$p.value, digits = 5)),
      x = "Diabetes", y = "Total"
    ) + my_theme
} else { p7 <- NULL }

# Combine all 7 plots in a 3x3 grid (center empty)
combined <- (
  (p1 | p2 | p3) /
  (p4 | p5 | p6) /
  (p7 | plot_spacer() | plot_spacer())
) +
  plot_annotation(title = "Inflammation Theory",
                  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))

ggsave("Karl/inflammation_theory_combined.png", combined, width = 18, height = 16, dpi = 300) 