# Load required libraries
library(ggplot2)
library(patchwork)

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
data$Weight <- clean_numeric(data$Weight)
data$Size <- clean_numeric(data$Size)
data$Total..A. <- clean_numeric(data$Total..A.)
data$Total..M. <- clean_numeric(data$Total..M.)

# 1. Spearman: Total vs Weight
complete1 <- complete.cases(data$Total, data$Weight)
spearman_weight <- cor.test(data$Total[complete1], data$Weight[complete1], method = "spearman")
p1 <- ggplot(data[complete1,], aes(x = Weight, y = Total)) +
  geom_point(color = "#0072B2", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Total vs Weight",
    subtitle = paste0("Spearman: rho = ", round(spearman_weight$estimate, 2), ", p = ", format.pval(spearman_weight$p.value, digits = 5)),
    x = "Weight", y = "Total"
  ) + theme_minimal()

# 2. ANOVA: Total vs Axilla
if("Axilla" %in% names(data)) {
  data$Axilla <- as.factor(data$Axilla)
  anova_axilla <- aov(Total ~ Axilla, data=data)
  anova_axilla_sum <- summary(anova_axilla)
  p2 <- ggplot(data, aes(x = Axilla, y = Total)) +
    geom_boxplot(fill = "#D55E00", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.4, color = "#D55E00") +
    labs(
      title = "Total vs Axilla",
      subtitle = paste0("ANOVA: F = ", round(anova_axilla_sum[[1]]$"F value"[1], 2), ", p = ", format.pval(anova_axilla_sum[[1]]$"Pr(>F)"[1], digits = 5)),
      x = "Axilla", y = "Total"
    ) + theme_minimal()
} else { p2 <- NULL }

# 3. Spearman: Total vs Size
complete3 <- complete.cases(data$Total, data$Size)
spearman_size <- cor.test(data$Total[complete3], data$Size[complete3], method = "spearman")
p3 <- ggplot(data[complete3,], aes(x = Size, y = Total)) +
  geom_point(color = "#009E73", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Total vs Size",
    subtitle = paste0("Spearman: rho = ", round(spearman_size$estimate, 2), ", p = ", format.pval(spearman_size$p.value, digits = 5)),
    x = "Size", y = "Total"
  ) + theme_minimal()

# 4. T-Test: Total (A) vs Mastectomy
if("Mastectomy" %in% names(data)) {
  data$Mastectomy <- as.factor(data$Mastectomy)
  ttest_totalA_mast <- t.test(Total..A. ~ Mastectomy, data=data)
  p4 <- ggplot(data, aes(x = Mastectomy, y = Total..A.)) +
    geom_boxplot(fill = "#CC79A7", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.4, color = "#CC79A7") +
    labs(
      title = "Total (A) vs Mastectomy",
      subtitle = paste0("T-Test: t = ", round(ttest_totalA_mast$statistic, 2), ", p = ", format.pval(ttest_totalA_mast$p.value, digits = 5)),
      x = "Mastectomy", y = "Total (A)"
    ) + theme_minimal()
} else { p4 <- NULL }

# 5. Spearman: Total (M) vs Size
complete5 <- complete.cases(data$Total..M., data$Size)
spearman_totalM_size <- cor.test(data$Total..M.[complete5], data$Size[complete5], method = "spearman")
p5 <- ggplot(data[complete5,], aes(x = Size, y = Total..M.)) +
  geom_point(color = "#E69F00", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Total (M) vs Size",
    subtitle = paste0("Spearman: rho = ", round(spearman_totalM_size$estimate, 2), ", p = ", format.pval(spearman_totalM_size$p.value, digits = 5)),
    x = "Size", y = "Total (M)"
  ) + theme_minimal()

# Combine all 5 plots in a 3x2 grid (last cell empty)
combined <- (
  (p1 | p2 | p3) /
  (p4 | p5 | plot_spacer())
) +
  plot_annotation(title = "Dead Space",
                  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))

ggsave("Karl/dead_space_combined.png", combined, width = 18, height = 12, dpi = 300) 