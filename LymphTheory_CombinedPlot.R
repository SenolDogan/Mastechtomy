# Load required libraries
library(ggplot2)
library(patchwork)

# Read the data
data <- read.csv2("Test.csv", stringsAsFactors = FALSE)
colnames(data) <- trimws(colnames(data))

# Clean numeric columns
data$Total <- as.numeric(gsub(",", ".", data$Total))
data$`Total..A.` <- as.numeric(gsub(",", ".", data$`Total..A.`))
data$`N.X.x.` <- as.numeric(gsub(",", ".", data$`N.X.x.`))
data$`N..x.X.` <- as.numeric(gsub(",", ".", data$`N..x.X.`))
data$pN <- as.numeric(gsub(",", ".", data$pN))
data$L <- as.numeric(gsub(",", ".", data$L))

# 1. Total vs N(X/x)
complete1 <- complete.cases(data$Total, data$`N.X.x.`)
s1 <- cor.test(data$Total[complete1], data$`N.X.x.`[complete1], method = "spearman")
p1 <- ggplot(data[complete1,], aes(x = `N.X.x.`, y = Total)) +
  geom_point(color = "#0072B2", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total vs N(X/x)",
       subtitle = paste0("Spearman: rho = ", round(s1$estimate, 2), ", p = ", format.pval(s1$p.value, digits = 5)),
       x = "N(X/x)", y = "Total") + theme_minimal()

# 2. Total (A) vs N(X/x)
complete2 <- complete.cases(data$`Total..A.`, data$`N.X.x.`)
s2 <- cor.test(data$`Total..A.`[complete2], data$`N.X.x.`[complete2], method = "spearman")
p2 <- ggplot(data[complete2,], aes(x = `N.X.x.`, y = `Total..A.`)) +
  geom_point(color = "#D55E00", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total (A) vs N(X/x)",
       subtitle = paste0("Spearman: rho = ", round(s2$estimate, 2), ", p = ", format.pval(s2$p.value, digits = 5)),
       x = "N(X/x)", y = "Total (A)") + theme_minimal()

# 3. Total vs N(x/X)
complete3 <- complete.cases(data$Total, data$`N..x.X.`)
s3 <- cor.test(data$Total[complete3], data$`N..x.X.`[complete3], method = "spearman")
p3 <- ggplot(data[complete3,], aes(x = `N..x.X.`, y = Total)) +
  geom_point(color = "#009E73", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total vs N(x/X)",
       subtitle = paste0("Spearman: rho = ", round(s3$estimate, 2), ", p = ", format.pval(s3$p.value, digits = 5)),
       x = "N(x/X)", y = "Total") + theme_minimal()

# 4. Total (A) vs N(x/X)
complete4 <- complete.cases(data$`Total..A.`, data$`N..x.X.`)
s4 <- cor.test(data$`Total..A.`[complete4], data$`N..x.X.`[complete4], method = "spearman")
p4 <- ggplot(data[complete4,], aes(x = `N..x.X.`, y = `Total..A.`)) +
  geom_point(color = "#CC79A7", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total (A) vs N(x/X)",
       subtitle = paste0("Spearman: rho = ", round(s4$estimate, 2), ", p = ", format.pval(s4$p.value, digits = 5)),
       x = "N(x/X)", y = "Total (A)") + theme_minimal()

# 5. Total vs pN
complete5 <- complete.cases(data$Total, data$pN)
s5 <- cor.test(data$Total[complete5], data$pN[complete5], method = "spearman")
p5 <- ggplot(data[complete5,], aes(x = pN, y = Total)) +
  geom_point(color = "#E69F00", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total vs pN",
       subtitle = paste0("Spearman: rho = ", round(s5$estimate, 2), ", p = ", format.pval(s5$p.value, digits = 5)),
       x = "pN", y = "Total") + theme_minimal()

# 6. Total (A) vs pN
complete6 <- complete.cases(data$`Total..A.`, data$pN)
s6 <- cor.test(data$`Total..A.`[complete6], data$pN[complete6], method = "spearman")
p6 <- ggplot(data[complete6,], aes(x = pN, y = `Total..A.`)) +
  geom_point(color = "#F0E442", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total (A) vs pN",
       subtitle = paste0("Spearman: rho = ", round(s6$estimate, 2), ", p = ", format.pval(s6$p.value, digits = 5)),
       x = "pN", y = "Total (A)") + theme_minimal()

# 7. Total vs L
complete7 <- complete.cases(data$Total, data$L)
s7 <- cor.test(data$Total[complete7], data$L[complete7], method = "spearman")
p7 <- ggplot(data[complete7,], aes(x = L, y = Total)) +
  geom_point(color = "#56B4E9", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total vs L",
       subtitle = paste0("Spearman: rho = ", round(s7$estimate, 2), ", p = ", format.pval(s7$p.value, digits = 5)),
       x = "L", y = "Total") + theme_minimal()

# 8. Total (A) vs L
complete8 <- complete.cases(data$`Total..A.`, data$L)
s8 <- cor.test(data$`Total..A.`[complete8], data$L[complete8], method = "spearman")
p8 <- ggplot(data[complete8,], aes(x = L, y = `Total..A.`)) +
  geom_point(color = "#000000", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total (A) vs L",
       subtitle = paste0("Spearman: rho = ", round(s8$estimate, 2), ", p = ", format.pval(s8$p.value, digits = 5)),
       x = "L", y = "Total (A)") + theme_minimal()

# Combine all 8 plots in a 4x2 grid
grid <- (
  (p1 | p2) /
  (p3 | p4) /
  (p5 | p6) /
  (p7 | p8)
) +
  plot_annotation(title = "Lymph Theory",
                  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))

ggsave("Karl/lymph_theory_combined.png", grid, width = 18, height = 20, dpi = 300) 