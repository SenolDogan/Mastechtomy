# Load required libraries
library(ggplot2)
library(patchwork)

# Read the data
data <- read.csv2("Test.csv", stringsAsFactors = FALSE)
colnames(data) <- trimws(colnames(data))

# Clean numeric columns
data$Total <- as.numeric(gsub(",", ".", data$Total))
data$`Total..M.` <- as.numeric(gsub(",", ".", data$`Total..M.`))
data$Follow.Up <- as.numeric(gsub(",", ".", data$Follow.Up))
data$pT <- as.numeric(gsub(",", ".", data$pT))
data$Duration <- as.numeric(gsub(",", ".", data$Duration))

# 1. Spearman: Total vs pT
complete1 <- complete.cases(data$Total, data$pT)
s1 <- cor.test(data$Total[complete1], data$pT[complete1], method = "spearman")
p1 <- ggplot(data[complete1,], aes(x = pT, y = Total)) +
  geom_point(color = "#0072B2", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total vs pT",
       subtitle = paste0("Spearman: rho = ", round(s1$estimate, 2), ", p = ", format.pval(s1$p.value, digits = 5)),
       x = "pT", y = "Total") + theme_minimal()

# 2. Spearman: Total (M) vs Duration
complete2 <- complete.cases(data$`Total..M.`, data$Duration)
s2 <- cor.test(data$`Total..M.`[complete2], data$Duration[complete2], method = "spearman")
p2 <- ggplot(data[complete2,], aes(x = Duration, y = `Total..M.`)) +
  geom_point(color = "#D55E00", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Total (M) vs Duration",
       subtitle = paste0("Spearman: rho = ", round(s2$estimate, 2), ", p = ", format.pval(s2$p.value, digits = 5)),
       x = "Duration", y = "Total (M)") + theme_minimal()

# 3. Spearman: Follow-Up vs Duration
complete3 <- complete.cases(data$Follow.Up, data$Duration)
s3 <- cor.test(data$Follow.Up[complete3], data$Duration[complete3], method = "spearman")
p3 <- ggplot(data[complete3,], aes(x = Duration, y = Follow.Up)) +
  geom_point(color = "#009E73", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Follow-Up vs Duration",
       subtitle = paste0("Spearman: rho = ", round(s3$estimate, 2), ", p = ", format.pval(s3$p.value, digits = 5)),
       x = "Duration", y = "Follow-Up") + theme_minimal()

# Combine all 3 plots in a single row
combined <- (p1 | p2 | p3) +
  plot_annotation(title = "Clinical Characteristics",
                  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))

ggsave("Karl/clinical_characteristics_combined.png", combined, width = 18, height = 6, dpi = 300) 