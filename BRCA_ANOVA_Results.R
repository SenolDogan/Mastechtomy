# Load required libraries
#
library(readr)

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
cat("Total (M) vs BRCA\nF-statistic:", sum_M[[1]]$"F value"[1], "\np-value:", sum_M[[1]]$"Pr(>F)"[1], "\n\n")

# 2. ANOVA: Total (A) vs BRCA
anova_A <- aov(`Total..A.` ~ BRCA, data=data)
sum_A <- summary(anova_A)
cat("Total (A) vs BRCA\nF-statistic:", sum_A[[1]]$"F value"[1], "\np-value:", sum_A[[1]]$"Pr(>F)"[1], "\n\n")

# 3. ANOVA: Total vs BRCA
anova_T <- aov(Total ~ BRCA, data=data)
sum_T <- summary(anova_T)
cat("Total vs BRCA\nF-statistic:", sum_T[[1]]$"F value"[1], "\np-value:", sum_T[[1]]$"Pr(>F)"[1], "\n\n") 