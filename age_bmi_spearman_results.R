# Load required libraries
library(readr)

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
cat("Total vs Age\nSpearman rho:", s1$estimate, "\np-value:", s1$p.value, "\n\n")

# 2. Spearman: Follow-Up vs Age
complete2 <- complete.cases(data$Follow.Up, data$Age)
s2 <- cor.test(data$Follow.Up[complete2], data$Age[complete2], method = "spearman")
cat("Follow-Up vs Age\nSpearman rho:", s2$estimate, "\np-value:", s2$p.value, "\n\n")

# 3. Spearman: Total vs BMI
complete3 <- complete.cases(data$Total, data$BMI)
s3 <- cor.test(data$Total[complete3], data$BMI[complete3], method = "spearman")
cat("Total vs BMI\nSpearman rho:", s3$estimate, "\np-value:", s3$p.value, "\n\n")

# 4. Spearman: Follow-Up vs BMI
complete4 <- complete.cases(data$Follow.Up, data$BMI)
s4 <- cor.test(data$Follow.Up[complete4], data$BMI[complete4], method = "spearman")
cat("Follow-Up vs BMI\nSpearman rho:", s4$estimate, "\np-value:", s4$p.value, "\n\n") 