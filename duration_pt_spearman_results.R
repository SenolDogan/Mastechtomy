# Load required libraries
library(readr)

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
cat("Total vs pT\nSpearman rho:", s1$estimate, "\np-value:", s1$p.value, "\n\n")

# 2. Spearman: Total (M) vs Duration
complete2 <- complete.cases(data$`Total..M.`, data$Duration)
s2 <- cor.test(data$`Total..M.`[complete2], data$Duration[complete2], method = "spearman")
cat("Total (M) vs Duration\nSpearman rho:", s2$estimate, "\np-value:", s2$p.value, "\n\n")

# 3. Spearman: Follow-Up vs Duration
complete3 <- complete.cases(data$Follow.Up, data$Duration)
s3 <- cor.test(data$Follow.Up[complete3], data$Duration[complete3], method = "spearman")
cat("Follow-Up vs Duration\nSpearman rho:", s3$estimate, "\np-value:", s3$p.value, "\n\n") 