# Load required libraries
library(readr)

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

# 1. Spearman: Total vs N(X/x)
complete1 <- complete.cases(data$Total, data$`N.X.x.`)
s1 <- cor.test(data$Total[complete1], data$`N.X.x.`[complete1], method = "spearman")
cat("Total vs N(X/x)\nSpearman rho:", s1$estimate, "\np-value:", s1$p.value, "\n\n")

# 2. Spearman: Total (A) vs N(X/x)
complete2 <- complete.cases(data$`Total..A.`, data$`N.X.x.`)
s2 <- cor.test(data$`Total..A.`[complete2], data$`N.X.x.`[complete2], method = "spearman")
cat("Total (A) vs N(X/x)\nSpearman rho:", s2$estimate, "\np-value:", s2$p.value, "\n\n")

# 3. Spearman: Total vs N(x/X)
complete3 <- complete.cases(data$Total, data$`N..x.X.`)
s3 <- cor.test(data$Total[complete3], data$`N..x.X.`[complete3], method = "spearman")
cat("Total vs N(x/X)\nSpearman rho:", s3$estimate, "\np-value:", s3$p.value, "\n\n")

# 4. Spearman: Total (A) vs N(x/X)
complete4 <- complete.cases(data$`Total..A.`, data$`N..x.X.`)
s4 <- cor.test(data$`Total..A.`[complete4], data$`N..x.X.`[complete4], method = "spearman")
cat("Total (A) vs N(x/X)\nSpearman rho:", s4$estimate, "\np-value:", s4$p.value, "\n\n")

# 5. Spearman: Total vs pN
complete5 <- complete.cases(data$Total, data$pN)
s5 <- cor.test(data$Total[complete5], data$pN[complete5], method = "spearman")
cat("Total vs pN\nSpearman rho:", s5$estimate, "\np-value:", s5$p.value, "\n\n")

# 6. Spearman: Total (A) vs pN
complete6 <- complete.cases(data$`Total..A.`, data$pN)
s6 <- cor.test(data$`Total..A.`[complete6], data$pN[complete6], method = "spearman")
cat("Total (A) vs pN\nSpearman rho:", s6$estimate, "\np-value:", s6$p.value, "\n\n")

# 7. Spearman: Total vs L
complete7 <- complete.cases(data$Total, data$L)
s7 <- cor.test(data$Total[complete7], data$L[complete7], method = "spearman")
cat("Total vs L\nSpearman rho:", s7$estimate, "\np-value:", s7$p.value, "\n\n")

# 8. Spearman: Total (A) vs L
complete8 <- complete.cases(data$`Total..A.`, data$L)
s8 <- cor.test(data$`Total..A.`[complete8], data$L[complete8], method = "spearman")
cat("Total (A) vs L\nSpearman rho:", s8$estimate, "\np-value:", s8$p.value, "\n\n") 