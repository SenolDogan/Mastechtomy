# Read the CSV file
data <- read.csv2("Test.csv")

# Calculate Spearman correlation
result <- cor.test(data$Total, data$Lenght.of.stay, 
                  method = "spearman",
                  exact = FALSE)

# Print results
cat("\nSpearman Correlation: Length of stay vs Total")
cat("\n----------------------------------------")
cat("\nSpearman's rho:", round(result$estimate, 3))
cat("\np-value:", format.pval(result$p.value, digits = 3))
cat("\n----------------------------------------\n") 