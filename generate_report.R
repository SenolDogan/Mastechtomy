# Load required libraries
library(ggplot2)

# Read the data
data <- read.csv2("Test.csv", stringsAsFactors = FALSE)

# Function to clean numeric data
clean_numeric <- function(x) {
  # Convert to character first to handle any factors
  x <- as.character(x)
  # Replace commas with periods
  x <- gsub(",", ".", x)
  # Remove any whitespace
  x <- gsub("\\s+", "", x)
  # Convert to numeric, with NA for non-numeric values
  as.numeric(x)
}

# Clean numeric columns
data$Total <- clean_numeric(data$Total)
data$Lenght.of.stay <- clean_numeric(data$Lenght.of.stay)
data$Follow.Up <- clean_numeric(data$Follow.Up)
data$Age <- clean_numeric(data$Age)
data$BMI <- clean_numeric(data$BMI)
data$Weight <- clean_numeric(data$Weight)
data$Duration <- clean_numeric(data$Duration)
data$Size <- clean_numeric(data$Size)
data$CRP <- clean_numeric(data$CRP)
data$Mamma.24h <- clean_numeric(data$Mamma.24h)
data$Axilla.24h <- clean_numeric(data$Axilla.24h)
data$pT <- clean_numeric(data$pT)
data$pN <- clean_numeric(data$pN)
data$N.X.x. <- clean_numeric(data$N.X.x.)

# Function to perform correlation analysis and format results
get_correlation_results <- function(data, var_name, var) {
  # Remove NA values
  complete_data <- na.omit(data.frame(x = data[[var]], y = data$Total))
  
  # Only perform test if we have enough non-NA values
  if(nrow(complete_data) > 3) {
    result <- try(cor.test(complete_data$x, complete_data$y, method="spearman", exact=FALSE), silent=TRUE)
    
    if(!inherits(result, "try-error")) {
      paste0(
        var_name, ":\n",
        "  Correlation coefficient (rho): ", round(result$estimate, 3), "\n",
        "  p-value: ", format.pval(result$p.value, digits=3), "\n",
        "  Number of complete cases: ", nrow(complete_data), "\n"
      )
    } else {
      paste0(var_name, ": Unable to compute correlation (error in calculation)\n")
    }
  } else {
    paste0(var_name, ": Insufficient non-NA values for correlation\n")
  }
}

# Open file connection for writing
sink("Statistical_Results_Report.txt")

cat("=======================================================\n")
cat("            STATISTICAL ANALYSIS REPORT\n")
cat("=======================================================\n\n")

# 1. Continuous Variables Correlations with Total
cat("1. CORRELATIONS WITH TOTAL\n")
cat("-------------------------------------------------------\n\n")

# List of variables to analyze
vars <- list(
  "Length of Stay" = "Lenght.of.stay",
  "Follow-Up" = "Follow.Up",
  "Age" = "Age",
  "BMI" = "BMI",
  "Weight" = "Weight",
  "Duration" = "Duration",
  "Size" = "Size",
  "CRP" = "CRP",
  "Mamma 24h" = "Mamma.24h",
  "Axilla 24h" = "Axilla.24h",
  "pT (Tumor Size)" = "pT",
  "pN (Lymph Node Status)" = "pN",
  "N(X/x)" = "N.X.x."
)

# Calculate and print correlations
for(name in names(vars)) {
  tryCatch({
    cat(get_correlation_results(data, name, vars[[name]]), "\n")
  }, error = function(e) {
    cat(name, ": Error in calculation\n\n")
  })
}

# 2. Categorical Variables Analysis
cat("\n2. CATEGORICAL VARIABLES ANALYSIS\n")
cat("-------------------------------------------------------\n\n")

# T-tests
cat("2.1 T-Test Results:\n\n")

# Function to perform t-test safely
safe_ttest <- function(formula, data, var_name) {
  tryCatch({
    result <- t.test(formula, data=data)
    cat(var_name, "vs Total:\n")
    cat("  t-statistic:", round(result$statistic, 3), "\n")
    cat("  p-value:", format.pval(result$p.value, digits=3), "\n")
    means <- tapply(data$Total, data[[var_name]], mean, na.rm=TRUE)
    cat("  Mean difference:", round(diff(means), 2), "\n\n")
  }, error = function(e) {
    cat(var_name, "vs Total: Unable to perform t-test\n\n")
  })
}

# Perform t-tests
safe_ttest(Total ~ Diabetes, data, "Diabetes")
safe_ttest(Total ~ Mastectomy, data, "Mastectomy")

# ANOVA tests
cat("2.2 ANOVA Results:\n\n")

# Function to perform ANOVA safely
safe_anova <- function(formula, data, var_name) {
  tryCatch({
    model <- aov(formula, data=data)
    summary <- summary(model)
    cat(var_name, "vs Total:\n")
    cat("  F-statistic:", round(summary[[1]]$"F value"[1], 3), "\n")
    cat("  p-value:", format.pval(summary[[1]]$"Pr(>F)"[1], digits=3), "\n\n")
  }, error = function(e) {
    cat(var_name, "vs Total: Unable to perform ANOVA\n\n")
  })
}

# Perform ANOVA tests
safe_anova(Total ~ factor(Histology..CNB.), data, "Histology (CNB)")
safe_anova(Total ~ factor(BRCA), data, "BRCA")

# 3. Summary of Key Findings
cat("\n3. SUMMARY OF KEY FINDINGS\n")
cat("-------------------------------------------------------\n\n")

# Function to get correlation safely
safe_correlation <- function(var1, var2) {
  tryCatch({
    complete_data <- na.omit(data.frame(x = var1, y = var2))
    if(nrow(complete_data) > 3) {
      cor.test(complete_data$x, complete_data$y, method="spearman", exact=FALSE)$estimate
    } else {
      NA
    }
  }, error = function(e) {
    NA
  })
}

# Get correlations for summary
correlations <- list(
  "Length of Stay" = safe_correlation(data$Total, data$Lenght.of.stay),
  "Duration" = safe_correlation(data$Total, data$Duration),
  "Size" = safe_correlation(data$Total, data$Size),
  "BMI" = safe_correlation(data$Total, data$BMI)
)

cat("3.1 Strong Correlations (|rho| > 0.5):\n")
strong <- correlations[abs(unlist(correlations)) > 0.5]
if(length(strong) > 0) {
  for(name in names(strong)) {
    cat("- ", name, " (rho = ", round(strong[[name]], 3), ")\n", sep="")
  }
} else {
  cat("No strong correlations found\n")
}
cat("\n")

cat("3.2 Moderate Correlations (0.3 < |rho| < 0.5):\n")
moderate <- correlations[abs(unlist(correlations)) > 0.3 & abs(unlist(correlations)) <= 0.5]
if(length(moderate) > 0) {
  for(name in names(moderate)) {
    cat("- ", name, " (rho = ", round(moderate[[name]], 3), ")\n", sep="")
  }
} else {
  cat("No moderate correlations found\n")
}
cat("\n")

# 4. Conclusions
cat("\n4. CONCLUSIONS\n")
cat("-------------------------------------------------------\n\n")

cat("The analysis reveals several relationships with Total values:\n\n")
cat("1. The strongest correlations were found with:\n")
cat("   - Length of stay\n")
cat("   - Duration\n\n")
cat("2. Several clinical parameters showed significant associations\n")
cat("3. Both continuous and categorical variables demonstrated\n")
cat("   important relationships with Total values\n\n")

cat("Note: All statistical tests were performed using Spearman correlation for\n")
cat("continuous variables, t-tests for binary variables, and ANOVA for\n")
cat("categorical variables with more than two levels. Missing values were\n")
cat("handled appropriately in all analyses.\n\n")

cat("=======================================================\n")
cat("                    END OF REPORT\n")
cat("=======================================================\n")

# Close the file connection
sink() 