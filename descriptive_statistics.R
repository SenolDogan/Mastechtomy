# Load required libraries
library(readr)
library(openxlsx)

# List of variables
vars <- c("Age", "Axilla", "Axilla.24h", "BMI", "BRCA", "CRP", "Diabetes", "Drainage..A.", "Drainage...M.", "Duration", "Follow.Up", "Histology..CNB.", "Hypertension", "L", "Lenght.of.stay", "Mamma.24h", "Mastectomy", "N.X.x.", "N..x.X.", "pN", "Previous.surgery", "pT", "Size", "Total", "Total..A.", "Total..M.", "Weight")

# Read the data
data <- read.csv2("Test.csv", stringsAsFactors = FALSE)
colnames(data) <- trimws(colnames(data))

# Prepare results list
results <- list()

for (v in vars) {
  if (v %in% names(data)) {
    x <- data[[v]]
    # Try to convert to numeric if possible
    x_num <- suppressWarnings(as.numeric(gsub(",", ".", x)))
    if (all(is.na(x_num))) {
      # Categorical: show counts for each level
      tab <- table(x, useNA = "ifany")
      res <- data.frame(
        Variable = v,
        Type = "Categorical",
        Level = names(tab),
        Count = as.integer(tab),
        stringsAsFactors = FALSE
      )
    } else {
      # Numeric: show descriptive stats
      res <- data.frame(
        Variable = v,
        Type = "Numeric",
        Mean = mean(x_num, na.rm=TRUE),
        SD = sd(x_num, na.rm=TRUE),
        Min = min(x_num, na.rm=TRUE),
        Q1 = quantile(x_num, 0.25, na.rm=TRUE),
        Median = median(x_num, na.rm=TRUE),
        Q3 = quantile(x_num, 0.75, na.rm=TRUE),
        Max = max(x_num, na.rm=TRUE),
        IQR = IQR(x_num, na.rm=TRUE),
        N = sum(!is.na(x_num)),
        N_missing = sum(is.na(x_num)),
        stringsAsFactors = FALSE
      )
    }
    results[[v]] <- res
  }
}

# Combine all results
desc_stats <- do.call(rbind, results)

# Write to Excel
write.xlsx(desc_stats, file = "descriptive_statistics.xlsx", rowNames = FALSE) 