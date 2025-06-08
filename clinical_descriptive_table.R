# Load required libraries
library(openxlsx)

# Prepare the data for the table
clinical_data <- data.frame(
  Name = c(
    "Type", "Grade (CNB)", "Histology (CNB)", "Weight", "Hypertension", "Smoker", "Previous surgery", "Diabetes", "Grade (P)", "Type (P)", "Histology (P)", "BRCA", "pT", "pN", "N(X/x)", "N(x/X)", "M", "L", "V", "Pn", "CRP", "Temp"
  ),
  Descriptive_name = c(
    "Tumor Subtype", "", "", "Breast weight", "", "", "", "", "", "", "", "", "", "", "Metastatic Lymph Nodes", "Removed Lymph Nodes", "pM", "pL", "pV", "pPn", "CRP postoperative", "Temperature postoperative"
  ),
  Min = c(NA, 1, 1, 41, 0, NA, 0, 0, NA, NA, NA, 0, 0, 0, 0, 1, NA, 0, NA, NA, 0.77, 35.6),
  Max = c(NA, 5, 7, 5020, 1, NA, 1, 1, NA, NA, NA, 2, 4, 3, 12, 29, NA, 2, NA, NA, 89.22, 38.5),
  Mean_SD = c(
    NA, NA, "2.78 ± 2.16", "477.57 ± 415.73", "0.33 ± 0.47", NA, "0.17 ± 0.38", "0.11 ± 0.31", NA, NA, NA, "0.72 ± 0.74", "1.12 ± 1.09", "0.54 ± 0.80", "1.43 ± 2.78", "7.28 ± 6.20", NA, "0.41 ± 0.50", NA, NA, "17.30 ± 15.02", "36.87 ± 0.54"
  ),
  Median = c(NA, 2, 2, 400, 0, NA, 0, 0, NA, NA, NA, 1, 1, 0, 0, 4.5, NA, 0, NA, NA, 13.26, 36.8),
  Std_Dev = c(NA, NA, 2.16, 415.73, 0.47, NA, 0.38, 0.31, NA, NA, NA, 0.74, 1.09, 0.80, 2.78, 6.20, NA, 0.50, NA, NA, 15.02, 0.54),
  Range = c(NA, 4, 6, 4979, 1, NA, 1, 1, NA, NA, NA, 2, 4, 3, 12, 28, NA, 2, NA, NA, 88.45, 2.9),
  OR_95CI = c(
    NA, NA, NA, NA, "-396.2 to -111.3", NA, "18.7 to 310.2", "-534.4 to -19.6", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
  ),
  P_value = c(
    NA, NA, "0.00143", "2.94e-08", "0.00059", NA, "0.0275", "0.0357", NA, NA, NA, "0.339", "5.54e-09", "4.47e-10", "1.67e-10", NA, NA, NA, NA, NA, "3.23e-05", NA
  ),
  stringsAsFactors = FALSE
)

# Save as Excel file
table_file <- "clinical_descriptive_table.xlsx"
write.xlsx(clinical_data, file = table_file, rowNames = FALSE)
cat("Clinical descriptive table saved as:", table_file, "\n") 