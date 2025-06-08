# Load required libraries
library(openxlsx)

# Prepare the data for the table
therapeutic_data <- data.frame(
  Name = c(
    "Duration", "Size", "Mastectomy", "Axilla", "CTX", "Axilla 24h", "Mamma 24h", "Drainage (A)", "Drainage (M)", "Opiod", "Length of stay"
  ),
  Descriptive_name = c(
    "Surgery Duration", "Implant Size", "Mastectomy Procedure", "Axillary Treatment", "Chemotherapy", "Seroma in Axilla in the first 24 hours", "Seroma in Mamma in the first 24 hours", "Drainage Duration in Axilla", "Drainage Duration in Mamma", "Number of Pyritramid injections (7.5 microgram)", "Length of stay"
  ),
  Min = c(43, 125, 1, 1, NA, 0, 10, 2, 2, 0, 3),
  Max = c(381, 600, 2, 3, NA, 320, 330, 15, 33, 3, 33),
  Mean_SD = c(
    "173.84 ± 70.24", "313.51 ± 99.70", "1.48 ± 0.50", "1.89 ± 0.83", NA, "51.66 ± 47.73", "94.17 ± 49.21", "5.85 ± 2.73", "8.22 ± 2.72", NA, "9.03 ± 2.59"
  ),
  Median = c(163, 300, 1, 2, NA, 40, 90, 5, 8, 0, 9),
  Std_Dev = c(70.24, 99.70, 0.50, 0.83, NA, 47.73, 49.21, 2.73, 2.72, NA, 2.59),
  Range = c(338, 475, 1, 2, NA, 320, 320, 13, 31, 3, 30),
  OR_95CI = c(NA, NA, "-37.9 to 188.9", NA, NA, NA, NA, NA, NA, NA, NA),
  P_value = c("0.0226", "0.00098", "0.191", "0.181", NA, "4.28e-12", "<2e-16", NA, NA, NA, "<2e-16"),
  stringsAsFactors = FALSE
)

# Save as Excel file
table_file <- "therapeutic_descriptive_table.xlsx"
write.xlsx(therapeutic_data, file = table_file, rowNames = FALSE)
cat("Therapeutic descriptive table saved as:", table_file, "\n") 