# Load required libraries
library(openxlsx)

# Prepare the data for the table
# Values from descriptive_statistics.csv and Statistical_Results_Report.txt

table_data <- data.frame(
  Variable = c("Age", "Sex", "BMI"),
  N = c(301, 301, 301),
  Min = c(18, 1, 17.18),
  Max = c(88, 2, 55.36),
  Mean_SD = c("52.80 ± 18.52", "1 (Kadın çoğunluk)", "26.19 ± 5.72"),
  Median = c(51, 1, 25.35),
  Std_Dev = c(18.52, NA, 5.72),
  Range = c(70, 1, 38.18),
  OR_95CI = c(NA, NA, NA),
  P_value = c("0.00073", NA, "3.23e-05"),
  stringsAsFactors = FALSE
)

# Save as Excel file
table_file <- "epidemiological_description_table.xlsx"
write.xlsx(table_data, file = table_file, rowNames = FALSE)
cat("Epidemiological description table saved as:", table_file, "\n") 