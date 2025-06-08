# Load required libraries
library(openxlsx)
library(gridExtra)
library(grid)
library(ggplot2)

# 1. Prepare data frames for each group
# Epidemiological Description
epi <- data.frame(
  Variables = c("Age", "Sex", "BMI"),
  N = c(301, 301, 301),
  Min = c(18, 1, 17.18),
  Max = c(88, 2, 55.36),
  Mean_SD = c("52.80 ± 18.52", "1 (Kadın çoğunluk)", "26.19 ± 5.72"),
  Median = c(51, 1, 25.35),
  Std_Dev = c(18.52, NA, 5.72),
  Range = c(70, 1, 38.18),
  OR_95CI = c(NA, NA, NA),
  P_value = c(NA, NA, NA),
  stringsAsFactors = FALSE
)

# Clinical Descriptive
clin <- data.frame(
  Name = c("Type", "Grade (CNB)", "Histology (CNB)", "Weight", "Hypertension", "Smoker", "Previous surgery", "Diabetes", "Grade (P)", "Type (P)", "Histology (P)", "BRCA", "pT", "pN", "N(X/x)", "N(x/X)", "M", "L", "V", "Pn", "CRP", "Temp"),
  N = c(NA, 301, 301, 301, 301, NA, 301, 301, NA, NA, NA, 140, 300, 214, 212, 212, NA, 281, NA, NA, 295, NA),
  Min = c(NA, 1, 1, 41, 0, NA, 0, 0, NA, NA, NA, 0, 0, 0, 0, 1, NA, 0, NA, NA, 0.77, 35.6),
  Max = c(NA, 5, 7, 5020, 1, NA, 1, 1, NA, NA, NA, 2, 4, 3, 12, 29, NA, 2, NA, NA, 89.22, 39.2),
  Mean_SD = c(NA, NA, "2.78 ± 2.16", "477.57 ± 415.73", "0.33 ± 0.47", NA, "0.17 ± 0.38", "0.11 ± 0.31", NA, NA, NA, "0.72 ± 0.74", "1.12 ± 1.09", "0.54 ± 0.80", "1.43 ± 2.78", "7.28 ± 6.20", NA, "0.41 ± 0.50", NA, NA, "17.30 ± 15.02", NA),
  Median = c(NA, NA, 2, 400, 0, NA, 0, 0, NA, NA, NA, 1, 1, 0, 0, 4.5, NA, 0, NA, NA, 13.26, NA),
  Std_Dev = c(NA, NA, 2.16, 415.73, 0.47, NA, 0.38, 0.31, NA, NA, NA, 0.74, 1.09, 0.80, 2.78, 6.20, NA, 0.50, NA, NA, 15.02, NA),
  Range = c(NA, 4, 6, 4979, 1, NA, 1, 1, NA, NA, NA, 2, 4, 3, 12, 28, NA, 2, NA, NA, 88.45, 3.6),
  OR_95CI = rep(NA, 22),
  P_value = rep(NA, 22),
  stringsAsFactors = FALSE
)

# Therapeutic Descriptive
ther <- data.frame(
  Name = c("Duration", "Size", "Mastectomy", "Axilla", "CTX", "Axilla 24h", "Mamma 24h", "Drainage (A)", "Drainage (M)", "Opiod", "Length of stay"),
  N = c(301, 144, 301, 301, NA, 207, 301, 207, 301, 301, 301),
  Min = c(43, 125, 1, 1, NA, 0, 10, 2, 2, 0, 3),
  Max = c(381, 600, 2, 3, NA, 320, 330, 15, 33, 3, 33),
  Mean_SD = c("173.84 ± 70.24", "313.51 ± 99.70", "1.48 ± 0.50", "1.89 ± 0.83", NA, "51.66 ± 47.73", "94.17 ± 49.21", "5.85 ± 2.73", "8.22 ± 2.72", NA, "9.03 ± 2.59"),
  Median = c(163, 300, 1, 2, NA, 40, 90, 5, 8, 0, 9),
  Std_Dev = c(70.24, 99.70, 0.50, 0.83, NA, 47.73, 49.21, 2.73, 2.72, NA, 2.59),
  Range = c(338, 475, 1, 2, NA, 320, 320, 13, 31, 3, 30),
  OR_95CI = rep(NA, 11),
  P_value = rep(NA, 11),
  stringsAsFactors = FALSE
)

# Seroma Descriptive
sero <- data.frame(
  Name = c("Follow-Up", "Total (A)", "Total (M)", "Total"),
  N = c(291, 301, 301, 301),
  Min = c(0, 0, 30, 35),
  Max = c(2, 1590, 2895, 3625),
  Mean_SD = c("0.80 ± 0.85", "187.57 ± 283.21", "518.35 ± 351.70", "705.92 ± 506.50"),
  Median = c(1, 55, 455, 585),
  Std_Dev = c(0.85, 283.21, 351.70, 506.50),
  Range = c(2, 1590, 2865, 3590),
  OR_95CI = rep(NA, 4),
  P_value = rep(NA, 4),
  stringsAsFactors = FALSE
)

# 2. Write to Excel (each group as a sheet)
wb <- createWorkbook()
addWorksheet(wb, "Epidemiological")
addWorksheet(wb, "Clinical")
addWorksheet(wb, "Therapeutic")
addWorksheet(wb, "Seroma")
writeData(wb, "Epidemiological", epi)
writeData(wb, "Clinical", clin)
writeData(wb, "Therapeutic", ther)
writeData(wb, "Seroma", sero)
saveWorkbook(wb, "final_descriptive_tables.xlsx", overwrite = TRUE)

# 3. Write to single-page PDF (all tables stacked vertically)
table_with_title <- function(df, title) {
  title_grob <- textGrob(title, gp = gpar(fontsize = 20, fontface = "bold"), just = "center")
  arrangeGrob(title_grob, tableGrob(df, rows = NULL, theme = ttheme_minimal(base_size = 12, core = list(fg_params = list(hjust = 0.5)))), ncol = 1, heights = unit.c(unit(2, "cm"), unit(1, "null")))
}

group_spacer <- function() {
  rectGrob(gp = gpar(col = NA, fill = NA), height = unit(1.2, "cm"))
}

tables <- list(
  table_with_title(epi, "Epidemiological Descriptive Table"),
  group_spacer(),
  table_with_title(clin, "Clinical Descriptive Table"),
  group_spacer(),
  table_with_title(ther, "Therapeutic Descriptive Table"),
  group_spacer(),
  table_with_title(sero, "Seroma Descriptive Table")
)

pdf("final_descriptive_tables.pdf", width = 12, height = 26)
grid.arrange(grobs = tables, ncol = 1, heights = NULL)
dev.off()
cat("Excel and PDF descriptive tables saved.\n") 