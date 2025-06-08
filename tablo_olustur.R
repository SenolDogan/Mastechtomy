options(repos = c(CRAN = "https://cloud.r-project.org"))

# Gerekli paketler
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("broom")) install.packages("broom")
if (!require("epitools")) install.packages("epitools")
if (!require("writexl")) install.packages("writexl")

library(readr)
library(dplyr)
library(broom)
library(epitools)
library(writexl)

# Veriyi oku
df <- read_delim("Test.csv", delim = ";", locale = locale(decimal_mark = ",", grouping_mark = "."))

# Değişken listesi (örnek, ihtiyaca göre genişletilebilir)
variables <- c(
  "Age", "Sex", "BMI", "Type", "Grade (CNB)", "Histology (CNB)", "Weight", "Hypertension", "Smoker",
  "Previous surgery", "Diabetes", "Grade (P)", "Type (P)", "Histology (P)", "BRCA ", "pT", "pN",
  "N(x/x)" = "N(X/x)", "N(X/x)", "N(x/X)" = "N (x/X)", "M", "L", "V", "Pn", "CRP", "Temp",
  "Duration", "Size", "Mastectomy", "Axilla", "CTX", "Axilla 24h", "Mamma 24h", "Drainage (A)",
  "Drainage  (M)", "Opiod", "Lenght of stay", "Follow-Up", "Total (A)", "Total (M)", "Total"
)

# Sonuç tablosu için boş bir data.frame
results <- data.frame(
  Variable = character(),
  N = integer(),
  Min = numeric(),
  Max = numeric(),
  Mean_SD = character(),
  Median = numeric(),
  Std_Dev = numeric(),
  Range = character(),
  OR_95CI = character(),
  P_value = character(),
  Comparison = character(),
  stringsAsFactors = FALSE
)

# Mastectomy'yi faktör olarak ayarla
df$Mastectomy <- as.factor(df$Mastectomy)

for (var in variables) {
  if (!(var %in% names(df))) {
    results <- rbind(results, data.frame(
      Variable = var, N = NA, Min = NA, Max = NA, Mean_SD = NA, Median = NA, Std_Dev = NA, Range = NA, OR_95CI = NA, P_value = NA, Comparison = NA
    ))
    next
  }
  x <- df[[var]]
  Comparison <- ""
  # Sayısal mı kategorik mi?
  if (is.numeric(x) || is.integer(x)) {
    N <- sum(!is.na(x))
    Min <- min(x, na.rm = TRUE)
    Max <- max(x, na.rm = TRUE)
    Mean <- mean(x, na.rm = TRUE)
    SD <- sd(x, na.rm = TRUE)
    Median <- median(x, na.rm = TRUE)
    Range <- paste0(Min, "-", Max)
    Mean_SD <- sprintf("%.2f ± %.2f", Mean, SD)
    Std_Dev <- sprintf("%.3f", SD)
    # Mastectomy'ye göre karşılaştırma (t-testi veya Mann-Whitney)
    if (length(unique(na.omit(df$Mastectomy))) == 2) {
      g0 <- x[df$Mastectomy == levels(df$Mastectomy)[1]]
      g1 <- x[df$Mastectomy == levels(df$Mastectomy)[2]]
      if (length(g0) > 1 && length(g1) > 1) {
        pval <- tryCatch(
          t.test(g0, g1)$p.value,
          error = function(e) tryCatch(
            wilcox.test(g0, g1)$p.value,
            error = function(e) NA
          )
        )
        P_value <- sprintf("%.3g", pval)
        Comparison <- paste0("Mastectomy: ", levels(df$Mastectomy)[1], " vs ", levels(df$Mastectomy)[2])
      } else {
        P_value <- NA
      }
    } else {
      P_value <- NA
    }
    OR_95CI <- ""
  } else if (is.factor(x) || is.character(x) || length(unique(na.omit(x))) <= 10) {
    # Kategorik değişkenler için
    tab <- table(x, df$Mastectomy)
    N <- sum(!is.na(x))
    Min <- Max <- Mean_SD <- Median <- Std_Dev <- Range <- NA
    # Sadece 2x2 tablo için OR ve p-value
    if (all(dim(tab) == 2) && all(tab > 0)) {
      or_res <- oddsratio(tab)
      OR <- or_res$measure[2,1]
      CI_low <- or_res$measure[2,2]
      CI_high <- or_res$measure[2,3]
      OR_95CI <- sprintf("%.2f (%.2f-%.2f)", OR, CI_low, CI_high)
      P_value <- sprintf("%.3g", fisher.test(tab)$p.value)
      Comparison <- paste0("Mastectomy: ", colnames(tab)[1], " vs ", colnames(tab)[2])
    } else {
      OR_95CI <- ""
      P_value <- ""
      Comparison <- ""
    }
  } else {
    N <- Min <- Max <- Mean_SD <- Median <- Std_Dev <- Range <- OR_95CI <- P_value <- Comparison <- NA
  }
  results <- rbind(results, data.frame(
    Variable = var, N = N, Min = Min, Max = Max, Mean_SD = Mean_SD, Median = Median, Std_Dev = Std_Dev, Range = Range, OR_95CI = OR_95CI, P_value = P_value, Comparison = Comparison
  ))
}

# Seroma için boş satır ekle
results <- rbind(
  results,
  data.frame(Variable = "Seroma", N = NA, Min = NA, Max = NA, Mean_SD = NA, Median = NA, Std_Dev = NA, Range = NA, OR_95CI = NA, P_value = NA, Comparison = NA)
)

# --- Size için p-value ---
size_pvalue <- ""
if ("Size" %in% names(df) && length(unique(na.omit(df$Mastectomy))) == 2) {
  size <- df$Size
  g0 <- size[df$Mastectomy == levels(df$Mastectomy)[1]]
  g1 <- size[df$Mastectomy == levels(df$Mastectomy)[2]]
  if (length(g0) > 1 && length(g1) > 1) {
    size_pval <- tryCatch(
      t.test(g0, g1)$p.value,
      error = function(e) tryCatch(
        wilcox.test(g0, g1)$p.value,
        error = function(e) NA
      )
    )
    size_pvalue <- as.character(signif(size_pval, 3))
  }
}
# Tabloya Size'ın p-value'sini karakter olarak ekle
results$P_value[results$Variable == "Size"] <- as.character(size_pvalue)

# --- Mastectomy için istatistikler ---
mastectomy <- as.numeric(as.character(df$Mastectomy))
N <- sum(!is.na(mastectomy))
Min <- min(mastectomy, na.rm = TRUE)
Max <- max(mastectomy, na.rm = TRUE)
Mean <- mean(mastectomy, na.rm = TRUE)
SD <- sd(mastectomy, na.rm = TRUE)
Median <- median(mastectomy, na.rm = TRUE)
Range <- paste0(Min, "-", Max)
Mean_SD <- sprintf("%.2f ± %.2f", Mean, SD)
Std_Dev <- sprintf("%.3f", SD)
P_value <- NA
Comparison <- ""
if (length(unique(na.omit(mastectomy))) == 2) {
  g0 <- mastectomy[mastectomy == 0]
  g1 <- mastectomy[mastectomy == 1]
  if (length(g0) > 1 && length(g1) > 1) {
    pval <- tryCatch(
      t.test(g0, g1)$p.value,
      error = function(e) tryCatch(
        wilcox.test(g0, g1)$p.value,
        error = function(e) NA
      )
    )
    P_value <- sprintf("%.3g", pval)
    Comparison <- "Mastectomy: 0 vs 1"
  }
}
mastectomy_row <- data.frame(
  Variable = "Mastectomy (summary)",
  N = as.character(N),
  Min = as.character(Min),
  Max = as.character(Max),
  Mean_SD = as.character(Mean_SD),
  Median = as.character(Median),
  Std_Dev = as.character(Std_Dev),
  Range = as.character(Range),
  OR_95CI = "",
  P_value = as.character(P_value),
  Comparison = as.character(Comparison),
  stringsAsFactors = FALSE
)
mastectomy_row[] <- lapply(mastectomy_row, as.character)
results2 <- rbind(results, mastectomy_row)
results2[] <- lapply(results2, as.character)
write_xlsx(results2, "Tablo_Sonuclari_R.xlsx")
cat("Tablo başarıyla oluşturuldu ve Tablo_Sonuclari_R.xlsx olarak kaydedildi.\n") 