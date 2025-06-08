import pandas as pd
import numpy as np
from scipy.stats import chi2_contingency, fisher_exact, ttest_ind, mannwhitneyu
from statsmodels.stats.contingency_tables import Table2x2

# Dosyayı oku
# Not: encoding ve delimiter ayarları gerekirse değiştirilebilir
df = pd.read_csv("Test.csv", sep=";", encoding="latin1")

# Görseldeki tabloya göre başlıkları csv ile eşleştir
variables = [
    # Epidemiological
    ("Age", "Age"),
    ("Sex", "Sex"),
    ("BMI", "BMI"),
    # Clinical
    ("Type", "Type"),
    ("Grade (CNB)", "Grade (CNB)"),
    ("Histology (CNB)", "Histology (CNB)"),
    ("Weight", "Weight"),
    ("Hypertension", "Hypertension"),
    ("Smoker", "Smoker"),
    ("Previous surgery", "Previous surgery"),
    ("Diabetes", "Diabetes"),
    ("Grade (P)", "Grade (P)"),
    ("Type (P)", "Type (P)"),
    ("Histology (P)", "Histology (P)"),
    ("BRCA", "BRCA "),
    ("pT", "pT"),
    ("pN", "pN"),
    ("N(x/x)", "N(X/x)"),
    ("N(X/x)", "N(X/x)"),
    ("N(x/X)", "N (x/X)"),
    ("M", "M"),
    ("L", "L"),
    ("V", "V"),
    ("Pn", "Pn"),
    ("CRP", "CRP"),
    ("Temp", "Temp"),
    # Therapeutic
    ("Duration", "Duration"),
    ("Size", "Size"),
    ("Mastectomy", "Mastectomy"),
    ("Axilla", "Axilla"),
    ("CTX", "CTX"),
    ("Axilla 24h", "Axilla 24h"),
    ("Mamma 24h", "Mamma 24h"),
    ("Drainage (A)", "Drainage (A)"),
    ("Drainage (M)", "Drainage  (M)"),
    ("Opioid", "Opiod"),
    ("Length of stay", "Lenght of stay"),
    ("Seroma", None),
    # Seroma ve altı
    ("Follow-Up", "Follow-Up"),
    ("Total (A)", "Total (A)"),
    ("Total (M)", "Total (M)"),
    ("Total", "Total"),
]

results = []

# Mastectomy iki grup: 0 ve 1 (veya varsa diğer değerler)
mastectomy_col = "Mastectomy"
if mastectomy_col in df.columns:
    group0 = df[df[mastectomy_col] == 0]
    group1 = df[df[mastectomy_col] == 1]
else:
    group0 = group1 = pd.DataFrame()

for label, col in variables:
    if col and col in df.columns:
        data = pd.to_numeric(df[col], errors='coerce')
        N = data.count()
        Min = data.min()
        Max = data.max()
        Mean_SD = f"{data.mean():.2f} ± {data.std():.2f}" if N > 0 else ""
        Median = data.median()
        Std_Dev = data.std()
        Range = f"{Min}-{Max}" if N > 0 else ""
        OR_CI = ""
        P_value = ""
        # Kategorik mi sürekli mi?
        unique_vals = df[col].dropna().unique()
        if len(unique_vals) <= 10 and df[col].dropna().dtype in [np.int64, np.float64, int, float]:
            # Kategorik değişken (ör: 0/1)
            try:
                tab = pd.crosstab(df[col], df[mastectomy_col])
                if tab.shape == (2,2):
                    # OR ve %95 CI
                    table = Table2x2(tab.values)
                    OR = table.oddsratio
                    CI_low, CI_upp = table.oddsratio_confint()
                    OR_CI = f"{OR:.2f} ({CI_low:.2f}-{CI_upp:.2f})"
                    # Fisher exact veya ki-kare
                    _, p = fisher_exact(tab.values)
                    P_value = f"{p:.4f}"
                else:
                    P_value = "-"
            except Exception:
                P_value = "-"
        elif group0.shape[0] > 0 and group1.shape[0] > 0:
            # Sürekli değişkenlerde iki grup karşılaştırması
            g0 = pd.to_numeric(group0[col], errors='coerce').dropna()
            g1 = pd.to_numeric(group1[col], errors='coerce').dropna()
            if len(g0) > 0 and len(g1) > 0:
                # Normal dağılım varsayımı için t-testi, değilse Mann-Whitney U
                try:
                    stat, p = ttest_ind(g0, g1, nan_policy='omit')
                except Exception:
                    stat, p = mannwhitneyu(g0, g1, alternative='two-sided')
                P_value = f"{p:.4f}"
        results.append([label, N, Min, Max, Mean_SD, Median, Std_Dev, Range, OR_CI, P_value])
    else:
        results.append([label, "", "", "", "", "", "", "", "", ""])

columns = ["Variable", "N", "Min", "Max", "Mean_SD", "Median", "Std_Dev", "Range", "OR_95CI", "P_value"]
result_df = pd.DataFrame(results, columns=columns)
result_df.to_excel("Tablo_Sonuclari.xlsx", index=False)
print("Tablo başarıyla oluşturuldu ve Tablo_Sonuclari.xlsx olarak kaydedildi.") 