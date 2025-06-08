# Load required libraries
library(readr)
library(ggplot2)
library(patchwork)

# Read the CSV file
data <- read.csv2("Test.csv")

# Trim whitespace from column names
colnames(data) <- trimws(colnames(data))

# Convert variables to numeric if needed
num_vars <- c("Total", "Mamma.24h", "Axilla.24h", "Drainage..A.", "Drainage...M.")
for (v in num_vars) {
  if (!is.numeric(data[[v]])) {
    data[[v]] <- as.numeric(as.character(data[[v]]))
  }
}

# List of variable names and pretty labels
vars <- list(
  "Mamma 24h" = "Mamma.24h",
  "Axilla 24h" = "Axilla.24h",
  "Drainage (A)" = "Drainage..A.",
  "Drainage (M)" = "Drainage...M."
)

# Prepare results table
results <- data.frame(
  y_Axis = character(),
  x_Axis = character(),
  Test = character(),
  P_value = character(),
  Correlation_Coefficient = character(),
  stringsAsFactors = FALSE
)

for (label in names(vars)) {
  var <- vars[[label]]
  test <- cor.test(data$Total, data[[var]], method = "spearman")
  results <- rbind(results, data.frame(
    y_Axis = "Total",
    x_Axis = label,
    Test = "Spearman",
    P_value = format.pval(test$p.value, digits = 3),
    Correlation_Coefficient = round(test$estimate, 3),
    stringsAsFactors = FALSE
  ))
}

# Print results as a table
print(results, row.names = FALSE)

# Plot function
make_plot <- function(x, y, xlab, color, label) {
  ggplot(data, aes_string(x = x, y = y)) +
    geom_point(color = color, alpha = 0.6) +
    geom_smooth(method = "lm", color = color, se = FALSE) +
    labs(title = xlab, subtitle = label, x = xlab, y = "Total") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 11),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
}

# Colors for each plot
plot_colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")

# Create plots
plots <- list()
i <- 1
for (label in names(vars)) {
  var <- vars[[label]]
  test <- cor.test(data$Total, data[[var]], method = "spearman")
  subtitle <- paste0("rho = ", round(test$estimate, 3), ", p = ", format.pval(test$p.value, digits = 3))
  plots[[label]] <- make_plot(var, "Total", label, plot_colors[i], subtitle)
  i <- i + 1
}

# Combine plots in 2x2 grid with patchwork
combined <- (plots[[1]] | plots[[2]]) / (plots[[3]] | plots[[4]]) +
  plot_annotation(title = "New Treatment",
                  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))

ggsave("Karl/new_treatment_total_spearman.png", combined, width = 16, height = 12, dpi = 300) 