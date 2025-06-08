library(ggplot2)
library(readr)

# Read the data
data <- read.csv2("Test.csv", stringsAsFactors = FALSE)

# Function to clean numeric data
clean_numeric <- function(x) {
  x <- as.character(x)
  x <- gsub(",", ".", x)
  x <- gsub("\\s+", "", x)
  as.numeric(x)
}

# Clean numeric columns
numeric_cols <- c("Total", "Lenght.of.stay", "Follow.Up", "Age", "BMI", "Weight",
                 "Duration", "Size", "CRP", "Mamma.24h", "Axilla.24h", "pT", "pN", "N.X.x.")
for(col in numeric_cols) {
  if(col %in% names(data)) {
    data[[col]] <- clean_numeric(data[[col]])
  }
}

# Theme for all plots
my_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black", size = 14, face = "bold"),
    legend.background = element_rect(fill = "white", color = NA)
  )

# Regression plots
vars_for_regression <- list(
  "Age" = "Age",
  "BMI" = "BMI",
  "Weight" = "Weight",
  "Duration" = "Duration",
  "Size" = "Size",
  "CRP" = "CRP",
  "Mamma 24h" = "Mamma.24h",
  "Axilla 24h" = "Axilla.24h",
  "pT" = "pT",
  "pN" = "pN",
  "N(X/x)" = "N.X.x.",
  "Length of Stay" = "Lenght.of.stay"
)

for(var_name in names(vars_for_regression)) {
  var <- vars_for_regression[[var_name]]
  if(var %in% names(data)) {
    # Calculate correlation
    complete_data <- na.omit(data.frame(x = data[[var]], y = data$Total))
    if(nrow(complete_data) > 3) {
      cor_test <- cor.test(complete_data$x, complete_data$y, method="spearman", exact=FALSE)
      rho <- round(cor_test$estimate, 3)
      p_val <- format.pval(cor_test$p.value, digits=3)
      
      # Create plot
      p <- ggplot(complete_data, aes(x = x, y = y)) +
        geom_point(alpha = 0.6, color = "steelblue") +
        geom_smooth(method = "lm", color = "red", fill = "pink") +
        labs(
          title = paste0(var_name, " vs Total\n",
                        "Spearman's rho = ", rho,
                        ", p-value = ", p_val),
          x = var_name,
          y = "Total"
        ) +
        my_theme
      
      # Save plot
      ggsave(
        filename = paste0("spearman_", tolower(gsub("[^[:alnum:]]", "", var_name)), "_regression.png"),
        plot = p,
        width = 10,
        height = 8,
        dpi = 300,
        bg = "white"
      )
    }
  }
}

# T-test plots
binary_vars <- c("Diabetes", "Mastectomy")
for(var in binary_vars) {
  if(var %in% names(data)) {
    # T-test
    t_test <- t.test(Total ~ get(var), data = data)
    p_val <- format.pval(t_test$p.value, digits=3)
    
    # Box plot
    p <- ggplot(data, aes(x = factor(get(var)), y = Total)) +
      geom_boxplot(fill = "steelblue", alpha = 0.6) +
      geom_jitter(width = 0.2, alpha = 0.4, color = "steelblue") +
      labs(
        title = paste0(var, " vs Total\n",
                      "t-test p-value = ", p_val),
        x = var,
        y = "Total"
      ) +
      my_theme
    
    ggsave(
      filename = paste0("t_test_plot_", tolower(var), ".png"),
      plot = p,
      width = 10,
      height = 8,
      dpi = 300,
      bg = "white"
    )
  }
}

# ANOVA plots
categorical_vars <- c("Histology..CNB.", "BRCA")
for(var in categorical_vars) {
  if(var %in% names(data)) {
    # ANOVA
    model <- aov(Total ~ factor(get(var)), data = data)
    p_val <- format.pval(summary(model)[[1]]$"Pr(>F)"[1], digits=3)
    
    # Box plot
    p <- ggplot(data, aes(x = factor(get(var)), y = Total)) +
      geom_boxplot(fill = "steelblue", alpha = 0.6) +
      geom_jitter(width = 0.2, alpha = 0.4, color = "steelblue") +
      labs(
        title = paste0(gsub("\\.\\.", " ", var), " vs Total\n",
                      "ANOVA p-value = ", p_val),
        x = gsub("\\.\\.", " ", var),
        y = "Total"
      ) +
      my_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(
      filename = paste0("anova_plot_", tolower(gsub("\\.\\.", "", var)), ".png"),
      plot = p,
      width = 12,
      height = 8,
      dpi = 300,
      bg = "white"
    )
  }
} 