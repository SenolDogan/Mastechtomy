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

# Clean Total column
data$Total <- clean_numeric(data$Total)

# Enhanced theme for ANOVA plots with pure white background
anova_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(color = "black", size = 12, face = "bold"),
    plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
    legend.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# ANOVA plots with improved visibility
categorical_vars <- c("Histology..CNB.", "BRCA", "Histology..P.")
for(var in categorical_vars) {
  if(var %in% names(data)) {
    # Remove NA values
    complete_data <- data[!is.na(data[[var]]) & !is.na(data$Total), ]
    
    # ANOVA
    model <- aov(Total ~ factor(get(var)), data = complete_data)
    p_val <- format.pval(summary(model)[[1]]$"Pr(>F)"[1], digits=3)
    
    # Box plot with enhanced visibility
    p <- ggplot(complete_data, aes(x = factor(get(var)), y = Total)) +
      geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.4, color = "steelblue", size = 2) +
      labs(
        title = paste0(gsub("\\.\\.", " ", var), " vs Total\n",
                      "ANOVA p-value = ", p_val),
        x = gsub("\\.\\.", " ", var),
        y = "Total"
      ) +
      anova_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
    
    # Save plot with white background
    ggsave(
      filename = paste0("anova_plot_", tolower(gsub("\\.\\.", "", var)), ".png"),
      plot = p,
      width = 12,
      height = 8,
      dpi = 300,
      bg = "white",
      device = "png"
    )
  }
} 