############################################################
# 7COM1079 - Team Research and Development Project
# Analysis.R
#
# Dataset: Fat_Supply_Quantity_Data.csv
#
# Research Question (RQ):
#   "Is there a significant correlation between animal fat
#    consumption and COVID-19 deaths across countries?"
#
# Variables:
#   X = Animal fats (grams per capita per day)
#   Y = Deaths (COVID-19 deaths measure from dataset)
#
# Outputs:
#   - Figure1_scatter_AnimalFats_vs_Deaths.png
#   - Figure2_histogram_AnimalFats.png
#   - Analysis_results.txt (numerical results and test output)
#
############################################################

########## 0. Setup working directory ##########
setwd("/Users/Downloads")

# 1. Load required packages 

required_packages <- c("readr", "dplyr", "ggplot2")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# 2. Load the dataset

# The file must be in your working directory
data <- readr::read_csv("Fat_Supply_Quantity_Data.csv")

# Check the structure and column names (optional but useful)
str(data)
print(names(data))

# 3. Select and clean the analysis variables

# We use:
#   Country      -> for reference
#   Animal fats  -> X (diet variable)
#   Deaths       -> Y (COVID outcome)

analysis_df <- data %>%
  dplyr::select(
    Country,
    X = `Animal fats`,
    Y = Deaths
  ) %>%
  dplyr::filter(
    !is.na(X),
    !is.na(Y)
  )

# Quick summaries of X and Y
summary(analysis_df$X)
summary(analysis_df$Y)

# 4. Visualisation

# 4.1 Main plot: Scatterplot with regression line
# This plot is used for the correlation-type RQ.

scatter_plot <- ggplot2::ggplot(analysis_df, aes(x = X, y = Y)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Animal Fat Consumption and COVID-19 Deaths",
    x = "Animal Fat Supply (grams per capita per day)",
    y = "COVID-19 Deaths"
  ) +
  theme_minimal()

# Show in R
print(scatter_plot)

# Save as an image file (to include in the report, NOT a screenshot)
ggplot2::ggsave(
  filename = "Figure1_scatter_AnimalFats_vs_Deaths.png",
  plot = scatter_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# 4.2 Supporting plot: Histogram of Animal fats
# This helps understand the distribution of the main diet variable.

hist_plot <- ggplot2::ggplot(analysis_df, aes(x = X)) +
  geom_histogram(bins = 15) +
  labs(
    title = "Distribution of Animal Fat Consumption Across Countries",
    x = "Animal Fat Supply (grams per capita per day)",
    y = "Number of Countries"
  ) +
  theme_minimal()

print(hist_plot)

ggplot2::ggsave(
  filename = "Figure2_histogram_AnimalFats.png",
  plot = hist_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# 5. Statistical analysis

# We use Pearson correlation because:
# - Both X and Y are numeric and continuous
# - We are interested in linear association

cor_test <- cor.test(analysis_df$X, analysis_df$Y, method = "pearson")

# Optional: simple linear regression model (Y ~ X)
lm_model <- lm(Y ~ X, data = analysis_df)
lm_summary <- summary(lm_model)

# 6.Assumption checks

# Shapiro-Wilk normality tests for X and Y.
# Note: if many rows, you can sample for the test.

if (nrow(analysis_df) >= 3 && nrow(analysis_df) <= 5000) {
  shapiro_X <- shapiro.test(analysis_df$X)
  shapiro_Y <- shapiro.test(analysis_df$Y)
} else {
  set.seed(123)
  sample_df <- analysis_df[sample(nrow(analysis_df), size = min(5000, nrow(analysis_df))), ]
  shapiro_X <- shapiro.test(sample_df$X)
  shapiro_Y <- shapiro.test(sample_df$Y)
}

# 7. Save numerical results to a text file

sink("Analysis_results.txt")

cat("============================================\n")
cat("7COM1079 - Statistical Analysis Output\n")
cat("============================================\n\n")

cat("Research Question:\n")
cat("Is there a significant correlation between animal fat consumption\n")
cat("and COVID-19 deaths across countries?\n\n")

cat("Variables used:\n")
cat("X = Animal fats (grams per capita per day)\n")
cat("Y = COVID-19 Deaths\n\n")

cat("Number of countries in analysis: ", nrow(analysis_df), "\n\n")

cat("Summary statistics for Animal fats (X):\n")
print(summary(analysis_df$X))
cat("\nSummary statistics for Deaths (Y):\n")
print(summary(analysis_df$Y))

cat("\n--------------------------------------------\n")
cat("Pearson Correlation Test (X vs Y)\n")
cat("--------------------------------------------\n")
print(cor_test)

cat("\n--------------------------------------------\n")
cat("Linear Regression Model: Y ~ X\n")
cat("--------------------------------------------\n")
print(lm_summary)

cat("\n--------------------------------------------\n")
cat("Normality checks (Shapiro-Wilk)\n")
cat("--------------------------------------------\n")
cat("\nX (Animal fats):\n")
print(shapiro_X)
cat("\nY (Deaths):\n")
print(shapiro_Y)

sink()  # Stop writing to Analysis_results.txt

# 8. End of script

cat("Analysis complete. Plots and results have been saved in the working directory.\n")
