# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(nortest)
library(corrplot)
library(ggplot2)
library(pROC)

# Load your data into 'df' if not already loaded
df <- read.csv("fraud test.csv")

# List of continuous variables
continuous_vars <- c("cc_num", "amt", "zip", "lat", "long", "city_pop", "unix_time", "merch_lat", "merch_long")

# Function to perform KS test and return p-value
ks_test <- function(var) {
  ks.test(df[[var]], "pnorm", mean = mean(df[[var]], na.rm = TRUE), sd = sd(df[[var]], na.rm = TRUE))$p.value
}

# Function to perform Anderson-Darling test and return p-value
ad_test <- function(var) {
  ad.test(df[[var]])$p.value
}

# Apply KS test and Anderson-Darling test to each continuous variable
ks_results <- sapply(continuous_vars, ks_test)
ad_results <- sapply(continuous_vars, ad_test)

# Print KS and Anderson-Darling test results
cat("Kolmogorov-Smirnov Test Results:\n")
print(ks_results)
cat("\nAnderson-Darling Test Results:\n")
print(ad_results)

# Function to create QQ plot
qq_plot <- function(var) {
  ggplot(df, aes(sample = df[[var]])) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("QQ Plot for", var))
}

# Function to create histogram with density curve
histogram_plot <- function(var) {
  ggplot(df, aes(x = df[[var]])) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
    geom_density(col = "red", size = 1) +
    ggtitle(paste("Histogram for", var)) +
    xlab(var) +
    ylab("Density")
}

# Create both QQ plots and Histograms for each variable
plots <- lapply(continuous_vars, function(var) {
  qq <- qq_plot(var)
  hist <- histogram_plot(var)
  list(qq, hist)  # Return a list of the QQ plot and histogram for each variable
})

for (i in seq(1, length(continuous_vars), by = 3)) {
  # Extract the next 3 variables' plots (QQ + Histogram for each)
  current_plots <- unlist(plots[i:(i+2)], recursive = FALSE)
  
  # Plot them in a 2x3 grid (2 columns for each variable's QQ plot and histogram)
  grid.arrange(grobs = current_plots, ncol = 2, nrow = 3)
}

boxplot_list_1 <- lapply(continuous_vars[1:6], function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_boxplot(fill = "lightblue") +
    ggtitle(paste("Boxplot for", var)) +
    coord_flip() +  # Make the boxplot horizontal
    theme_minimal()
})

boxplot_list_2 <- lapply(continuous_vars[7:9], function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_boxplot(fill = "lightblue") +
    ggtitle(paste("Boxplot for", var)) +
    coord_flip() +  # Make the boxplot horizontal
    theme_minimal()
})

# Arrange plots for the first 6 variables
grid.arrange(grobs = boxplot_list_1, ncol = 3)

# Arrange plots for the last 3 variables
grid.arrange(grobs = boxplot_list_2, ncol = 3)

# Select only continuous variables for correlation
continuous_vars_df <- df[sapply(df, is.numeric)]

# Calculate the correlation matrix
correlation_matrix <- cor(continuous_vars_df, use = "pairwise.complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Visualize the correlation matrix
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45,  # Text label color and rotation
         addCoef.col = "black",          # Add correlation coefficient to the plot
         diag = FALSE)                   # Do not show the diagonal
