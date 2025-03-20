# Load necessary libraries
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(ggplot2)
library(dplyr)  # For the pipe and mutate_if

# Convert 'is_fraud' to a factor
data$is_fraud <- as.factor(data$is_fraud)

# Identify categorical and numeric variables
categorical_vars <- sapply(data, is.factor)
numeric_vars <- sapply(data, is.numeric)
all_vars <- c(categorical_vars, numeric_vars)
all_vars <- names(all_vars[all_vars])

# Select relevant variables
data_all <- data[, all_vars]
data_all$is_fraud <- data$is_fraud

# Convert character variables to factors
data_all <- data_all %>%
  mutate_if(is.character, as.factor)

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)
trainIndex <- createDataPartition(data_all$is_fraud, p = 0.7, list = FALSE)
trainData <- data_all[trainIndex, ]
testData <- data_all[-trainIndex, ]

# Build the decision tree model
decision_tree_model <- rpart(is_fraud ~ ., data = trainData, method = "class")

# Plot the decision tree with improved visibility
rpart.plot(
  decision_tree_model, 
  main = "Decision Tree", 
  cex = 0.6,           # Increase text size
  extra = 104,         # Display class probabilities and percentages
  fallen.leaves = TRUE, # Position leaves at the bottom
  box.palette = "GnBu", # Add color to the boxes
  shadow.col = "gray",  # Add shadow for better visibility
  nn = TRUE            # Display node numbers
)

# Make predictions on the test set
predictions <- predict(decision_tree_model, newdata = testData, type = "class")

# Create a confusion matrix to evaluate model performance
confusion_matrix <- confusionMatrix(predictions, testData$is_fraud)
print(confusion_matrix)


# Generate ROC curve and plot it
prob_predictions <- predict(decision_tree_model, newdata = testData, type = "prob")[, 2]
roc_curve <- roc(testData$is_fraud, prob_predictions)
ggroc(roc_curve) +
  ggtitle("ROC Curve") +
  xlab("1 - Specificity") +
  ylab("Sensitivity") +
  theme_minimal()

