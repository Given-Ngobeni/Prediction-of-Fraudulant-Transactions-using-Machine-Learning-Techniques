# Load libraries
library(randomForest)
library(caret)

# Set working directory and load data
setwd("C:\\Users\\224068619\\Desktop\\fraud data")
data <- read.csv("fraud data.csv")

# Convert relevant columns to factors
data$gender <- as.factor(data$gender)
data$category <- as.factor(data$category)
data$state <- as.factor(data$state)
data$is_fraud <- as.factor(data$is_fraud)

# Select only the most important variables (without is_fraud)
selected_vars <- c("amt", "dob", "unix_time", "trans_date", "category", "state", "cc_num")
reduced_data <- data[, selected_vars]

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)
index <- createDataPartition(data$is_fraud, p = 0.7, list = FALSE)
train_data <- reduced_data[index, ]
train_labels <- data$is_fraud[index]  # Target labels for training

test_data <- reduced_data[-index, ]
test_labels <- data$is_fraud[-index]  # Target labels for testing

# Train the Random Forest model with reduced variables
reduced_model <- randomForest(x = train_data, y = train_labels, ntree = 100, mtry = 2, importance = TRUE)

# Make predictions
predictions <- predict(reduced_model, test_data)

# Evaluate the model
conf_matrix <- confusionMatrix(predictions, test_labels)
print(conf_matrix)

# Check variable importance
importance(reduced_model)
varImpPlot(reduced_model)
