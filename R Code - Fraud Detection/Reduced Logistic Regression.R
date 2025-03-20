# Load required libraries
library(caret)
library(pROC)

# Set working directory and read data
setwd("C:\\Users\\224068619\\Desktop\\fraud data")
data <- read.csv("fraud data.csv")

# Convert date variables to Date type
data$trans_date <- as.Date(data$trans_date)
data$dob <- as.Date(data$dob)

# Create numeric versions of date variables
data$trans_date_numeric <- as.numeric(data$trans_date)
data$dob_numeric <- as.numeric(data$dob)

# Convert is_fraud to factor
data$is_fraud <- as.factor(data$is_fraud)

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)
index <- createDataPartition(data$is_fraud, p = 0.7, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

# Train the logistic regression model using glm with specific variables (excluding state)
logistic_model <- glm(is_fraud ~ amt + unix_time + trans_date_numeric + dob_numeric, 
                      data = train_data, family = binomial)

# Make predictions
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")
logistic_predictions_binary <- ifelse(logistic_predictions > 0.5, 1, 0)

# Create confusion matrix
conf_matrix <- confusionMatrix(factor(logistic_predictions_binary, levels = c(0, 1)), 
                               test_data$is_fraud)
print(conf_matrix)

# Create ROC curve
roc_obj <- roc(as.numeric(test_data$is_fraud) - 1, as.vector(logistic_predictions))
auc_value <- auc(roc_obj)
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 3), ")"))

# Print model summary
summary(logistic_model)