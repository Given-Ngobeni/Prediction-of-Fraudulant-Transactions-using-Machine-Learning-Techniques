# Load libraries
library(e1071)
library(caret)
library(pROC)

# Set working directory and read data
setwd("C:\\Users\\224068619\\Desktop\\fraud data")
data <- read.csv("fraud test.csv")

# Convert categorical variables to factors
categorical_vars <- c("merchant", "category", "gender", "city", "state", "job")
data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

# Convert date and time variables
data$trans_date <- as.Date(data$trans_date)
data$trans_time <- as.POSIXct(data$trans_time, format="%H:%M:%S")
data$dob <- as.Date(data$dob)

# Create the new dataset with specified variables
new_data <- data[, c("trans_date", "trans_time", "cc_num", "merchant", "category", "amt", 
                     "gender", "city", "state", "zip", "lat", "long", "city_pop", "job", 
                     "dob", "trans_num", "unix_time", "merch_lat", "merch_long", "is_fraud")]

# Convert the response variable to a factor with explicit levels
new_data$is_fraud <- factor(new_data$is_fraud, levels = c("0", "1"))

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)
index <- createDataPartition(new_data$is_fraud, p = 0.7, list = FALSE)
train_data <- new_data[index, ]
test_data <- new_data[-index, ]

# Train the Naive Bayes model
model <- naiveBayes(is_fraud ~ ., data = train_data)

# Make predictions
predictions <- predict(model, test_data, type = "raw")

# Convert predictions to factor with the same levels as is_fraud
predictions_factor <- factor(ifelse(predictions[,2] > 0.5, "1", "0"), levels = c("0", "1"))

# Create confusion matrix
conf_matrix <- confusionMatrix(predictions_factor, test_data$is_fraud)

# Print confusion matrix and model performance
print(conf_matrix)

# Create ROC curve
roc_obj <- roc(test_data$is_fraud, predictions[,2])
auc_value <- auc(roc_obj)

# Plot ROC curve
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 3), ")"))

