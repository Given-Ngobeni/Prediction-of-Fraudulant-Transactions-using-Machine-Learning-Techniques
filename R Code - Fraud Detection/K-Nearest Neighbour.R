# Load libraries
library(class)
library(caret)
library(pROC)
library(dplyr)
library(lubridate)

# Set working directory and read data
setwd("C:\\Users\\224068619\\Desktop\\fraud data")
data <- read.csv("fraud data.csv")

# Convert categorical variables to factors
categorical_vars <- c("merchant", "category", "gender", "city", "state", "job")
data[categorical_vars] <- lapply(data[categorical_vars], as.factor)

# Convert date and time variables
data$trans_date <- as.Date(data$trans_date)
data$trans_time <- as.POSIXct(data$trans_time, format="%H:%M:%S")
data$dob <- as.Date(data$dob)

# Create new features from date and time
data$trans_year <- year(data$trans_date)
data$trans_month <- month(data$trans_date)
data$trans_day <- day(data$trans_date)
data$trans_hour <- hour(data$trans_time)
data$trans_minute <- minute(data$trans_time)

# Calculate age
data$age <- as.numeric(difftime(Sys.Date(), data$dob, units = "weeks")) / 52.25

# Remove original date and time columns
data <- data %>% select(-trans_date, -trans_time, -dob)

# Convert the response variable to a factor with explicit levels
data$is_fraud <- factor(data$is_fraud, levels = c("0", "1"))

# Remove rows with NA values
data <- na.omit(data)

# Identify numeric and categorical columns
numeric_vars <- sapply(data, is.numeric)
categorical_vars <- sapply(data, is.factor)

# Remove 'is_fraud' from predictors
numeric_vars['is_fraud'] <- FALSE
categorical_vars['is_fraud'] <- FALSE

# Create dummy variables for categorical variables
dummy <- dummyVars(" ~ .", data = data[, categorical_vars])
categorical_data <- predict(dummy, newdata = data[, categorical_vars])

# Combine numeric and categorical data
model_data <- cbind(data[, numeric_vars], categorical_data)

# Remove rows with infinite values
model_data <- model_data[is.finite(rowSums(model_data)),]

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)
index <- createDataPartition(data$is_fraud, p = 0.7, list = FALSE)
train_data <- model_data[index, ]
test_data <- model_data[-index, ]
train_labels <- data$is_fraud[index]
test_labels <- data$is_fraud[-index]

# Prepare the data for KNN (normalize all variables)
preproc <- preProcess(train_data, method = c("center", "scale"))
train_data_normalized <- predict(preproc, train_data)
test_data_normalized <- predict(preproc, test_data)

# Train the KNN model
k <- 5  # You can adjust this value
model <- knn(train = train_data_normalized,
             test = test_data_normalized,
             cl = train_labels,
             k = k)

# Create confusion matrix
conf_matrix <- confusionMatrix(model, test_labels)

# Print confusion matrix and model performance
print(conf_matrix)

# Create ROC curve
roc_obj <- roc(test_labels, as.numeric(model))
auc_value <- auc(roc_obj)

# Plot ROC curve
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 3), ")"))