# Load required libraries
library(neuralnet)
library(caret)
library(pROC)

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

# Create the new dataset with specified variables
new_data <- data[, c("trans_date", "trans_time", "cc_num", "merchant", "category", "amt", 
                     "gender", "city", "state", "zip", "lat", "long", "city_pop", "job", 
                     "dob", "unix_time", "merch_lat", "merch_long", "is_fraud")]

# Prepare data for neural network
prepare_data <- function(data) {
  # One-hot encode categorical variables
  dummies <- dummyVars(~ merchant + category + gender + city + state + job, data = data)
  encoded <- predict(dummies, newdata = data)
  
  # Normalize numeric variables
  numeric_vars <- c("amt", "zip", "lat", "long", "city_pop", "merch_lat", "merch_long")
  numeric_data <- scale(data[, numeric_vars])
  
  # Convert dates to numeric (days since a reference date)
  reference_date <- as.Date("1970-01-01")
  date_vars <- c("trans_date", "dob")
  date_data <- sapply(data[, date_vars], function(x) as.numeric(as.Date(x) - reference_date))
  
  # Convert time to seconds since midnight
  time_data <- as.numeric(data$trans_time - trunc(data$trans_time, "days"))
  
  # Combine all prepared variables
  prepared_data <- data.frame(encoded, numeric_data, date_data, time_data, unix_time = data$unix_time)
  
  # Add the target variable
  prepared_data$is_fraud <- as.factor(data$is_fraud)
  
  return(prepared_data)
}

# Prepare the data
prepared_data <- prepare_data(new_data)

# Check the structure of prepared_data
str(prepared_data)

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)
index <- createDataPartition(prepared_data$is_fraud, p = 0.7, list = FALSE)
train_data <- prepared_data[index, ]
test_data <- prepared_data[-index, ]

# Define the formula for the neural network
# We exclude 'is_fraud' from the input variables
formula <- as.formula(paste("is_fraud ~", paste(setdiff(names(prepared_data), "is_fraud"), collapse = " + ")))

# Train the neural network
# Note: This may take some time depending on your dataset size
nn_model <- neuralnet(formula, 
                      data = train_data, 
                      hidden = c(10, 5),  # Two hidden layers with 10 and 5 neurons
                      linear.output = FALSE,
                      threshold = 0.01,
                      stepmax = 1e6)

# Make predictions
nn_predictions <- predict(nn_model, test_data)

# Convert predictions to binary (0 or 1)
nn_predictions_binary <- ifelse(nn_predictions[,1] > 0.5, 1, 0)

# Create confusion matrix
conf_matrix <- confusionMatrix(factor(nn_predictions_binary, levels = c(0, 1)), 
                               test_data$is_fraud)

# Print confusion matrix and model performance
print(conf_matrix)

# Create ROC curve
roc_obj <- roc(as.numeric(test_data$is_fraud) - 1, nn_predictions[,1])
auc_value <- auc(roc_obj)

# Plot ROC curve
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 3), ")"))
