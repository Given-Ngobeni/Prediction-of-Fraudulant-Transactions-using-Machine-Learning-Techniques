# Load libraries
library(randomForest)
library(caret)

setwd("C:\\Users\\224068619\\Desktop\\fraud data")
data <- read.csv("fraud data.csv")

# Convert relevant columns to factors
a <- as.factor(data$gender)
b <- as.factor(data$category)
c <- as.factor(data$state)
y <- as.factor(data$is_fraud)

# Replace the original columns with the renamed variables
data$gender <- a
data$category <- b
data$state <- c
data$is_fraud <- y

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)
index <- createDataPartition(data$is_fraud, p = 0.7, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

# Train the Random Forest model
model <- randomForest(is_fraud ~ ., data = train_data, ntree = 100, mtry = 3, importance = TRUE)

# Make predictions
predictions <- predict(model, test_data)
confusionMatrix(predictions, test_data$is_fraud)

# Check variable importance
importance(model)
varImpPlot(model)
