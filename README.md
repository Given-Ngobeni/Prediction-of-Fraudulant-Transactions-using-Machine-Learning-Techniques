# Prediction of Fraudulent Transactions using Machine Learning Techniques (Honours Project)

## Overview
This research project explores the use of machine learning techniques to detect fraudulent financial transactions. The study evaluates multiple classification models, including Random Forest, Logistic Regression, Decision Tree, Support Vector Machines (SVM), K-Nearest Neighbors (KNN), Artificial Neural Networks (ANN), and Naïve Bayes, to determine the most effective approach for fraud detection.

## Key Features
- **Dataset**: Financial transactions dataset with highly imbalanced fraud occurrences
- **Models Used**:  
  - Random Forest  
  - Logistic Regression  
  - Decision Tree  
  - Support Vector Machines (SVM)  
  - K-Nearest Neighbors (KNN)  
  - Artificial Neural Networks (ANN)  
  - Naïve Bayes  
- **Performance Metrics**:  
  - Accuracy  
  - Sensitivity & Specificity  
  - Positive Predictive Value (PPV)  
  - Negative Predictive Value (NPV)  
  - Balanced Accuracy  

## Findings
- Random Forest, SVM, and KNN achieved the highest accuracy (99.88%) in fraud detection.
- ANN performed poorly, with an accuracy of only 0.36%.
- Logistic Regression, Naive Bayes and Decision Trees provided accuracy of 98.32%, 97.78% and 98.76% respectively.
- Feature importance analysis highlighted transaction amount, time, and user details as critical fraud indicators.

## Implementation
- **Data Preprocessing**: Handling missing values, normality testing, and multicollinearity checks  
- **Feature Engineering**: Selection of relevant predictors for fraud classification  
- **Model Training**: Supervised learning techniques applied to classify fraudulent vs. non-fraudulent transactions  
- **Evaluation**: Comparison of models based on key performance indicators  

## Conclusion
The study highlights the significance of choosing the right machine learning model for fraud detection. The findings suggest that Random Forest, SVM, and KNN are the most effective classifiers, offering high accuracy and robustness against imbalanced datasets. Future research will explore ensemble techniques and explainable AI methods like SHAP for improved interpretability.


