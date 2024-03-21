# Bay Clinic Appointment Prediction - Reducing No-Shows
This repository explores predicting patient appointment no-shows at Bay Clinic, a medical center with 25,000 employees and 3 million patient visits. The MedicalCentre.csv dataset contains patient information such as age, gender, appointment date, and disease history.

![image](https://github.com/sarax0/clinic-no-show-prediction/assets/122404545/c325dd24-a8e7-4b61-bb76-e4435005d828)

This project, utilizes R to build and evaluate models for predicting appointment no-shows.

## Project Structure
```R Script: bayclinic_prediction.R``` - This script contains all the code for data preparation, feature engineering, model development, evaluation, and visualization.
```Data: MedicalCentre.csv``` - The dataset containing patient appointment information.

## Classification (Predicting Appointment No-Shows)

### Data Preprocessing and Feature Engineering:

Missing value imputation and outlier detection/removal.
Feature transformation (e.g., handling negative values, date manipulation).
Feature encoding for categorical variables.
Feature scaling for numerical features.
Correlation analysis for identifying and potentially removing correlated features.
Model Development and Evaluation:

### Develop and evaluate three models: Support Vector Machine (SVM), Decision Tree, and Deep Neural Network (DNN).
Split the data into training and testing sets.
Train each model on the training data.
Evaluate model performance on the testing data using metrics like accuracy, sensitivity, specificity, and ROC analysis.
Model Comparison:

Identify the best performing model based on the chosen metrics.
Compare the performance of SVM and Decision Tree with ROC analysis.

