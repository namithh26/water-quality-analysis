# Water Quality Analysis and Pollution Level Classification

## Project Overview

This project focuses on analyzing water quality parameters and classifying water pollution levels using data analysis and machine learning techniques in R.

The objective is to understand the relationship between water quality indicators and pollution levels, identify important features, and compare the performance of classification models.

## Objectives

- Explore and understand the water quality dataset.
- Perform data cleaning and exploratory data analysis (EDA).
- Analyze relationships between water quality parameters and pollution levels.
- Develop classification models using Logistic Regression and Random Forest.
- Compare model performance using accuracy, recall, and confusion matrices.
- Identify the most influential water quality parameters.

## Dataset

**Dataset:** Water Quality and Pollution Monitoring Dataset

The dataset contains 1,000 observations across 11 variables, including:

- pH
- Turbidity
- Temperature
- Dissolved Oxygen (DO)
- Biochemical Oxygen Demand (BOD)
- Lead
- Mercury
- Arsenic
- Pollution Level

The target variable contains three classes:

| Pollution Level | Class |
|---|---|
| 0 | Good |
| 1 | Moderate |
| 2 | Polluted |

The dataset is synthetic, and the findings should not be interpreted as a direct assessment of real-world water quality.

## Tools and Technologies

- R
- RStudio
- ggplot2
- dplyr
- tidyr
- randomForest
- nnet

## Methodology

1. Data loading and inspection
2. Data cleaning and preprocessing
3. Exploratory Data Analysis
4. Correlation analysis
5. Train-test split
6. Logistic Regression classification
7. Random Forest classification
8. Model evaluation and comparison
9. Feature importance analysis

## Model Performance

The models were evaluated on a test set containing 199 observations.

| Model | Test Accuracy |
|---|---:|
| Logistic Regression | 93.97% |
| Random Forest | 98.99% |

Random Forest achieved higher test accuracy and performed better in identifying the Moderate pollution class.

**Important limitation:** The dataset is highly imbalanced, with only 6 Good, 79 Moderate, and 915 Polluted observations. Neither model correctly identified the Good class in the test set. Therefore, the accuracy values should be interpreted cautiously.

## Key Findings

- Turbidity showed the strongest correlation with Pollution Level among the examined parameters.
- BOD was another important parameter.
- Turbidity and BOD were the top two features in the Random Forest feature-importance analysis.
- Random Forest performed better than Logistic Regression on the current test split.
- More representative data is required for reliable evaluation of all pollution classes.

## Future Work

- Collect more representative water quality data.
- Address class imbalance using suitable techniques.
- Apply stratified cross-validation.
- Evaluate macro F1-score and class-wise performance.
- Validate the models using real-world water quality samples.

## Author

AI and Data Science Engineering Student


