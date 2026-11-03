install.packages(c("tidyverse", "janitor", "skimr", "corrplot", "GGally"))

library(tidyverse)  # dplyr, ggplot2, readr, etc. — the core toolkit
library(janitor)    # clean_names(), quick tabulations
library(skimr)      # nicer summary() output
library(corrplot)   # correlation heatmaps
library(GGally)     # pairwise plots

df <- read.csv("Water_Quality_Dataset.csv")
head(df)
dim(df)
str(df)
colSums(is.na(df))
sum(duplicated(df))
unique(df$Location)
table(df$Pollution_Level)
summary(df)
names(df)
names(df) <- c(
  "Timestamp",
  "Location",
  "pH",
  "Turbidity",
  "Temperature",
  "DO",
  "BOD",
  "Lead",
  "Mercury",
  "Arsenic",
  "Pollution_Level"
)
names(df)

df$Timestamp <- as.POSIXct(df$Timestamp,
                           format = "%Y-%m-%d %H:%M:%S")
str(df$Timestamp)
table(df$Location)
ggplot(df, aes(x = Location)) +
  geom_bar() +
  labs(
    title = "Number of Water Quality Observations by Location",
    x = "Location",
    y = "Number of Observations"
  )
install.packages("ggplot2")
library(ggplot2)
ggplot(df, aes(x = Location, fill = Location)) +
  geom_bar() +
  labs(
    title = "Number of Water Quality Observations by Location",
    x = "Location",
    y = "Number of Observations"
  ) +
  theme_minimal()

ggplot(df, aes(x = factor(Pollution_Level),
               fill = factor(Pollution_Level))) +
  geom_bar() +
  scale_fill_manual(
    values = c("0" = "green3",
               "1" = "gold",
               "2" = "red3"),
    labels = c("Good", "Moderate", "Polluted")
  ) +
  labs(
    title = "Distribution of Pollution Levels",
    x = "Pollution Level",
    y = "Number of Observations",
    fill = "Pollution Status"
  ) +
  theme_minimal()
ggplot(df, aes(x = pH)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of pH",
    x = "pH",
    y = "Frequency"
  ) +
  theme_minimal()
ggplot(df, aes(x = Location, y = pH, fill = Location)) +
  geom_boxplot() +
  labs(
    title = "pH Distribution by Location",
    x = "Location",
    y = "pH"
  ) +
  theme_minimal()

ggplot(df, aes(x = Location, fill = factor(Pollution_Level))) +
  geom_bar() +
  scale_fill_manual(
    values = c(
      "0" = "green3",
      "1" = "gold",
      "2" = "red3"
    ),
    labels = c(
      "0" = "Good",
      "1" = "Moderate",
      "2" = "Polluted"
    )
  ) +
  labs(
    title = "Pollution Levels by Location",
    x = "Location",
    y = "Number of Observations",
    fill = "Pollution Status"
  ) +
  theme_minimal()
install.packages("dplyr", type = "binary")
library(dplyr)

pollution_percent <- df %>%
  count(Location, Pollution_Level) %>%
  group_by(Location) %>%
  mutate(Percentage = n / sum(n) * 100)

pollution_percent

ggplot(pollution_percent,
       aes(x = Location,
           y = Percentage,
           fill = factor(Pollution_Level))) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "0" = "green3",
      "1" = "gold",
      "2" = "red3"
    ),
    labels = c(
      "0" = "Good",
      "1" = "Moderate",
      "2" = "Polluted"
    )
  ) +
  labs(
    title = "Percentage of Pollution Levels by Location",
    x = "Location",
    y = "Percentage (%)",
    fill = "Pollution Status"
  ) +
  theme_minimal()
ggplot(df, aes(x = factor(Pollution_Level),
               y = DO,
               fill = factor(Pollution_Level))) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "0" = "green3",
      "1" = "gold",
      "2" = "red3"
    ),
    labels = c(
      "0" = "Good",
      "1" = "Moderate",
      "2" = "Polluted"
    )
  ) +
  labs(
    title = "Dissolved Oxygen by Pollution Level",
    x = "Pollution Level",
    y = "Dissolved Oxygen (mg/L)",
    fill = "Pollution Status"
  ) +
  theme_minimal()

ggplot(df, aes(x = factor(Pollution_Level),
               y = BOD,
               fill = factor(Pollution_Level))) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "0" = "green3",
      "1" = "gold",
      "2" = "red3"
    ),
    labels = c(
      "0" = "Good",
      "1" = "Moderate",
      "2" = "Polluted"
    )
  ) +
  labs(
    title = "Biochemical Oxygen Demand by Pollution Level",
    x = "Pollution Level",
    y = "BOD (mg/L)",
    fill = "Pollution Status"
  ) +
  theme_minimal()

ggplot(df, aes(x = factor(Pollution_Level),
               y = Turbidity,
               fill = factor(Pollution_Level))) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "0" = "green3",
      "1" = "gold",
      "2" = "red3"
    ),
    labels = c(
      "0" = "Good",
      "1" = "Moderate",
      "2" = "Polluted"
    )
  ) +
  labs(
    title = "Turbidity by Pollution Level",
    x = "Pollution Level",
    y = "Turbidity (NTU)",
    fill = "Pollution Status"
  ) +
  theme_minimal()

ggplot(df, aes(x = factor(Pollution_Level),
               y = Temperature,
               fill = factor(Pollution_Level))) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "0" = "green3",
      "1" = "gold",
      "2" = "red3"
    ),
    labels = c(
      "0" = "Good",
      "1" = "Moderate",
      "2" = "Polluted"
    )
  ) +
  labs(
    title = "Temperature by Pollution Level",
    x = "Pollution Level",
    y = "Temperature (°C)",
    fill = "Pollution Status"
  ) +
  theme_minimal()
library(tidyr)
metals <- df %>%
  select(Pollution_Level, Lead, Mercury, Arsenic) %>%
  pivot_longer(
    cols = c(Lead, Mercury, Arsenic),
    names_to = "Metal",
    values_to = "Concentration"
  )

metals

ggplot(metals,
       aes(x = factor(Pollution_Level),
           y = Concentration,
           fill = factor(Pollution_Level))) +
  geom_boxplot() +
  facet_wrap(~ Metal, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "0" = "green3",
      "1" = "gold",
      "2" = "red3"
    ),
    labels = c(
      "0" = "Good",
      "1" = "Moderate",
      "2" = "Polluted"
    )
  ) +
  labs(
    title = "Heavy Metal Concentrations by Pollution Level",
    x = "Pollution Level",
    y = "Concentration",
    fill = "Pollution Status"
  ) +
  theme_minimal()

cor_matrix <- cor(
  df[, c("pH", "Turbidity", "Temperature", "DO",
         "BOD", "Lead", "Mercury", "Arsenic")]
)

round(cor_matrix, 2)

cor_long <- as.data.frame(cor_matrix) %>%
  mutate(Variable1 = rownames(cor_matrix)) %>%
  pivot_longer(
    cols = -Variable1,
    names_to = "Variable2",
    values_to = "Correlation"
  )

ggplot(cor_long, aes(x = Variable1,
                     y = Variable2,
                     fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation Heatmap of Water Quality Parameters",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
pollution_cor <- cor(
  df[, c("pH", "Turbidity", "Temperature", "DO",
         "BOD", "Lead", "Mercury", "Arsenic",
         "Pollution_Level")]
)

round(pollution_cor["Pollution_Level", ], 2)

pollution_cor_values <- round(
  pollution_cor["Pollution_Level", -9],
  2
)

cor_df <- data.frame(
  Parameter = names(pollution_cor_values),
  Correlation = as.numeric(pollution_cor_values)
)

ggplot(cor_df,
       aes(x = reorder(Parameter, Correlation),
           y = Correlation,
           fill = Correlation)) +
  geom_col() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    title = "Correlation of Water Quality Parameters with Pollution Level",
    x = "Parameter",
    y = "Correlation"
  ) +
  coord_flip() +
  theme_minimal()

colSums(is.na(df))
numeric_data <- df[, c(
  "pH", "Turbidity", "Temperature", "DO",
  "BOD", "Lead", "Mercury", "Arsenic"
)]

numeric_scaled <- as.data.frame(scale(numeric_data))

numeric_long <- numeric_scaled %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "Scaled_Value"
  )

ggplot(numeric_long,
       aes(x = Parameter,
           y = Scaled_Value,
           fill = Parameter)) +
  geom_boxplot() +
  labs(
    title = "Outlier Overview of Water Quality Parameters",
    x = "Parameter",
    y = "Standardized Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
outlier_count <- sapply(numeric_data, function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- Q3 - Q1
  
  sum(x < (Q1 - 1.5 * IQR_value) |
        x > (Q3 + 1.5 * IQR_value))
})

outlier_count
head(df)

# Check dataset dimensions
dim(df)

# Check column names
names(df)

# Check pollution-level distribution
table(df$Pollution_Level)

# Check missing values
colSums(is.na(df))


# Create a separate dataset for modelling
df_model <- df[, c(
  "pH", "Turbidity", "Temperature",
  "DO", "BOD", "Lead", "Mercury", "Arsenic",
  "Pollution_Level"
)]

# Convert the target into a categorical variable
df_model$Pollution_Level <- factor(
  df_model$Pollution_Level,
  levels = c(0, 1, 2),
  labels = c("Good", "Moderate", "Polluted")
)

# Verify the prepared data
str(df_model)
table(df_model$Pollution_Level)

set.seed(123)

# Get row indices for each pollution class
class_indices <- split(
  seq_len(nrow(df_model)),
  df_model$Pollution_Level
)

# Select approximately 20% from each class for testing
test_indices <- unlist(
  lapply(class_indices, function(indices) {
    sample(indices, size = max(1, floor(length(indices) * 0.20)))
  }),
  use.names = FALSE
)

# Create training and testing datasets
train_data <- df_model[-test_indices, ]
test_data  <- df_model[test_indices, ]

# Check class distribution
table(train_data$Pollution_Level)
table(test_data$Pollution_Level)

# Check dimensions
dim(train_data)
dim(test_data)

table(train_data$Pollution_Level)
table(test_data$Pollution_Level)


# Install only if it is not already installed
if (!requireNamespace("nnet", quietly = TRUE)) {
  install.packages("nnet")
}

library(nnet)


# Select predictor columns
predictor_names <- c(
  "pH", "Turbidity", "Temperature", "DO",
  "BOD", "Lead", "Mercury", "Arsenic"
)

# Calculate scaling values from training data
train_means <- sapply(train_data[predictor_names], mean)
train_sds   <- sapply(train_data[predictor_names], sd)

# Scale training and testing predictors
train_scaled <- train_data
test_scaled  <- test_data

train_scaled[predictor_names] <- scale(
  train_data[predictor_names],
  center = train_means,
  scale = train_sds
)

test_scaled[predictor_names] <- scale(
  test_data[predictor_names],
  center = train_means,
  scale = train_sds
)


# Train multinomial logistic regression
model_logistic <- multinom(
  Pollution_Level ~ .,
  data = train_scaled,
  trace = FALSE,
  MaxNWts = 1000,
  maxit = 1000
)

# View model results
summary(model_logistic)


# Predict pollution levels for unseen test data
predictions <- predict(
  model_logistic,
  newdata = test_scaled
)

# Compare predicted and actual classes
confusion_matrix <- table(
  Actual = test_scaled$Pollution_Level,
  Predicted = predictions
)

print(confusion_matrix)


# Overall accuracy
accuracy <- sum(diag(confusion_matrix)) /
  sum(confusion_matrix)

# Precision for each class
precision <- diag(confusion_matrix) /
  colSums(confusion_matrix)

# Recall for each class
recall <- diag(confusion_matrix) /
  rowSums(confusion_matrix)

# F1-score for each class
f1_score <- ifelse(
  precision + recall == 0,
  0,
  2 * precision * recall / (precision + recall)
)

# Combine results
metrics <- data.frame(
  Class = names(precision),
  Precision = as.numeric(precision),
  Recall = as.numeric(recall),
  F1_Score = as.numeric(f1_score)
)

print(paste("Overall Accuracy:", round(accuracy, 4)))
print(metrics)


if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}

library(randomForest)



set.seed(123)

model_rf <- randomForest(
  Pollution_Level ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Predict on the same test set
predictions_rf <- predict(
  model_rf,
  newdata = test_data
)

# Confusion matrix
confusion_rf <- table(
  Actual = test_data$Pollution_Level,
  Predicted = predictions_rf
)

print(confusion_rf)



# Overall accuracy
accuracy_rf <- sum(diag(confusion_rf)) /
  sum(confusion_rf)

# Per-class metrics
precision_rf <- diag(confusion_rf) /
  colSums(confusion_rf)

recall_rf <- diag(confusion_rf) /
  rowSums(confusion_rf)

f1_rf <- ifelse(
  precision_rf + recall_rf == 0,
  0,
  2 * precision_rf * recall_rf /
    (precision_rf + recall_rf)
)

# Display results
metrics_rf <- data.frame(
  Class = names(precision_rf),
  Precision = round(as.numeric(precision_rf), 3),
  Recall = round(as.numeric(recall_rf), 3),
  F1_Score = round(as.numeric(f1_rf), 3)
)

print(paste("Random Forest Accuracy:",
            round(accuracy_rf * 100, 2), "%"))

print(metrics_rf)


# Extract feature importance
importance_rf <- importance(model_rf)

# Display importance measures
print(importance_rf)

# Plot feature importance
varImpPlot(
  model_rf,
  main = "Water Quality - Random Forest Feature Importance"
)


# Compare the two classification models

model_comparison <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  Accuracy = c(accuracy, accuracy_rf) * 100,
  Good_Recall = c(recall["Good"], recall_rf["Good"]) * 100,
  Moderate_Recall = c(
    recall["Moderate"], recall_rf["Moderate"]
  ) * 100,
  Polluted_Recall = c(
    recall["Polluted"], recall_rf["Polluted"]
  ) * 100
)

# Round results
model_comparison[-1] <- lapply(
  model_comparison[-1],
  round,
  digits = 2
)

print(model_comparison)


# Save current plotting settings
old_par <- par(no.readonly = TRUE)

barplot(
  model_comparison$Accuracy,
  names.arg = model_comparison$Model,
  col = c("grey70", "darkseagreen3"),
  ylim = c(0, 105),
  main = "Model Accuracy Comparison",
  ylab = "Accuracy (%)",
  border = NA
)

# Add percentage labels
text(
  x = c(0.7, 1.9),
  y = model_comparison$Accuracy + 3,
  labels = paste0(model_comparison$Accuracy, "%")
)

par(old_par)


write.csv(
  model_comparison,
  "model_comparison.csv",
  row.names = FALSE
)
getwd()


# Prepare recall comparison data

recall_comparison <- data.frame(
  Class = rep(
    c("Good", "Moderate", "Polluted"),
    each = 2
  ),
  Model = rep(
    c("Logistic Regression", "Random Forest"),
    times = 3
  ),
  Recall = c(
    recall["Good"] * 100,
    recall_rf["Good"] * 100,
    recall["Moderate"] * 100,
    recall_rf["Moderate"] * 100,
    recall["Polluted"] * 100,
    recall_rf["Polluted"] * 100
  )
)

print(recall_comparison)


# Grouped recall comparison chart

recall_matrix <- rbind(
  "Logistic Regression" = as.numeric(recall) * 100,
  "Random Forest" = as.numeric(recall_rf) * 100
)

colnames(recall_matrix) <- names(recall)

barplot(
  recall_matrix,
  beside = TRUE,
  col = c("grey70", "darkseagreen3"),
  ylim = c(0, 110),
  main = "Recall Comparison by Pollution Class",
  ylab = "Recall (%)",
  xlab = "Pollution Class",
  legend.text = rownames(recall_matrix),
  args.legend = list(
    x = "topleft",
    bty = "n",
    cex = 0.8
  )
)

png(
  "recall_comparison.png",
  width = 1200,
  height = 800,
  res = 120
)

barplot(
  recall_matrix,
  beside = TRUE,
  col = c("grey70", "darkseagreen3"),
  ylim = c(0, 110),
  main = "Recall Comparison by Pollution Class",
  ylab = "Recall (%)",
  xlab = "Pollution Class",
  legend.text = rownames(recall_matrix),
  args.legend = list(
    x = "topleft",
    bty = "n",
    cex = 0.8
  )
)

dev.off()

bp <- barplot(
  recall_matrix,
  beside = TRUE,
  col = c("grey70", "darkseagreen3"),
  ylim = c(0, 112),
  main = "Recall Comparison by Pollution Class",
  ylab = "Recall (%)",
  xlab = "Pollution Class",
  legend.text = rownames(recall_matrix),
  args.legend = list(
    x = "topleft",
    bty = "n",
    cex = 0.8
  )
)

# Add percentage labels
text(
  x = bp,
  y = recall_matrix + 2,
  labels = paste0(
    round(as.vector(recall_matrix), 2), "%"
  ),
  cex = 0.75
)

png(
  "recall_comparison.png",
  width = 1200,
  height = 800,
  res = 120
)

bp <- barplot(
  recall_matrix,
  beside = TRUE,
  col = c("grey70", "darkseagreen3"),
  ylim = c(0, 112),
  main = "Recall Comparison by Pollution Class",
  ylab = "Recall (%)",
  xlab = "Pollution Class",
  legend.text = rownames(recall_matrix),
  args.legend = list(
    x = "topleft",
    bty = "n",
    cex = 0.8
  )
)

text(
  x = bp,
  y = recall_matrix + 2,
  labels = paste0(
    round(as.vector(recall_matrix), 2), "%"
  ),
  cex = 0.75
)

dev.off()



