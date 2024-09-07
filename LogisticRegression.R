library(class)
library(gmodels)
library(readr)
library(ggstatsplot)
library(ggcorrplot)
library(gridExtra)
library(caret)
library(smotefamily)
library(DataExplorer)
library(modEvA) 
library(performance)
library(pROC)
library(car)
library(lmtest)




# Load the Dataset into a Dataframe
cardiac_data <- read.csv("cardiac.csv",header = TRUE, na.strings=c(""), stringsAsFactors = TRUE)


print(table(cardiac_data$cardiac_condition))
# Check the proportion of target label(HeartDisease
prop.table(table(cardiac_data$cardiac_condition))
round(prop.table(table(cardiac_data$cardiac_condition))*100,1) # Rounding and formatting proportions as percentages

################################## Exploratory Data Analysis  ##################################
# Create a profile report
#create_report(cardiac_data)

#Drop caseno as it has not effect or interpretation to the model
cardiac_data <- subset(cardiac_data, select = -c(caseno))

summary(cardiac_data)

# Categorical Features in Data
cat_features <- cardiac_data[, sapply(cardiac_data, is.factor)] 
cat_columns <- names(cat_features)
cat_columns

# Numerical Features in Data
num_features <- cardiac_data[, sapply(cardiac_data, is.numeric)]
num_columns <- names(num_features)
num_columns

# Summary statistics of the data
summary(cardiac_data)
heart_summary <- (summary(cardiac_data))

# Check for missing values
sapply(cardiac_data, function(x) sum(is.na(x)))

# Checking for Duplicated Values in the dataset
sum(duplicated(cardiac_data)) 

# Check the distribution of the target variable Cardiac Condition
barplot(table(cardiac_data$cardiac_condition), 
        main='Distribution of Cardiac Condition Classes', xlab='cardiac_condition', col=c('steelblue', 'tan3'), 
        legend.text=levels(cardiac_data$cardiac_condition))

# Check Outliers in the target variable 'Cardiac Condition'
boxplot(cardiac_data$cardiac_condition,
        main = "Boxplot of Cardiac Condition",
        col = "skyblue3")
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))


# Outlier Analysis
# Visualize Distribution of Numerical values
boxplots <- lapply(num_columns, function(feature) {
  ggplot(data = cardiac_data, aes(x = 1, y = .data[[feature]])) +
    geom_boxplot() +
    labs(title = feature) +
    theme_minimal()
})
# Arrange the boxplots in a four columns
grid.arrange(grobs = boxplots, ncol = 4)


# Create histograms for numerical columns
histograms <- lapply(num_columns, function(feature) {
  ggplot(data = cardiac_data, aes(x = .data[[feature]])) +
    geom_histogram(bins = 20, fill= 'steelblue',color = "white", alpha = 0.9) +
    labs(title = "") +
    theme_minimal()
})
# Arrange the histograms in a two-column grid
grid.arrange(grobs = histograms, ncol = 2)

# Observation of the relationship between predictor variables and target variable
# Create Box plots for each numerical variable against Cardiac Condition
box_plots <- lapply(num_columns, function(variable) {
  ggplot(cardiac_data, aes(x = cardiac_condition, y = !!as.name(variable))) +
    geom_boxplot(fill = 'steelblue2') +
    labs(title = paste("Box Plot of Cardiac Condition vs", variable),
         x = "Cardiac Condition", y = variable) +
    theme_minimal()
})
#Arrange the scatter plots in a grid
grid.arrange(grobs = box_plots, ncol = 2) 


# Check count based on categorical features
# Create histograms for categorical columns
histograms <- lapply(cat_columns, function(feature) {
  ggplot(data = cardiac_data, aes(x = .data[[feature]])) +
    geom_bar(fill = 'steelblue', color = "white", alpha = 0.9) +
    labs(title = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

# Arrange the histograms in a two-column grid
grid.arrange(grobs = histograms, ncol = 2)

# Check how Cardiac Condition  is split over categorical features

# Create an empty list to store the plots
plots_list <- list()

cat_columns2 <- c("gender")
for (cat_feature in cat_columns2) {
  
  plot_data <- cardiac_data %>%
    ggplot(aes(x = .data[[cat_feature]], fill = cardiac_condition)) +
    geom_bar(position = "stack", stat = "count") +
    labs(title = paste("Relationship between", cat_feature, "and cardiac_condition")) +
    theme_minimal() +
    scale_fill_manual(values = c("Absent" = "steelblue3", "Present" = "tan3")) + 
    facet_grid(. ~ cardiac_condition, scales = "free_y") +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # Append the current plot to the list
  plots_list[[cat_feature]] <- plot_data
}

# Arrange the plots in separate grids
grid.arrange(grobs = plots_list, ncol = 1)


#Kernel Density Estimation plot for each numerical feature

par(mfrow=c(2, 2), mar=c(2, 4, 2, 2), oma=c(0, 0, 2, 0))  # Set up the plotting layout
#Loop through each numerical feature
for (i in 1:length(num_columns)) {
  # Plot the KDE for the numerical features
  plot(density(cardiac_data
               [[num_columns[i]]]), 
       main = paste("Kernel Density Estimation for", num_columns[i]),
       xlab = num_columns[i], ylab = "Density", col = "steelblue", lwd = 2)
  
  # Add density curve to the plot
  lines(density(cardiac_data[[num_columns[i]]]), col = "tan2", lwd = 2)
}
#reset plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))


################################## Feature Engineering  ##################################

# # Binary Encoding
# binary_encoded <- c("gender", "cardiac_condition")
# 
# # Loop through each binary variable and encode it
# for (attr in binary_encoded) {
#   if (attr == "gender") {
#     # For the "gender" variable, encode "Male" as 1 and "Female" as 0
#     cardiac_data[[attr]] <- as.numeric(cardiac_data[[attr]] == "Male")
#   } else {
#     # For cardiac_condition  variable, encode "Present" as 1 and other values as 0
#     cardiac_data[[attr]] <- as.numeric(cardiac_data[[attr]] == "Present")
#   }
# }


# Binary Encoding
binary_encoded <- c("cardiac_condition")

# Loop through each binary variable and encode it
for (attr in binary_encoded) {
  if (attr == "cardiac_condition") {
    # For cardiac_condition  variable, encode "Present" as 1 and other values as 0
    cardiac_data[[attr]] <- as.numeric(cardiac_data[[attr]] == "Present")
  }
}

head(cardiac_data)
# Confirming if variables are changed from "Yes/No" to "1 or 0"
print(table(cardiac_data$cardiac_condition))
str(cardiac_data)



# One-hot encode Nominal variables (non-ordinal)
one_hot_encode <- c("gender")
encoded_features <- data.frame(model.matrix(~ . -1, data = cardiac_data[one_hot_encode]))
str(encoded_features)
# Add the encoded categorical variables to the housing dataframe
cardiac_data <- cbind(cardiac_data, encoded_features)
summary(cardiac_data)

#Drop categorical variables after one-hot encoding and the variables that have 0 range
cardiac_data <- subset(cardiac_data, select = -c(gender))


# Creating a dataframe for numeric variables after binary and onehot encoding
new_numeric_vars <- cardiac_data[, sapply(cardiac_data, is.numeric) & names(cardiac_data) != "cardiac_condition"]
str(new_numeric_vars)
# Check if there are any missing values after Encoding
sapply(cardiac_data, function(x) sum(is.na(x)))





#Checking for correlation of numeric features 
#Correlation matrix: (0<=|c|<0.3: weak), (0.3<=|c|<0.7: moderate) & (0.7<=|c|<1: strong)
cor_matrix <- cor(num_features)
pairs(num_features,col = 'steelblue')
ggcorrplot(cor(cor_matrix),hc.order = TRUE, lab = TRUE,colors = c("steelblue3", "white", "tan3"))

correlation_matrix <- cardiac_data[, sapply(cardiac_data, is.numeric)]
summary(correlation_matrix)
ggcorrplot(cor(correlation_matrix),hc.order = TRUE, lab = TRUE,colors = c("steelblue3", "white", "tan3"))
pairs(correlation_matrix,col = 'steelblue')

cardiac_data <- subset(cardiac_data, select = -c(genderMale))


# Outlier Detection Using Interquatile Range(IQR) method. Identify and remove outliers
# Calculating Q1, Q3, and IQR for each numeric variable
for (variable in num_columns) {
  Q1_var <- quantile(cardiac_data[[variable]], 0.25)
  Q3_var <- quantile(cardiac_data[[variable]], 0.75)
  IQR_value_var <- Q3_var - Q1_var
  
  # Calculate lower and upper bounds
  lower_bound_var <- Q1_var - 1.5 * IQR_value_var
  upper_bound_var <- Q3_var + 1.5 * IQR_value_var
  
  # Remove outliers for each variable
  cardiac_data <- cardiac_data[!(cardiac_data[[variable]] < lower_bound_var | cardiac_data[[variable]] > upper_bound_var), ]
  
}
sapply(cardiac_data, function(x) sum(is.na(x)))
print(table(cardiac_data$cardiac_condition))
summary(cardiac_data)

# Check variables that have near to zero variance and drop them
nearZeroVar(cardiac_data, saveMetrics = TRUE) 

# Feature Scaling
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply min-max scaling to relevant columns
cardiac_data[num_columns] <- lapply(cardiac_data[num_columns], min_max_transform)

#OverSampling
# Oversample the minority class using ROSE
library(ROSE)
# Specify the column names and formula
class_col <- "cardiac_condition"
feature_cols <- setdiff(names(cardiac_data), class_col)
formula_str <- as.formula(paste(class_col, "~", paste(feature_cols, collapse = "+")))

# Use ROSE to oversample and balance the classes
oversampled_data <- ROSE(formula_str, data = cardiac_data, seed = 5)$data

print(table(oversampled_data$cardiac_condition))
# Plot the dataset after Over-sampling
barplot(table(oversampled_data$cardiac_condition), main='Balanced Classes', xlab='cardiac_condition', col=c('steelblue', 'tan3'), legend.text=levels(oversampled_data$cardiac_condition))
ggcorrplot(cor(oversampled_data),hc.order = TRUE, lab = TRUE,colors = c("#6D9EC1", "white", "#E46726"))


################################### Logistic Regression model ####################################
# Check the new class distribution
print(table(oversampled_data$cardiac_condition))
#Logistic Regression model for classification
y <- oversampled_data$cardiac_condition
X <- oversampled_data[, !(names(oversampled_data) %in% c("cardiac_condition"))]

# Split the data into training and testing sets
set.seed(23131535)  # Set seed for reproducibility
#index <- sample(1:nrow(under_sampled_data), 0.75 * nrow(under_sampled_data))  # 75% training, 25% testing
index <- createDataPartition(y, p = 0.80, list = FALSE)

X_train <- X[index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test <- y[-index]

# Combine y_train and X_train into a data frame
train_data <- cbind.data.frame(cardiac_condition = y_train, X_train)
print(table(train_data$cardiac_condition))
sapply(train_data, function(x) sum(is.na(x)))

# Combine y_test and X_test into a data frame
test_data <- cbind.data.frame(cardiac_condition = y_test, X_test)
print(table(test_data$cardiac_condition))
sapply(test_data, function(x) sum(is.na(x)))
sapply(train_data, function(x) sum(is.na(x)))
# Plot the dataset after under-sampling
barplot(table(oversampled_data$cardiac_condition), main='Balanced Classes', xlab='cardiac_condition', col=c('steelblue', 'tan3'), legend.text=levels(oversampled_data$HeartDisease))


null_model <- glm(cardiac_condition ~1, data = oversampled_data, family = binomial())
##################################### Model 1 ####################################################

model1 <- glm(cardiac_condition ~ ., data = oversampled_data, family = binomial)

# Print the summary of the logistic regression model
summary(model1)
coef(model1)
anova(null_model, model1, test = "Chi")

# Predictions on the test set
y_predlr <- predict(model1, newdata = cbind(y_test, X_test), type = "response")
y_predlr <- ifelse(y_predlr > 0.5, 1, 0)

#Model Evaluation

# Confusion Matrix
conf_matrix <- table(Actual = y_test, Predicted = y_predlr)

# Accuracy Score
score <- sum(diag(conf_matrix)) / sum(conf_matrix)
conf_matrix
score
# Classification Report
class_report <- caret::confusionMatrix(conf_matrix)
print(class_report)

# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

# Precision
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(paste("Precision:", precision))

# Recall
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(paste("Recall:", recall))

# F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1 score:", f1_score))



exp(cbind(OR=coef(model1), confint(model1)))

#Get the R-squares
RsqGLM(model=model1) 

#Check for collinearity
check_collinearity(model1)

#Chi-square test of deviance
anova(model1, test = "Chisq")

#Get area under the curve
roc_model1 <- roc(test_data$cardiac_condition, predict(model1, newdata = test_data, type = "response"))
auc(roc_model1)

#Plot ROC curve
plot(roc_model1, main = "ROC Curve - Logistic Regression 1", col = "blue", lwd = 2)
text(0.8, 0.2, paste("AUC =", round(auc(roc_model1), 4)), col = "blue")
legend("bottomright", legend = paste("AUC =", round(auc(roc_model1), 4)), col = "blue", lty = 1, cex = 0.8)



# MSE (Mean Squared Error)
mse <- mean((y_test - y_predlr)^2)
cat("Mean Squared Error (MSE):", mse, "\n")


# RMSE (Root Mean Squared Error)
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")


#Check for Homoscedasticity
bptest(model1)
#Multicollinearity Test:
vif(model1) # there is Low Correlation therefore the model does not have Multicollinearity issue
#Testing for autocorrelation
durbinWatsonTest(model1) #Positive Autocorrelation #0.4764868
check_collinearity(model1) 
#Residuals Check
par(mfrow=c(2,2))
plot(model1, pch=23 ,bg='orange',cex=2)


##################################### Model 2 ####################################################
FitALL<-glm(cardiac_condition ~.,data=oversampled_data)

summary(FitALL)

par(mfrow = c(2, 2)) 
plot(FitALL)

log_reg <- step(FitALL,direction="backward")

final=step(FitALL)
summary(final)

# cardiac_condition ~ age + fitness_score + genderFemale
# 
# Df Deviance    AIC
# <none>              17.829 120.82
# - weight        1   18.453 122.12
# - genderFemale  1   18.781 123.81
# - age           1   20.468 132.07

model2 <- glm(cardiac_condition ~ age + weight + genderFemale, data = oversampled_data, family = binomial)

# Print the summary of the logistic regression model
summary(model2)
coef(model2)

# Predictions on the test set
y_predlr <- predict(model2, newdata = cbind(y_test, X_test), type = "response")
y_predlr <- ifelse(y_predlr > 0.5, 1, 0)

#Model Evaluation

# Confusion Matrix
conf_matrix <- table(Actual = y_test, Predicted = y_predlr)

# Accuracy Score
score <- sum(diag(conf_matrix)) / sum(conf_matrix)
conf_matrix
score
# Classification Report
class_report <- caret::confusionMatrix(conf_matrix)
print(class_report)

# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

# Precision
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(paste("Precision:", precision))

# Recall
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(paste("Recall:", recall))

# F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1 score:", f1_score))



exp(cbind(OR=coef(model2), confint(model2)))

#Get the R-squares
RsqGLM(model=model2) 

#Check for collinearity
check_collinearity(model2)

#Chi-square test of deviance
anova(model2, test = "Chisq")

#Get area under the curve
roc_model2 <- roc(test_data$cardiac_condition, predict(model2, newdata = test_data, type = "response"))
auc(roc_model2)


# MSE (Mean Squared Error)
mse <- mean((y_test - y_predlr)^2)
cat("Mean Squared Error (MSE):", mse, "\n")


# RMSE (Root Mean Squared Error)
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

####### Logistic Regression Assumptions Evaluation ###################
#Check for Homoscedasticity
bptest(model2)
#Multicollinearity Test:
vif(model2) # there is Low Correlation therefore the model does not have Multicollinearity issue
#Testing for autocorrelation
durbinWatsonTest(model2) #Positive Autocorrelation #0.418438
check_collinearity(model2) 
#Residuals Check
par(mfrow=c(2,2))
plot(model2, pch=23 ,bg='orange',cex=2)



