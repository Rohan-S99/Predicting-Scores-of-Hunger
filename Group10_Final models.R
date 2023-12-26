# Load the data
getwd()
data <- read.csv("Updated_predictors_GFSI.csv")
# Assuming 'data' is your DataFrame loaded in R

# Identify numeric columns with more than 10 unique values on which
# we will remove outliers
numeric_cols <- sapply(data, is.numeric)
data_numeric <- data[, numeric_cols]
numeric_cols_gt_10 <- sapply(data_numeric, function(x) length(unique(x)) > 10)

# Remove top and bottom 2.5% of these numeric variables
for(col in names(data_numeric)[numeric_cols_gt_10]) {
  lower_bound <- quantile(data_numeric[[col]], 0.025, na.rm = TRUE)  # 2.5th percentile
  upper_bound <- quantile(data_numeric[[col]], 0.975, na.rm = TRUE)  # 97.5th percentile
  data <- data[data[[col]] >= lower_bound & data[[col]] <= upper_bound, ]
}

# Continue with your analysis with the cleaned data


# Assuming 'data' is your DataFrame

# Identify numeric columns with more than 10 unique values, excluding 'Country.Name' and 'Score'
numeric_cols <- sapply(data, is.numeric)
data_numeric <- data[, numeric_cols]
numeric_cols_gt_10 <- sapply(data_numeric, function(x) length(unique(x)) > 10)
selected_cols <- names(data_numeric)[numeric_cols_gt_10]
selected_cols <- setdiff(selected_cols, c('Country.Name', 'Score'))

# Apply min-max scaling to the selected numeric columns
data_scaled <- data
for (col in selected_cols) {
  min_val <- min(data_scaled[[col]], na.rm = TRUE)
  max_val <- max(data_scaled[[col]], na.rm = TRUE)
  data_scaled[[col]] <- (data_scaled[[col]] - min_val) / (max_val - min_val)
}
print(dim(data_scaled))

# Your data is now scaled and stored in data_scaled
View(data_scaled)
# Load necessary libraries
library(dplyr)
library(caret)

# LINEAR REGRESSION MODEL
# Set seed for reproducibility
set.seed(123)

# Assuming your scaled dataframe is stored in 'data_scaled'
# Remove 'Country.Name' and 'Year' from the predictors
data_for_model <- select(data_scaled, -c(Country.Name, Year))

# Split the data into training and testing sets (80-20 split)
splitIndex <- createDataPartition(data_for_model$Score, p = .80, list = FALSE, times = 1)
train <- data_for_model[splitIndex, ]
test <- data_for_model[-splitIndex, ]

# Fit a linear regression model on the training set
model <- lm(Score ~ ., data = train)

# Make predictions on both training and testing sets
train_predictions <- predict(model, train)
test_predictions <- predict(model, test)

# Calculate R-squared values for training and testing sets
train_r_squared <- summary(model)$r.squared
test_r_squared <- cor(test$Score, test_predictions)^2

# Print the R-squared values
cat("R-squared for Training Set:", train_r_squared, "\n")
cat("R-squared for Testing Set:", test_r_squared, "\n")
plot(test$Score,test_predictions)

# MARS MODEL
# Load necessary libraries
library(earth)

# Set seed for reproducibility
set.seed(123)

# Assuming your scaled dataframe is stored in 'data_scaled'

# Split the data into training and testing sets (80-20 split)
splitIndex <- createDataPartition(data_for_model$Score, p = .80, list = FALSE, times = 1)
train <- data_for_model[splitIndex, ]
test <- data_for_model[-splitIndex, ]

# Define control using trainControl from caret package
control <- trainControl(method = "cv", number = 10) # 10-fold cross-validation

# Train the MARS model using the 'earth' method
mars_model <- train(Score ~ ., data = train, method = "earth",
                    tuneGrid = expand.grid(degree = 1:3, nprune = 2:15),
                    trControl = control)

# Best tuned parameters
best_degree <- mars_model$bestTune$degree
best_nprune <- mars_model$bestTune$nprune

# Fit the MARS model on the training set with selected parameters
final_mars_model <- earth(Score ~ ., data = train, degree = best_degree, nprune = best_nprune)

# Make predictions on both training and testing sets
train_predictions <- predict(final_mars_model, newdata = train)
test_predictions <- predict(final_mars_model, newdata = test)

# Calculate R-squared values for training and testing sets
train_r_squared <- cor(train$Score, train_predictions)^2
test_r_squared <- cor(test$Score, test_predictions)^2

# Print the R-squared values
cat("R-squared for Training Set:", train_r_squared, "\n")
cat("R-squared for Testing Set:", test_r_squared, "\n")
plot(test$Score,test_predictions)


# LASSO MODEL
library(glmnet)

# Set seed for reproducibility
set.seed(123)

# Assuming your scaled dataframe is stored in 'data_scaled'
# Prepare the data

x <- as.matrix(data_for_model[, -which(names(data_for_model) == "Score")])
y <- data_for_model$Score

# Split the data into training and testing sets (80-20 split)
splitIndex <- createDataPartition(y, p = .80, list = FALSE, times = 1)
x_train <- x[splitIndex, ]
y_train <- y[splitIndex]
x_test <- x[-splitIndex, ]
y_test <- y[-splitIndex]

# Fit Lasso regression model using cross-validation
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)

# Best lambda
best_lambda_lasso <- cv_lasso$lambda.min

# Fit model on training data with selected lambda
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda_lasso)

# Make predictions and calculate R-squared values
lasso_pred_train <- predict(lasso_model, newx = x_train, s = best_lambda_lasso)
lasso_pred_test <- predict(lasso_model, newx = x_test, s = best_lambda_lasso)
lasso_r_squared_train <- cor(y_train, lasso_pred_train)^2
lasso_r_squared_test <- cor(y_test, lasso_pred_test)^2

# Print the R-squared values for Lasso
cat("Lasso R-squared for Training Set:", lasso_r_squared_train, "\n")
cat("Lasso R-squared for Testing Set:", lasso_r_squared_test, "\n")
plot(y_test,lasso_pred_test)

# RIDGE REGRESSION
# Fit Ridge regression model using cross-validation
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)

# Best lambda
best_lambda_ridge <- cv_ridge$lambda.min

# Fit model on training data with selected lambda
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda_ridge)

# Make predictions and calculate R-squared values
ridge_pred_train <- predict(ridge_model, newx = x_train, s = best_lambda_ridge)
ridge_pred_test <- predict(ridge_model, newx = x_test, s = best_lambda_ridge)
ridge_r_squared_train <- cor(y_train, ridge_pred_train)^2
ridge_r_squared_test <- cor(y_test, ridge_pred_test)^2

# Print the R-squared values for Ridge
cat("Ridge R-squared for Training Set:", ridge_r_squared_train, "\n")
cat("Ridge R-squared for Testing Set:", ridge_r_squared_test, "\n")
plot(y_test,ridge_pred_test)

# RANDOM FOREST
# Load necessary libraries

library(randomForest)


# Set seed for reproducibility
set.seed(123)

# Assuming your scaled dataframe is stored in 'data_scaled'
# Prepare the data
x <- data_scaled[, -which(names(data_scaled) == "Score")]
y <- data_scaled$Score

# Split the data into training and testing sets (80-20 split)
splitIndex <- createDataPartition(y, p = .80, list = FALSE, times = 1)
train_set <- data_scaled[splitIndex, ]
test_set <- data_scaled[-splitIndex, ]

# Fit the Random Forest model on the training set
# Fit the Random Forest model on the training set excluding 'Country.Name' and 'Year'
rf_model <- randomForest(Score ~ . - Country.Name - Year, data = train_set, ntree = 500)


# Make predictions on both training and testing sets
train_pred <- predict(rf_model, newdata = train_set)
test_pred <- predict(rf_model, newdata = test_set)
View(test_set)
# 'Score' is the third column in our dataframe
train_set <- cbind(train_set[,1:3], PredictedScores = train_pred, train_set[,4:ncol(train_set)])
View(train_set)

# Create vectors with NA to fill up to 5 entries
top_countries_scores <- rep(NA, 5)
bottom_countries_scores <- rep(NA, 5)
top_countries_predicted <- rep(NA, 5)
bottom_countries_predicted <- rep(NA, 5)

# Assuming 'Country.Name' and 'Score' are in your dataframe
# Identify the top 5 countries
top_indices <- order(-train_set$Score)[1:5]
bottom_indices <- order(train_set$Score)[1:5]

# Replace the NAs with actual scores and predicted scores for top and bottom countries
top_countries_scores[1:length(top_indices)] <- train_set$Score[top_indices]
bottom_countries_scores[1:length(bottom_indices)] <- train_set$Score[bottom_indices]
top_countries_predicted[1:length(top_indices)] <- train_set$PredictedScores[top_indices]
bottom_countries_predicted[1:length(bottom_indices)] <- train_set$PredictedScores[bottom_indices]

# Create a new data frame for comparison
comparison_table <- data.frame(
  Top_5_Countries = train_set$Country.Name[top_indices],
  Top_5_Actual_Scores = top_countries_scores,
  Top_5_Predicted_Scores = top_countries_predicted,
  Bottom_5_Countries = train_set$Country.Name[bottom_indices],
  Bottom_5_Actual_Scores = bottom_countries_scores,
  Bottom_5_Predicted_Scores = bottom_countries_predicted
)

# Display the table
print(comparison_table)
write.csv(comparison_table,"Actual Vs Predicted.csv",row.names = FALSE)


# Assume 'top_bottom_countries' is your dataframe with the top and bottom 5 countries' names.
# Assume 'full_dataset' is your original dataset that includes the predictor values and country names.

# Subset the full dataset for the top and bottom countries and select the top 5 predictors
important_predictors_data <- train_set %>%
  filter(Country.Name %in% top_bottom_countries$Country.Name) %>%
  select(Country.Name,Protein.quality , Access.to.drinking.water,
         Proportion.of.population.under.global.poverty.line, Share.of.non.starchy.foods,
         Operation.of.food.safety.net.program)

# Print the result
print(important_predictors_data)
write.csv(important_predictors_data,"Imortant_Predictors.csv",row.names = FALSE)


# Calculate R-squared values for training and testing sets
train_r_squared <- cor(train_set$Score, train_pred)^2
test_r_squared <- cor(test_set$Score, test_pred)^2


# Print the R-squared values
cat("Random Forest R-squared for Training Set:", train_r_squared, "\n")
cat("Random Forest R-squared for Testing Set:", test_r_squared, "\n")
# Results : Random Forest R-squared for Training Set: 0.9897387 
# Random Forest R-squared for Testing Set: 0.9495487 
plot(test_set$Score,test_pred)

# Assuming 'rf_model' is your fitted Random Forest model
library(randomForest)
library(ggplot2)

# Calculate variable importance
importance <- importance(rf_model)
print(colnames(importance))

# Create a data frame for plotting
var_imp_plot <- data.frame(Variable = row.names(importance), Importance = importance[, 'IncNodePurity'])

# Plotting variable importance
ggplot(var_imp_plot, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Variables") +
  ylab("Importance") +
  ggtitle("Variable Importance Plot")


#PARTIAL DEPENDENCE PLOTS
# Install and load the pdp package
if (!require("pdp")) install.packages("pdp")
library(pdp)

# Assuming 'Score' is your target variable and 'VariableOfInterest' is one of the predictor variables
pdp_object <- partial(rf_model, pred.var = "Trade.freedom", grid.resolution = 20)

# Plotting partial dependence plots
plot(pdp_object, main = "Partial Dependence Plot", sub = "Trade.freedom", xlab = "VariableOfInterest", ylab = "Partial Dependence")

#XG Boost
library(xgboost)
# Set seed for reproducibility
set.seed(123)

# Assuming your scaled dataframe is stored in 'data_scaled'
# Prepare the data

x <- data.matrix(data_for_model[, -which(names(data_for_model) == "Score")])
y <- data_for_model$Score

# Split the data into training and testing sets (80-20 split)
splitIndex <- createDataPartition(y, p = .80, list = FALSE, times = 1)
train_set <- data_for_model[splitIndex, ]
test_set <- data_for_model[-splitIndex, ]

# Convert to xgboost friendly format
dtrain <- xgb.DMatrix(data = data.matrix(train_set[, -which(names(train_set) == "Score")]), label = train_set$Score)
dtest <- xgb.DMatrix(data = data.matrix(test_set[, -which(names(test_set) == "Score")]), label = test_set$Score)

# Fit the XGBoost model
xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror")

# Make predictions on both training and testing sets
train_pred <- predict(xgb_model, newdata = dtrain)
test_pred <- predict(xgb_model, newdata = dtest)

# Calculate R-squared values for training and testing sets
train_r_squared <- cor(train_set$Score, train_pred)^2
test_r_squared <- cor(test_set$Score, test_pred)^2

# Print the R-squared values
cat("XGBoost R-squared for Training Set:", train_r_squared, "\n")
cat("XGBoost R-squared for Testing Set:", test_r_squared, "\n")
plot(test_set$Score,test_pred)

# Assuming you have a dataframe `test_set_with_pred` with actual and predicted scores
# And assuming the country names are included in this dataframe

# Identify top and bottom 5 countries by actual score
top_countries <- test_set_with_pred %>% top_n(5, Score)
bottom_countries <- test_set_with_pred %>% top_n(-5, Score)

# Combine them into one dataframe
top_bottom_countries <- rbind(top_countries, bottom_countries)

# Create the comparison table
comparison_table <- top_bottom_countries %>% 
  select(Country.Name, Actual_Score, Predicted_Score)

# Assuming you have the variable importance plot data in a dataframe `var_importance`
# And assuming that the top 10 predictors are as per the plot you've shown
top_predictors <- head(var_importance$Variable, 10)

# Extract the values of the top predictors for the selected countries
predictor_values <- top_bottom_countries %>%
  select(c('Country.Name', top_predictors))

# Print the tables
print(comparison_table)
print(predictor_values)

save.image("Group#10_RData_Models.RData")

