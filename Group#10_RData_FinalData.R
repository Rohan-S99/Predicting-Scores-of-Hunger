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
save.image("Group#10_RData_FinalData.RData")
