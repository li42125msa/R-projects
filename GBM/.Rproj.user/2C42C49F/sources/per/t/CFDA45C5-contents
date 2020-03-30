
library(rpart)
library(caret)
library(e1071)
library(ipred)
library(caret)
library(Metrics)
library(readr)
library(gbm)
library(dplyr)
library(randomForest)
library(ROCR)

library(rpart)

library(readr)
grade <- read_csv("grade.csv")
grade=grade %>% mutate_if(is.character, as.factor)
# Look at the data
str(grade)

# Set seed and create assignment 
set.seed(1)
assignment <- sample(1:3, size = nrow(grade), prob = c(0.7, 0.15, 0.15), replace = TRUE)

# Create a train, validation and tests from the original data frame 
grade_train <- grade[assignment == 1, ]  # subset grade to training indices only
grade_valid <- grade[assignment == 2, ]  # subset grade to validation indices only
grade_test <- grade[assignment == 3, ]   # subset grade to test indices only
# Train the model anova for regression

grade_model <- rpart(formula = final_grade ~ ., 
                     data = grade_train, 
                     method = "anova")
 
# Look at the model output                      
print(grade_model)

library(rpart.plot)
# Plot the tree model
rpart.plot(x = grade_model, yesno = 2, type = 0, extra = 0)

library(Metrics) #Evaluate a regression tree model
# Generate predictions on a test set
pred_reg <- predict(object = grade_model,  # model object 
                newdata = grade_test)  # test dataset
pred<- predict(object = grade_model,  # model object 
                    newdata = grade_test)  # test dataset
# Compute the RMSE
rmse(actual = grade_test$final_grade, 
     predicted = pred)

# Plot the "CP Table"
plotcp(grade_model)
names(grade_model)
# Print the "CP Table"
print(grade_model$cptable)

# Retrieve optimal cp value based on cross-validated error
opt_index <- which.min(grade_model$cptable[, "xerror"])
cp_opt <- grade_model$cptable[opt_index, "CP"]

# Prune the model (to optimized cp value)
grade_model_opt <- prune(tree = grade_model, 
                         cp = cp_opt)

# Plot the optimized model
rpart.plot(x = grade_model_opt, yesno = 2, type = 0, extra = 0)

pred_cp <- predict(object = grade_model_opt,  # model object 
                newdata = grade_test)  # test dataset
rmse(actual = grade_test$final_grade, 
     predicted = pred)
rmse(actual = grade_test$final_grade, 
     predicted = pred_cp)

# Establish a list of possible values for minsplit and maxdepth
minsplit <- seq(1, 4, 1)
maxdepth <- seq(1, 6, 1)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(minsplit = minsplit, maxdepth = maxdepth)

# Check out the grid
head(hyper_grid)

# Print the number of grid combinations
nrow(hyper_grid)
# Number of potential models in the grid
num_models <- nrow(hyper_grid)

# Create an empty list to store models
grade_models <- list()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:num_models) {
  
  # Get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # Train a model and store in the list
  grade_models[[i]] <- rpart(formula = final_grade ~ ., 
                             data = grade_train, 
                             method = "anova",
                             minsplit = minsplit,
                             maxdepth = maxdepth)
}

# Number of potential models in the grid
num_models <- length(grade_models)

# Create an empty vector to store RMSE values
rmse_values <- c()

# Write a loop over the models to compute validation RMSE
for (i in 1:num_models) {
  
  # Retrieve the i^th model from the list
  model <- grade_models[[i]]
  
  # Generate predictions on grade_valid 
  pred <- predict(object = model,
                  newdata = grade_valid)
  
  # Compute validation RMSE and add to the 
  rmse_values[i] <- rmse(actual = grade_valid$final_grade, 
                         predicted = pred)
}

# Identify the model with smallest validation set RMSE
best_model <- grade_models[[which.min(rmse_values)]]

# Print the model paramters of the best model
best_model$control

# Compute test set RMSE on best_model
pred <- predict(object = best_model,
                newdata = grade_test)
rmse(actual = grade_test$final_grade, 
     predicted = pred)


########################machine learning toolbox


library(ggplot2)
# Fit lm model: model
model <- lm(price ~ ., diamonds)

# Predict on full data: p
p <- predict(model, diamonds)

# Compute errors: error
error <- p - diamonds[["price"]]

# Calculate RMSE
sqrt(mean(error ^ 2))

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(diamonds))

# Randomly order data
shuffled_diamonds <- diamonds[rows, ]

# Determine row to split on: split
split <- round(nrow(diamonds) * 0.80)

# Create train
train <- diamonds[1:split, ]

# Create test
test <- diamonds[(split + 1):nrow(diamonds), ]

# Fit lm model on train: model
model <- lm(price ~ ., train)

# Predict on test: p
p <- predict(model, test)

# Compute errors: error
error <- p - test[["price"]]

# Calculate RMSE
sqrt(mean(error^2))

library(caret)

# Fit lm model using 10-fold CV: model
model_lm <- train(
  price ~ ., 
  diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
model

# Fit lm model using 5 x 5-fold CV: model
model <- train(
  medv ~ ., 
  Boston,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", 
    number = 5,
    repeats = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
model

# Predict on full Boston dataset
predict(model, Boston)
predict(model_lm,diamonds)

library(purrr)
# Define models (don't change)
models <- mtcars %>% 
  split(mtcars$cyl) %>%
  map(~ lm(mpg ~ wt, data = .))

# Rewrite to be a single command using pipes 

models %>% 
  map(summary) %>%
  map_dbl("r.squared")

# Use map and coef to get the coefficients for each model: coefs
coefs <- map(models, coef)

# Use string shortcut to extract the wt coefficient 
map(coefs, "wt")
# use map_dbl with the numeric shortcut to pull out the second element
map_dbl(coefs, 2)