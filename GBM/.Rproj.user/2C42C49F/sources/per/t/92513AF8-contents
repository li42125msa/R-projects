
library(caret)
set.seed(42)

# Create partition index
index <- createDataPartition(breast_cancer_data$diagnosis, p = .70, 
                             list = FALSE)
# Subset `breast_cancer_data` with index
bc_train_data <- breast_cancer_data[index, ]
bc_test_data  <- breast_cancer_data[-index, ]

library(caret)
library(tictoc)

# Repeated CV.
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5)

# Train a Random Forest
library(randomForest)
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
library(tictoc)
?randomForest
set.seed(1)  # for reproducibility

# credit_train_r=credit_train
# 
# library(dplyr)
# credit_train_r=credit_train %>% mutate_if(is.character, as.factor)
library(randomForest)
credit_model_randomForest <- randomForest(formula = default ~ ., 
                             data = credit_train)
varImp(credit_model_randomForest)
# Print the model output                             
print(credit_model_randomForest)
names(credit_model_randomForest)
head(credit_model_randomForest$predicted)
head(credit_model_randomForest$importance)
credit_model_randomForest$classes
# Generate predicted classes using the model object
# Grab OOB error matrix & take a look
err <- credit_model_randomForest$err.rate
head(err)
tail(err)
# Look at final OOB error rate (last row in err matrix)
oob_err <- err[nrow(err), "OOB"]
print(oob_err)

# Plot the model trained in the previous exercise
plot(credit_model_randomForest)

# Add a legend since it doesn't have one by default
legend(x = "bottom", 
       legend = colnames(err),
       fill = 1:ncol(err))


# Generate predicted classes using the model object
pred_randomForest <- predict(object = credit_model_randomForest,  # model object 
                            newdata = credit_test,  # test dataset
                            type = "class")         # return classification labels

# Calculate the confusion matrix for the test set
cm <- confusionMatrix(data = pred_randomForest,          # predicted classes
                      reference = credit_test$default)  # actual classes
print(cm)

# Compare test set accuracy to OOB accuracy
paste0("Test Accuracy: ", cm$overall[1])
paste0("OOB Accuracy: ", 1 - oob_err)


# Generate predictions on the test set
pred_randomForest1 <- predict(object = credit_model_randomForest, 
                newdata = credit_test,
                type = "prob")

# `pred` is a matrix
class(pred)

# Look at the pred format
head(pred)                

# Compute the AUC (`actual` must be a binary 1/0 numeric vector)
auc_randomForest=auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
    predicted = pred_randomForest1[,"yes"])    
auc_randomForest
# Execute the tuning process
set.seed(1)              
res <- tuneRF(x = subset(credit_train, select = -default),
              y = credit_train$default,
              ntreeTry = 500)
print(res)
# Establish a list of possible values for mtry, nodesize and sampsize


# Look at results
print(res)

# Find the mtry value that minimizes OOB Error
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
print(mtry_opt)

mtry <- seq(4, ncol(credit_train) * 0.8, 2)
mtry
nodesize <- seq(3, 8, 2)
sampsize <- nrow(credit_train) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)
head(hyper_grid)
# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(formula = default ~ ., 
                        data = credit_train,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
opt_i 
min(oob_err)
print(hyper_grid[opt_i,])

credit_model_randomForest_update <- randomForest(formula = default ~ ., 
                      data = credit_train,
                      mtry = 6,
                      nodesize =3,
                      sampsize = 560)
pred_randomForest_update<-predict(credit_model_randomForest_update,credit_test,method="class")
pred_randomForest_update1<-predict(credit_model_randomForest_update,credit_test,method="prob")
head(pred_randomForest_update1)

cm_RandomForest_update <- confusionMatrix(data = pred_randomForest_update,          # predicted classes
                      reference = credit_test$default)  # actual classes
print(cm_RandomForest_update)

auc_randomForest_update1=auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
                     predicted = pred_randomForest_update1)    
auc_randomForest_update1

summary(credit_model_randomForest)


##########################################supervised machine learning in r##############################
library(ranger)
load("Bikes.Rdata")
head(bikesJuly)

# bikesJuly is in the workspace
str(bikesJuly)

# Random seed to reproduce results
seed=set.seed(1)

# the outcome column
(outcome <- "cnt")

# The input variables
(vars <- c("hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed"))

# Create the formula string for bikes rented as a function of the inputs
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))

(lm_model<-lm(fmla,bikesJuly))
summary(lm_model)
# Fit and print the random forest model.
(bike_model_rf <- ranger(fmla, 
                         bikesJuly, 
                         num.trees = 500, 
                         respect.unordered.factors = "order", 
                         seed = seed))

# bikesAugust is in the workspace
str(bikesAugust)

# bike_model_rf is in the workspace
bike_model_rf
summary(bike_model_rf)
# Make predictions on the August data
bikesAugust$pred <- predict(bike_model_rf, bikesAugust)$predictions

# Calculate the RMSE of the predictions
bikesAugust %>% 
  mutate(residual = cnt - pred)  %>%        # calculate the residual
  summarize(rmse  = sqrt(mean(residual^2))) # calculate rmse

# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(bikesAugust, aes(x = pred, y = cnt)) + 
  geom_point() + 
  geom_abline() #geom_abline(intercept=0, slope=1)


first_two_weeks <- bikesAugust %>% 
  # Set start to 0, convert unit to days
  mutate(instant = (instant - min(instant)) / 24) %>% 
  # Gather cnt and pred into a column named value with key valuetype
  gather(key = valuetype, value = value, cnt, pred) %>%
  # Filter for rows in the first two
  filter(instant < 14) 

# Plot predictions and cnt by date/time 
ggplot(first_two_weeks, aes(x = instant, y = value, color = valuetype, linetype = valuetype)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Predicted August bike rentals, Random Forest plot")

###############################Machine learning toolbox

library(caret)
library(ggplot2)
library(mlbench)
load("Sonar.RData")
wine <- readRDS("C:/R/Project/Model/GBM/wine_100.RDS")
# Fit random forest: model
unique(wine$quality)
model <- train(
  quality ~ .,
  tuneLength = 1,
  data = wine, 
  method = "ranger",
  ,importance = 'impurity',
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
    
  )
)

# Print model to console
model
model$finalModel
ls(model)
varImp(model)
# Fit random forest: model
model <- train(
  quality ~ .,
  tuneLength = 3,
  data = wine, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
model

# Plot model
plot(model)

# Define the tuning grid: tuneGrid
tuneGrid <- data.frame(
  .mtry = c(2, 3, 7),
  .splitrule = "variance",
  .min.node.size = 5
)

model <- train(
  quality ~ .,
  tuneLength = 3,
  tuneGrid=tuneGrid,
  data = wine, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)
plot(model)
caret::varImp(model)
?varImp

# Create custom indices: myFolds
myFolds <- createFolds(churn_y, k = 5)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

# Fit random forest: model_rf
model_rf <- train(
  x = churn_x, 
  y = churn_y,
  metric = "ROC",
  method = "ranger",
  trControl = myControl
)


# Create model_list
model_list <- list(glmnet = model_glmnet, rf = model_rf)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)

# Create bwplot
bwplot(resamples, metric = "ROC")

# Create xyplot
xyplot(resamples, metric = "ROC")

dotplot(resamples,metric="ROC")
# Create partition index
index <- createDataPartition(breast_cancer_data$diagnosis, p = 0.7, list = FALSE)

# Subset `breast_cancer_data` with index
bc_train_data <- breast_cancer_data[index, ]
bc_test_data  <- breast_cancer_data[-index, ]




library(ranger)

# Build a random forest model for each fold
cv_models_rf <- cv_data %>% 
  mutate(model = map(train, ~ranger(formula = life_expectancy~., data = .x,
                                    num.trees = 100, seed = 42)))

# Generate predictions using the random forest model
cv_prep_rf <- cv_models_rf %>% 
 
  
   
  #######################Machine Learning in Tidyverse############################
  library(ranger)

# Explore gapminder
head(gapminder)

# Prepare the nested dataframe gap_nested
library(tidyverse);library(dslabs)
names(gapminder)
head(gapminder)
gap_nested <- gapminder%>% na.omit()%>%
  group_by(country) %>% 
  nest()


# Explore gap_nested
head(gap_nested)


# Create the unnested dataframe called gap_unnnested
gap_unnested <- gap_nested %>% 
  unnest()

# Confirm that your data was not modified  
identical(gapminder, gap_unnested)


# Extract the data of Algeria
algeria_df <- gap_nested$data[[1]]

# Calculate the minimum of the population vector
min(algeria_df$population,na.rm = T)

# Calculate the maximum of the population vector
max(algeria_df$population)

# Calculate the mean of the population vector
mean(algeria_df$population)


map(.x = nested$data, .f = ~mean(.x$population))

pop_df <- gap_nested %>% 
  mutate(pop_mean = map(data, ~mean(.x$population,na.rm = T))) %>% 
  unnest(pop_mean)
 
pop_df %>% 
  unnest(pop_mean)

nested %>%
  mutate(model = map(data, ~lm(formula = population~fertility, data = .x)))

# Calculate the mean population for each country
pop_nested <- gap_nested %>%
  mutate(mean_pop = map(data, ~mean(.x$population)))

# Take a look at pop_nested
head(pop_nested)

# Extract the mean_pop value by using unnest
pop_mean <- pop_nested %>% 
  unnest(mean_pop)

# Take a look at pop_mean
head(pop_mean)

# Calculate mean population and store result as a double
pop_mean <- gap_nested %>%
  mutate(mean_pop = map_dbl(data, ~mean(.x$population)))

# Take a look at pop_mean
head(pop_mean)


# Build a linear model for each country
gap_models <- gap_nested %>%
  mutate(model = map(data, ~lm(formula = life_expectancy~year, data = .x)))

# Extract the model for Algeria    
algeria_model <- gap_models$model[[1]]

# View the summary for the Algeria model
summary(algeria_model)

library(broom)
tidy(algeria_model)
glance(algeria_model)
augment(algeria_model)

augment(algeria_model) %>% 
  ggplot(mapping = aes(x = year)) +
  geom_point(mapping = aes(y = life_expectancy)) +
  geom_line(mapping = aes(y = .fitted), color = "red")


gap_models %>% 
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef)

# Extract the coefficient statistics of each model into nested dataframes
model_coef_nested <- gap_models %>% 
  mutate(coef = map(model, ~tidy(.x)))

# Simplify the coef dataframes for each model    
model_coef <- model_coef_nested %>%
  unnest(coef)

# Plot a histogram of the coefficient estimates for year         
model_coef %>% 
  filter(term == "year") %>% 
  ggplot(aes(x = estimate)) +
  geom_histogram()

# Extract the fit statistics of each model into dataframes
model_perf_nested <- gap_models %>% 
  mutate(fit = map(model, ~glance(.x)))

# Simplify the fit dataframes for each model    
model_perf <- model_perf_nested %>% 
  unnest(fit)

# Look at the first six rows of model_perf
head(model_perf)


# Plot a histogram of rsquared for the 77 models    
model_perf %>% 
  ggplot(aes(x = r.squared)) + 
  geom_histogram()

# Extract the 4 best fitting models
best_fit <- model_perf %>% 
  top_n(n = 4, wt = r.squared)

# Extract the 4 models with the worst fit
worst_fit <- model_perf %>% 
  top_n(n = 4, wt = -r.squared)


best_augmented <- best_fit %>% 
  # Build the augmented dataframe for each country model
  mutate(augmented = map(model, ~augment(.x))) %>% 
  # Expand the augmented dataframes
  unnest(augmented)

worst_augmented <- worst_fit %>% 
  # Build the augmented dataframe for each country model
  mutate(augmented = map(model, ~augment(.x))) %>% 
  # Expand the augmented dataframes
  unnest(augmented)


# Compare the predicted values with the actual values of life expectancy 
# for the top 4 best fitting models
best_augmented %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = life_expectancy)) + 
  geom_line(aes(y = .fitted), color = "red") +
  facet_wrap(~country, scales = "free_y")

# Calculate validate MAE for each fold
cv_eval_rf <- cv_prep_rf %>% 
  mutate(validate_mae = map2_dbl(validate_actual, validate_predicted, ~mae(actual = .x, predicted = .y)))

# Print the validate_mae column
cv_eval_rf$validate_mae

# Calculate the mean of validate_mae column
mean(cv_eval_rf$validate_mae)

# Prepare for tuning your cross validation folds by varying mtry
cv_tune <- cv_data %>% 
  crossing(mtry = 2:5) 

# Build a model for each fold & mtry combination
cv_model_tunerf <- cv_tune %>% 
  mutate(model = map2(.x = train, .y = mtry, ~ranger(formula = life_expectancy~., 
                                                     data = .x, mtry = .y, 
                                                     num.trees = 100, seed = 42)))

# Generate validate predictions for each model
cv_prep_tunerf <- cv_model_tunerf %>% 
  mutate(validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))

# Calculate validate MAE for each fold and mtry combination
cv_eval_tunerf <- cv_prep_tunerf %>% 
  mutate(validate_mae = map2_dbl(.x = validate_actual, .y = validate_predicted, ~mae(actual = .x, predicted = .y)))

# Calculate the mean validate_mae for each mtry used  
cv_eval_tunerf %>% 
  group_by(mtry) %>% 
  summarise(mean_mae = mean(validate_mae))


# Build the model using all training data and the best performing parameter
best_model <- ranger(formula = life_expectancy~., data = training_data,
                     mtry = 4, num.trees = 100, seed = 42)

# Prepare the test_actual vector
test_actual <- testing_data$life_expectancy

# Predict life_expectancy for the testing_data
test_predicted <- predict(best_model, testing_data)$predictions

# Calculate the test MAE
mae(test_actual, test_predicted)


# Build the model using all training data and the best performing parameter
best_model <- ranger(formula = life_expectancy~., data = training_data,
                     mtry = 4, num.trees = 100, seed = 42)

# Prepare the test_actual vector
test_actual <- testing_data$life_expectancy

# Predict life_expectancy for the testing_data
test_predicted <- predict(best_model, testing_data)$predictions

# Calculate the test MAE
mae(test_actual, test_predicted)
  mutate(validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))
  
  # Load the pROC package
  library(pROC)
  
  
  # Estimate the donation probability
  donors$donation_prob <- predict(donation_model, type = "response")
  # Create a ROC curve
  ROC <- roc(donors$donated, donors$donation_prob)
  
  # Plot the ROC curve
  plot(ROC, col = "blue")
  
  # Calculate the area under the curve (AUC)
  auc(ROC)