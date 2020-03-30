
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
# Convert "yes" to 1, "no" to 0
credit_train_GBM=credit_train
credit_test_GBM=credit_test
credit_train_GBM$default <- ifelse(credit_train_GBM$default == "yes", 1, 0)
credit_test_GBM$default <- ifelse(credit_test_GBM$default == "yes", 1, 0)

library(gbm)

# Train a 10000-tree GBM model
set.seed(1)
credit_model_GBM <- gbm(formula = default ~ ., 
                    distribution = "bernoulli", 
                    data = credit_train_GBM,
                    n.trees = 10000)

# Print the model object                    
print(credit_model_GBM) 

# summary() prints variable importance
summary(credit_model_GBM)  


summary(credit_model)# relativ importance
# var     rel.inf
# checking_balance         checking_balance 33.49502510
# amount                             amount 11.62938098
# months_loan_duration months_loan_duration 11.17113439
# credit_history             credit_history 11.15698321
# savings_balance           savings_balance  6.44293358
# employment_duration   employment_duration  6.06266137
# age                                   age  5.73175696
# percent_of_income       percent_of_income  3.74219743
# other_credit                 other_credit  3.56695375
# purpose                           purpose  3.38820798
# housing                           housing  1.55169398
# years_at_residence     years_at_residence  1.35255308
# job                                   job  0.47631930
# phone                               phone  0.09142691
# existing_loans_count existing_loans_count  0.08924265
# dependents                     dependents  0.05152933
# 



# Since we converted the training response col, let's also convert the test response col



# Generate predictions on the test set
preds1_GBM <- predict(object = credit_model_GBM, 
                  newdata = credit_test_GBM,
                  n.trees = 10000)

# Generate predictions on the test set (scale to response)
preds2_GBM <- predict(object = credit_model_GBM, 
                  newdata = credit_test_GBM,
                  n.trees = 10000,
                  type = "response")


# Compare the range of the two sets of predictions
range(preds1_GBM);head(preds1_GBM)
range(preds2_GBM);head(preds2_GBM)

# Generate the test set AUCs using the two sets of preditions & compare
auc(actual = credit_test_GBM$default, predicted = preds1_GBM)  #default
auc_GBM=auc(actual = credit_test_GBM$default, predicted = preds2_GBM)  #rescaled
auc_GBM
# Optimal ntree estimate based on OOB
ntree_opt_oob <- gbm.perf(object = credit_model_GBM, 
                          method = "OOB", 
                          oobag.curve = TRUE)

# Train a CV GBM model
set.seed(1)
credit_model_cv <- gbm(formula = default ~ ., 
                       distribution = "bernoulli", 
                       data = credit_train_GBM,
                       n.trees = 10000,
                       cv.folds = 2)

# Optimal ntree estimate based on CV
ntree_opt_cv <- gbm.perf(object = credit_model_cv, 
                         method = "cv")

# Compare the estimates                         
print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob))                         
print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))
# Generate predictions on the test set using ntree_opt_oob number of trees
preds1_oob <- predict(object = credit_model_GBM, 
                  newdata = credit_test_GBM,
                  n.trees = ntree_opt_oob)

# Generate predictions on the test set using ntree_opt_cv number of trees
preds2_cv <- predict(object = credit_model_cv, 
                  newdata = credit_test_GBM,
                  n.trees = ntree_opt_cv)   

# Generate the test set AUCs using the two sets of preditions & compare
auc_GBM_OOB <- auc(actual = credit_test_GBM$default, predicted = preds1_oob)  #OOB
auc_GBM_CV <- auc(actual = credit_test_GBM$default, predicted = preds2_cv)  #CV 

# Compare AUC 
print(paste0("Test set AUC (OOB): ", auc_GBM_OOB))                         
print(paste0("Test set AUC (CV): ", auc_GBM_CV ))

# Generate the test set AUCs using the two sets of predictions & compare
# actual <- credit_test$default
# dt_auc <- auc(actual = actual, predicted = pred_tree)
# bag_auc <- auc(actual = actual, predicted = pred_bag)
# rf_auc <- auc(actual = actual, predicted = pred_randomForest_update)
# gbm_auc <- auc(actual = credit_test_GBM$default, predicted = preds2_cv)
# 
# Print results
sprintf("Decision Tree Test  AUC: %.3f", auc)
sprintf("Decision Tree Test GINI AUC: %.3f", auc_gini)
sprintf("Bagged Trees Test AUC: %.3f", auc_bag_caret)
sprintf("Random Forest Test AUC: %.3f", auc_randomForest)
sprintf("Random Forest Test OOB AUC: %.3f", auc_randomForest_update1)

sprintf("GBM Test OOB AUC: %.3f", auc_GBM_OOB)
sprintf("GBM Test CVAUC: %.3f", auc_GBM_CV)
sprintf("GBM Test AUC: %.3f", auc_GBM)


install.packages("ROCR");
library(ROCR)

# List of predictions
preds_list <- list(pred_tree1[,"yes"],pred_bag_caret[,"yes"],pred_randomForest1[,"yes"],preds2_GBM)
preds_list
# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(credit_test$default), m)
actuals_list
# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("Decision Tree", "Bagged Trees", "Random Forest", "GBM"),
       fill = 1:m)
#################################################vgboost#####################################################
dframe<-data.frame
color size popularity
1      b   13  1.0785088
2      r   11  1.3956245
3      r   15  0.9217988
4      r   14  1.2025453
5      r   13  1.0838662
6      b   11  0.8043527
7      r    9  1.1035440
8      g   12  0.8746332
9      b    7  0.6947058
10     b   12  0.8832502



# dframe is in the workspace
dframe

# Create a vector of variable names
(vars <- c("color", "size"))

# Load the package vtreat
library(vtreat);library(magrittr)

# Create the treatment plan
treatplan <- designTreatmentsZ(dframe, vars)

# Examine the scoreFrame
(scoreFrame <- treatplan %>%
    use_series(scoreFrame) %>%
    select(varName, origName, code))

# We only want the rows with codes "clean" or "lev"
(newvars <- scoreFrame %>%
    filter(code %in% c("clean", "lev")) %>%
    use_series(varName))

# Create the treated training data
(dframe.treat <- prepare(treatplan, dframe, varRestriction = newvars))

# treatplan is in the workspace
summary(treatplan)

# newvars is in the workspace
newvars

# Print dframe and testframe
dframe
testframe


# The outcome column
(outcome <- "cnt")

# The input columns
(vars <- c("hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed"))

# Load the package vtreat
library(vtreat)
library(magrittr)
# The outcome column
(outcome <- "cnt")

# The input columns
(vars <- c("hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed"))

# Load the package vtreat
library(vtreat)

# Create the treatment plan from bikesJuly (the training data)
treatplan <- designTreatmentsZ(bikesJuly, vars, verbose = FALSE)

# Get the "clean" and "lev" variables from the scoreFrame
(newvars <- treatplan %>%
    use_series(scoreFrame) %>%               
    filter(code %in% c("clean", "lev")) %>%  # get the variables you care about
    use_series(varName))                     # get the varName column

# Prepare the training data
bikesJuly.treat <- prepare(treatplan, bikesJuly,  varRestriction = newvars)

# Prepare the test data
bikesAugust.treat <- prepare(treatplan, bikesAugust, varRestriction = newvars)

# Call str() on the treated data
str(bikesJuly.treat) 
str(bikesAugust.treat)


# The outcome column
(outcome <- "cnt")

# The input columns
(vars <- c("hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed"))

# Load the package vtreat
library(vtreat)
library(magrittr)

# Create the treatment plan from bikesJuly (the training data)
treatplan <- designTreatmentsZ(bikesJuly, vars, verbose = FALSE)

# Get the "clean" and "lev" variables from the scoreFrame
(newvars <- treatplan %>%
    use_series(scoreFrame) %>%               
    filter(code %in% c("clean", "lev")) %>%  # get the variables you care about
    use_series(varName))                     # get the varName column

# Prepare the training data
bikesJuly.treat <- prepare(treatplan, bikesJuly,  varRestriction = newvars)

# Prepare the test data
bikesAugust.treat <- prepare(treatplan, bikesAugust, varRestriction = newvars)

# Call str() on the treated data
str(bikesJuly.treat) 
str(bikesAugust.treat)


# The July data is in the workspace
ls()
head(bikesJuly.treat)
# Load the package xgboost
library(xgboost)

# Run xgb.cv
cv <- xgb.cv(data = as.matrix(bikesJuly.treat), 
             label = bikesJuly$cnt,
             nrounds = 100,
             nfold = 5,
             objective = "reg:linear",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0   # silent
)

# Get the evaluation log
elog <- cv$evaluation_log

# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))    # find the index of min(test_rmse_mean)


# Examine the workspace
ls()

# The number of trees to use, as determined by xgb.cv
ntrees

# Run xgboost
bike_model_xgb <- xgboost(data = as.matrix(bikesJuly.treat), # training data as matrix
                          label = bikesJuly$cnt,  # column of outcomes
                          nrounds = ntrees,       # number of trees to build
                          objective = "reg:linear", # objective
                          eta = 0.3,
                          depth = 6,
                          verbose = 0  # silent
)

# Make predictions
bikesAugust$pred <- predict(bike_model_xgb, as.matrix(bikesAugust.treat))

# Plot predictions vs actual bike rental count
ggplot(bikesAugust, aes(x = pred, y = cnt)) + 
  geom_point() + 
  geom_abline()

# bikesAugust is in the workspace
str(bikesAugust)

# Calculate RMSE
bikesAugust %>%
  mutate(residuals = cnt - pred) %>%
  summarize(rmse = sqrt(mean(residuals^2)))


# Print quasipoisson_plot
quasipoisson_plot

# Print randomforest_plot
randomforest_plot

# Plot predictions and actual bike rentals as a function of time (days)
bikesAugust %>% 
  mutate(instant = (instant - min(instant))/24) %>%  # set start to 0, convert unit to days
  gather(key = valuetype, value = value, cnt, pred) %>%
  filter(instant < 14) %>% # first two weeks
  ggplot(aes(x = instant, y = value, color = valuetype, linetype = valuetype)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Predicted August bike rentals, Gradient Boosting model")



gbm_model <- train(diagnosis ~ ., 
                   data = bc_train_data, 
                   method = "gbm", 
                   trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                   verbose = FALSE,
                   tuneLength = 4)

# Define hyperparameter grid.
hyperparams <- expand.grid(n.trees = 200, 
                           interaction.depth = 1, 
                           shrinkage = 0.1, 
                           n.minobsinnode = 10)

set.seed(42)
# Apply hyperparameter grid to train().
gbm_model <- train(diagnosis ~ ., 
                   data = bc_train_data, 
                   method = "gbm", 
                   trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                   verbose = FALSE,
                   tuneGrid = hyperparams)

man_grid <-  expand.grid(n.trees = c(100, 200, 250),
                         interaction.depth = c(1, 4, 6), 
                         shrinkage = 0.1,
                         n.minobsinnode = 10)
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5)
tic()
set.seed(42)
gbm_model_voters_grid <- train(turnout16_2016 ~ ., 
                               data = voters_train_data, 
                               method = "gbm", 
                               trControl = fitControl,
                               verbose = FALSE,
                               tuneGrid = man_grid)
toc()


# Train control with random search
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5,
                           search = "random")

Define trainControl function
fitControl <- trainControl(method = "adaptive_cv",
                           number = 3, repeats = 3)

# Load the pROC package
library(pROC)

# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)