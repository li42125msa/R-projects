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


library(ipred);library(caret);library(Metrics)
# Bagging is a randomized model, so let's set a seed for reproducibility
set.seed(123)
library(ipred)
# Train a bagged model nbag, coob
credit_model_bag <- bagging(formula = default ~ ., 
                        data = credit_train,
                        nbag=50,
                        coob = TRUE)

# If we want to estimate the model's accuracy using the "out-of-bag" (OOB) samples, we can set the the coob parameter to TRUE
# Print the model
print(credit_model_bag)
# Generate predicted classes using the model object
pred_bag <- predict(object = credit_model_bag, 
                            newdata = credit_test,  
                            type = "class")         # return classification labels
# Print the predicted classes
print(pred_bag)

# Calculate the confusion matrix for the test set
CM_bag=confusionMatrix(data = pred_bag,         
                reference = as.factor(credit_test$default) )

CM_bag
# Generate predictions on the test set
pred_bag1 <- predict(object = credit_model_bag, 
                newdata = credit_test,
                type = "prob")

# `pred` is a matrix
class(pred_bag1)

# Look at the pred format
head(pred_bag1)                

# Compute the AUC (`actual` must be a binary vector)
auc_bag<-auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
    predicted = pred_bag1[,"yes"])
auc_bag

library(caret)
# Specify the training configuration
ctrl <- trainControl(method = "cv",     # Cross-validation
                     number = 5,        # 5 folds
                     classProbs = TRUE,                  # For AUC
                     summaryFunction = twoClassSummary)  # For AUC

# Cross validate the credit model using "treebag" method; 
# Track AUC (Area under the ROC curve)
set.seed(1)  # for reproducibility
credit_model_caret <- train(default ~ ., 
                            data = credit_train, 
                            method = "treebag",
                            metric = "ROC",
                            trControl = ctrl)

# Look at the model object
print(credit_model_caret )

# Inspect the contents of the model list 
names(credit_model_caret )

# Print the CV AUC
credit_model_caret$results[,"ROC"]

# Generate predictions on the test set
pred_bag_caret <- predict(object = credit_model_caret , 
                newdata = credit_test,
                type = "prob")

pred_bag <- predict(object = credit_caret_model, 
                newdata = credit_test,
                type = "prob")
# Compute the AUC (`actual` must be a binary (or 1/0 numeric) vector)
auc_bag_caret<-auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
    predicted = pred_bag_caret [,"yes"])
auc_bag_caret
# Print ipred::bagging test set AUC estimate
print(auc_bag)

# Print caret "treebag" test set AUC estimate
print(auc_bag_caret)

# Compare to caret 5-fold cross-validated AUC
credit_caret_model$results[,"ROC"]
