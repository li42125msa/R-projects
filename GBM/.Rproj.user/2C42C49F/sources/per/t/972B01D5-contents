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

credit <- read_csv("credit.csv")

credit=credit %>% mutate_if(is.character, as.factor)

str(credit)

# Total number of rows in the credit data frame
n <- nrow(credit)

# Number of rows for the training set (80% of the dataset)
n_train <- round(0.8 * n) 

# Create a vector of indices which is an 80% random sample
set.seed(123)
train_indices <- sample(1:n, n_train)

# Subset the credit data frame to training indices only
credit_train <- credit[train_indices, ]  

# Exclude the training indices to create the test set
credit_test <- credit[-train_indices, ]
# Train the model (to predict 'default')


library(rpart)
credit_model_tree <- rpart(formula = default ~ ., 
                      data = credit_train, 
                      method = "class")

library(rpart.plot)
rpart.plot(x = credit_model_tree, yesno = 2, type = 0, extra = 0)
# yesno =2,2 write yes and no at all splits
#type =0 0 Default. Draw a split label at each split and a node label at each leaf
# extra = 0 0 Default. No extra information
# Look at the model output                      
print(credit_model_tree)

# Generate predicted classes using the model object
pred_tree <- predict(object = credit_model_tree,  
                            newdata = credit_test,  
                            type = "class")       
pred_tree1 <- predict(object = credit_model_tree,  
                     newdata = credit_test,  
                     type = "prob")       
library(caret)


# Calculate the confusion matrix for the test set
class(pred_tree)
class(credit_test$default)
CM_tree<-confusionMatrix(data = pred_tree,         
                reference = credit_test$default)
CM_tree

# split creiteria Impurity Measure - Gini Index
# Train a gini-based model
credit_model_tree_gini <- rpart(formula = default ~ ., 
                       data = credit_train, 
                       method = "class",
                       parms = list(split = "gini"))

# Train an information-based model
credit_model_tree_information <- rpart(formula = default ~ ., 
                       data = credit_train, 
                       method = "class",
                       parms = list(split = "information"))

# Generate predictions on the validation set using the gini model
pred_tree_gini <- predict(object = credit_model_tree_gini,
                 newdata = credit_test,
                 type = "class")  

pred_tree_gini1 <- predict(object = credit_model_tree_gini,
                          newdata = credit_test,
                          type = "prob")

# Generate predictions on the validation set using the information model
pred_tree_information <- predict(object = credit_model_tree_information, 
                 newdata = credit_test,
                 type = "class")
pred_tree_information1 <- predict(object = credit_model_tree_information, 
                                 newdata = credit_test,
                                 type = "prob")


# Compare classification error =1-accurary

library(Metrics)
ce(actual = credit_test$default, 
   predicted = pred_tree_gini)
ce(actual = credit_test$default, 
   predicted = pred_tree_information) 
ce(actual = credit_test$default, 
   predicted = pred_tree) 


confusionMatrix(data = pred_tree,         
                reference = credit_test$default) 
confusionMatrix(data = pred_tree_gini,         
                reference = credit_test$default) 

confusionMatrix(data = pred_tree_information,         
                reference = credit_test$default) 





auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
    predicted = pred_tree1[,"yes"]) 

auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
    predicted = pred_tree_gini1[,"yes"]) 

auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
    predicted = pred_tree_information1[,"yes"]) 

auc=auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
        predicted = pred_tree1[,"yes"]) 
auc_gini=auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
             predicted = pred_tree_gini1[,"yes"]) 

acu_information=auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
                    predicted = pred_tree_information1[,"yes"])

library(caTools)

