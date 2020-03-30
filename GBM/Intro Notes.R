help(package="rpart")
library(caret): confusionMatrix()
library(Metrics)
  #ce( classificatino error),rmse

# auc: use prob, real numerical prediction and actual data has to be 1 and 0

#regression; method is anova


> auc_bag
[1] 0.7809724

auc_bag_caret
[1] 0.7762389

auc_bag
[1] 0.7809724
> auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
      +     predicted = pred_tree_gini1[,"yes"])
auc_randomForest
0.803

auc_randomForest_update1#(tuning based on oob err, not on test sample)
[1] 0.6214353

auc_GBM
.78

rint(paste0("Test set AUC (OOB): ", auc_GBM_OOB))                         
[1] "Test set AUC (OOB): 0.78184899485741"
> print(paste0("Test set AUC (CV): ", auc_GBM_CV ))
[1] "Test set AUC (CV): 0.785764375876578"

sprintf("Decision Tree Test AUC: %.3f", dt_auc)
[1] "Decision Tree Test AUC: 0.627"
> sprintf("Bagged Trees Test AUC: %.3f", bag_auc)
[1] "Bagged Trees Test AUC: 0.781"
> sprintf("Random Forest Test AUC: %.3f", rf_auc)
[1] "Random Forest Test AUC: 0.804"
> sprintf("GBM Test AUC: %.3f", gbm_auc)
[1] "GBM Test AUC: 0.786"