library(h2o)

h2o.init()

# Import a sample binary outcome train/test set into H2O
train <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_train_10k.csv")
test <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_test_5k.csv")

# Identify predictors and response
y <- "response"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

# Run AutoML for 20 base models (limited to 1 hour max runtime by default)
aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  max_models = 20,
                  seed = 1)

# View the AutoML Leaderboard
lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)

#                                               model_id       auc   logloss mean_per_class_error      rmse       mse
# 1     StackedEnsemble_AllModels_AutoML_20181210_150447 0.7895453 0.5516022            0.3250365 0.4323464 0.1869234
# 2  StackedEnsemble_BestOfFamily_AutoML_20181210_150447 0.7882530 0.5526024            0.3239841 0.4328491 0.1873584
# 3                     XGBoost_1_AutoML_20181210_150447 0.7846510 0.5575305            0.3254707 0.4349489 0.1891806
# 4        XGBoost_grid_1_AutoML_20181210_150447_model_4 0.7835232 0.5578542            0.3188188 0.4352486 0.1894413
# 5        XGBoost_grid_1_AutoML_20181210_150447_model_3 0.7830043 0.5596125            0.3250808 0.4357077 0.1898412
# 6                     XGBoost_2_AutoML_20181210_150447 0.7813603 0.5588797            0.3470738 0.4359074 0.1900153
# 7                     XGBoost_3_AutoML_20181210_150447 0.7808475 0.5595886            0.3307386 0.4361295 0.1902090
# 8                         GBM_5_AutoML_20181210_150447 0.7808366 0.5599029            0.3408479 0.4361915 0.1902630
# 9                         GBM_2_AutoML_20181210_150447 0.7800361 0.5598060            0.3399258 0.4364149 0.1904580
# 10                        GBM_1_AutoML_20181210_150447 0.7798274 0.5608570            0.3350957 0.4366159 0.1906335
# 11                        GBM_3_AutoML_20181210_150447 0.7786685 0.5617903            0.3255378 0.4371886 0.1911339
# 12       XGBoost_grid_1_AutoML_20181210_150447_model_2 0.7744105 0.5750165            0.3228112 0.4427003 0.1959836
# 13                        GBM_4_AutoML_20181210_150447 0.7714260 0.5697120            0.3374203 0.4410703 0.1945430
# 14           GBM_grid_1_AutoML_20181210_150447_model_1 0.7697524 0.5725826            0.3443314 0.4424524 0.1957641
# 15           GBM_grid_1_AutoML_20181210_150447_model_2 0.7543664 0.9185673            0.3558550 0.4966377 0.2466490
# 16                        DRF_1_AutoML_20181210_150447 0.7428924 0.5958832            0.3554027 0.4527742 0.2050045
# 17                        XRT_1_AutoML_20181210_150447 0.7420910 0.5993457            0.3565826 0.4531168 0.2053148
# 18  DeepLearning_grid_1_AutoML_20181210_150447_model_2 0.7388505 0.6012286            0.3695292 0.4555318 0.2075092
# 19       XGBoost_grid_1_AutoML_20181210_150447_model_1 0.7257836 0.6013126            0.3820490 0.4565541 0.2084417
# 20               DeepLearning_1_AutoML_20181210_150447 0.6979292 0.6339217            0.3979403 0.4692373 0.2201836
# 21  DeepLearning_grid_1_AutoML_20181210_150447_model_1 0.6847773 0.6694364            0.4081802 0.4799664 0.2303678
# 22           GLM_grid_1_AutoML_20181210_150447_model_1 0.6826481 0.6385205            0.3972341 0.4726827 0.2234290
#
# [22 rows x 6 columns]



# The leader model is stored here
aml@leader

# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly

pred <- h2o.predict(aml, test)  # predict(aml, test) also works

# or:
pred <- h2o.predict(aml@leader, test)