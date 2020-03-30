library(h2o)
h2o.init()

# import the auto dataset:
# this dataset looks at features of motor insurance policies and predicts the aggregate claim loss
# the original dataset can be found at https://cran.r-project.org/web/packages/HDtweedie/HDtweedie.pdf
auto <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/glm_test/auto.csv")

# set the predictor names and the response column name
predictors <- colnames(auto)[-1]
# The  response is aggregate claim loss (in $1000s)
response <- "y"

# split into train and validation sets
auto.splits <- h2o.splitFrame(data =  auto, ratios = .8)
train <- auto.splits[[1]]
valid <- auto.splits[[2]]

# try using the `tweedie_variance_power` parameter:
# train your model, where you specify tweedie_variance_power
auto_glm <- h2o.glm(x = predictors, y = response, training_frame = train,
                    validation_frame = valid,
                    family = 'tweedie',
                    tweedie_variance_power = 1)

# print the mse for validation set
print(h2o.mse(auto_glm, valid=TRUE))

# grid over `tweedie_variance_power`
# select the values for `tweedie_variance_power` to grid over
hyper_params <- list( tweedie_variance_power = c(0, 1, 1.1, 1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,
                                                 2.1, 2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3, 5, 7) )

# this example uses cartesian grid search because the search space is small
# and we want to see the performance of all models. For a larger search space use
# random grid search instead: {'strategy': "RandomDiscrete"}

# build grid search with previously selected hyperparameters
grid <- h2o.grid(x = predictors, y = response, training_frame = train, validation_frame = valid,
                 family = 'tweedie', algorithm = "glm", grid_id = "auto_grid", hyper_params = hyper_params,
                 search_criteria = list(strategy = "Cartesian"))

# Sort the grid models by mse
sortedGrid <- h2o.getGrid("auto_grid", sort_by = "mse", decreasing = FALSE)
sortedGrid

# print the mse for the validation data
print(h2o.mse(auto_glm, valid = TRUE))