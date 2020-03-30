library(tidyverse)
library(readxl)
library(readr)
unemployment <- read_csv("unemployment.csv")


# unemployment is loaded in the workspace
summary(unemployment)

# Define a formula to express female_unemployment as a function of male_unemployment
fmla <- female_unemployment ~ male_unemployment

# Print it
fmla

# Use the formula to fit a model: unemployment_model
unemployment_model <-  lm(Y ~ X, data = unemployment)

library(sigr);library(broom)

# broom and sigr are already loaded in your workspace
# Print unemployment_model
unemployment_model

# Call summary() on unemployment_model to get more details
summary(unemployment_model)

# Call glance() on unemployment_model to see the details in a tidier form
glance(unemployment_model)

# Call wrapFTest() on unemployment_model to see the most relevant details
wrapFTest(unemployment_model)


# unemployment is in your workspace
summary(unemployment)

# newrates is in your workspace
newrates<- data.frame (X=6)

# Predict female unemployment in the unemployment data set
unemployment$prediction <-  predict(unemployment_model)

# load the ggplot2 package
library(ggplot2)

# Make a plot to compare predictions to actual (prediction on x axis)
ggplot(unemployment, aes(x = prediction, y = Y)) + 
  geom_point() +
  geom_abline(color = "red")

# Predict female unemployment rate when male unemployment is 5%
pred <- predict(unemployment_model, newdata = newrates)
# Print it
pred


# Load the package WVPlots
library(WVPlots)

# Plot the Gain Curve
GainCurvePlot(unemployment, "prediction", "Y", "Unemployment model")


# unemployment is in the workspace
summary(unemployment)

# For convenience put the residuals in the variable res
unemployment$res <- unemployment$prediction-unemployment$Y

# Calculate RMSE, assign it to the variable rmse and print it
(rmse <- sqrt(mean(res^2)))

# Calculate the standard deviation of female_unemployment and print it
sd_unemployment <- sd(unemployment$Y)




# unemployment is in your workspace
summary(unemployment)

# unemployment_model is in the workspace
summary(unemployment_model)

# Calculate mean female_unemployment: fe_mean. Print it
(Y_mean <- mean(unemployment$Y))

# Calculate total sum of squares: tss. Print it
(tss <- sum( (unemployment$Y - Y_mean)^2 ))

# Calculate residual sum of squares: rss. Print it
(rss <- sum(unemployment$res^2))

# Calculate R-squared: rsq. Print it. Is it a good fit?
(rsq <- 1 - (rss/tss))

# Get R-squared from glance. Print it
(rsq_glance <- glance(unemployment_model)$r.squared)


# Get the correlation between the prediction and true outcome: rho and print it
(rho <- cor(unemployment$prediction, unemployment$Y))

# Square rho: rho2 and print it
(rho2 <- rho ^ 2)

# Get R-squared from glance and print it
(rsq_glance <- glance(unemployment_model)$r.squared)

# mpg is in the workspace
summary(mpg)
dim(mpg)

# Use nrow to get the number of rows in mpg (N) and print it
(N <- nrow(mpg))

# Calculate how many rows 75% of N should be and print it
# Hint: use round() to get an integer
(target <- round(N * 0.75))

# Create the vector of N uniform random variables: gp
gp <- runif(N)

# Use gp to create the training set: mpg_train (75% of data) and mpg_test (25% of data)
mpg_train <- mpg[gp < 0.75, ]
mpg_test <- mpg[gp >= 0.75, ]

# Use nrow() to examine mpg_train and mpg_test
nrow(mpg_train)
nrow(mpg_test)


# mpg_train is in the workspace
summary(mpg_train)

# create a formula to express cty as a function of hwy: fmla and print it.
(fmla <- cty ~ hwy)

# Now use lm() to build a model mpg_model from mpg_train that predicts cty from hwy 
mpg_model <- lm(fmla, data = mpg_train)

# Use summary() to examine the model
summary(mpg_model)
  

# Examine the objects in the workspace
ls.str()

# predict cty from hwy for the training set
mpg_train$pred <- predict(mpg_model)

# predict cty from hwy for the test set
mpg_test$pred <- predict(mpg_model, newdata = mpg_test)

# Evaluate the rmse on both training and test data and print them
(rmse_train <- rmse(mpg_train$pred, mpg_train$cty))
(rmse_test <- rmse(mpg_test$pred, mpg_test$cty))


# Evaluate the r-squared on both training and test data.and print them
(rsq_train <- r_squared(mpg_train$pred, mpg_train$cty))
(rsq_test <- r_squared(mpg_test$pred, mpg_test$cty))

# Plot the predictions (on the x-axis) against the outcome (cty) on the test data
ggplot(mpg_test, aes(x = pred, y = cty)) + 
  geom_point() + 
  geom_abline()


# Load the package vtreat
library(vtreat)

# mpg is in the workspace
summary(mpg)

# Get the number of rows in mpg
nRows <- nrow(mpg)

# Implement the 3-fold cross-fold plan with vtreat
splitPlan <- kWayCrossValidation(nRows, 3, NULL, NULL)

# Examine the split plan
str(splitPlan)



# mpg is in the workspace
summary(mpg)

# splitPlan is in the workspace
str(splitPlan)

# Run the 3-fold cross validation plan from splitPlan
k <- 3 # Number of folds
mpg$pred.cv <- 0 
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(cty ~ hwy, data = mpg[split$train, ])
  mpg$pred.cv[split$app] <- predict(model, newdata = mpg[split$app, ])
}

# Predict from a full model
mpg$pred <- predict(lm(cty ~ hwy, data = mpg))

# Get the rmse of the full model's predictions
rmse(mpg$pred, mpg$cty)

# Get the rmse of the cross-validation predictions
rmse(mpg$pred.cv, mpg$cty)

library(Sleuth3)

# Call str on flowers to see the types of each column


try(data(package = "Sleuth3") )
str(flowers)

# Use unique() to see how many possible values Time takes
unique(flowers$Time)

# Build a formula to express Flowers as a function of Intensity and Time: fmla. Print it
(fmla <- as.formula("Flowers ~ Intensity + Time"))

# Use fmla and model.matrix to see how the data is represented for modeling
mmat <- model.matrix(fmla, flowers)

# Examine the first 20 lines of flowers
head(flowers, n = 20)

# Examine the first 20 lines of mmat
head(mmat, n = 20)

# flowers in is the workspace
str(flowers)

# fmla is in the workspace
fmla

# Fit a model to predict Flowers from Intensity and Time : flower_model
flower_model <-  lm(fmla, data = flowers)

# Use summary on mmat to remind yourself of its structure
summary(mmat)

# Use summary to examine the flower_model
summary(flower_model)

# predict the number of flowers on each plant
flowers$predictions <- predict(flower_model)

# Plot predictions vs actual flowers (predictions on x-axis)
ggplot(flowers, aes(x = predictions, y = Flowers)) + 
  geom_point() +
  geom_abline(color = "blue")


# alcohol is in the workspace
summary(alcohol)

# Create the formula with main effects only
(fmla_add <- Metabol ~ Gastric + Sex)

# Create the formula with interactions
(fmla_interaction <- Metabol ~  Gastric + Gastric:Sex)

# Fit the main effects only model
model_add <- lm(fmla_add, data = alcohol)

# Fit the interaction model
model_interaction <- lm(fmla_interaction, data = alcohol)

# Call summary on both models and compare
summary(model_add)
summary(model_interaction)



# alcohol is in the workspace
summary(alcohol)

# Both the formulae are in the workspace
fmla_add
fmla_interaction

# Create the splitting plan for 3-fold cross validation
library(dplyr);library(tidyr)
set.seed(34245)  # set the seed for reproducibility
splitPlan <- kWayCrossValidation(nrow(alcohol), 3, NULL, NULL)

# Sample code: Get cross-val predictions for main-effects only model
alcohol$pred_add <- 0  # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_add <- lm(fmla_add, data = alcohol[split$train, ])
  alcohol$pred_add[split$app] <- predict(model_add, newdata = alcohol[split$app, ])
}

# Get the cross-val predictions for the model with interactions
alcohol$pred_interaction <- 0 # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_interaction <- lm(fmla_interaction, data = alcohol[split$train, ])
  alcohol$pred_interaction[split$app] <- predict(model_interaction, newdata = alcohol[split$app, ])
}

# Get RMSE
alcohol %>% 
  gather(key = modeltype, value = pred, pred_add, pred_interaction) %>%
  mutate(residuals = Metabol - pred) %>%
  group_by(modeltype) %>%
  summarize(rmse = sqrt(mean(residuals^2)))

# fdata is in the workspace
summary(fdata)

# Examine the data: generate the summaries for the groups large and small:
fdata %>% 
  group_by(label) %>%        # group by small/large purchases
  summarize(min  = min(y),   # min of y
            mean = mean(y),  # mean of y
            max  = max(y))   # max of y

# Fill in the blanks to add error columns
fdata2 <- fdata %>% 
  group_by(label) %>%               # group by label
  mutate(residual = pred - y,     # Residual
         relerr   = residual/y)   # Relative error

# Compare the rmse and rmse.rel of the large and small groups:
fdata2 %>% 
  group_by(label) %>% 
  summarize(rmse     = sqrt(mean(residual^2)),  # RMSE
            rmse.rel = sqrt(mean(relerr^2)))    # Root mean squared relative error

# Plot the predictions for both groups of purchases
ggplot(fdata2, aes(x = pred, y = y, color = label)) + 
  geom_point() + 
  geom_abline() + 
  facet_wrap(~ label, ncol = 1, scales = "free") + 
  ggtitle("Outcome vs prediction")

load("Income.Rdata")

# Examine Income2005 in the training set
income_train
summary(income_train$Income2005)

# Write the formula for log income as a function of the tests and print it
(fmla.log <- log(Income2005) ~ Arith + Word + Parag + Math + AFQT)

# Fit the linear model
model.log <- lm(fmla.log, data = income_train)

# Make predictions on income_test
income_test$logpred <- predict(model.log, newdata = income_test)
summary(income_test$logpred)

# Convert the predictions to monetary units
income_test$pred.income <- exp(income_test$logpred)
summary(income_test$pred.income)

#  Plot predicted income (x axis) vs income
ggplot(income_test, aes(x = pred.income, y = Income2005)) + 
  geom_point() + 
  geom_abline(color = "blue")

# houseprice is in the workspace
summary(houseprice)

# Create the formula for price as a function of squared size
(fmla_sqr <- price ~ I(size^2))

# Fit a model of price as a function of squared size (use fmla_sqr)
model_sqr <- lm(fmla_sqr, houseprice)

# Fit a model of price as a linear function of size
model_lin <- lm(price ~ size, houseprice)

# Make predictions and compare
houseprice %>% 
  mutate(pred_lin = predict(model_lin),       # predictions from linear model
         pred_sqr = predict(model_sqr)) %>%   # predictions from quadratic model 
  gather(key = modeltype, value = pred, pred_lin, pred_sqr) %>% # gather the predictions
  ggplot(aes(x = size)) + 
  geom_point(aes(y = price)) +                  # actual prices
  geom_line(aes(y = pred, color = modeltype)) + # the predictions
  scale_color_brewer(palette = "Dark2")

# houseprice is in the workspace
summary(houseprice)

# fmla_sqr is in the workspace
fmla_sqr

# Create a splitting plan for 3-fold cross validation
set.seed(34245)  # set the seed for reproducibility
splitPlan <- kWayCrossValidation(nrow(houseprice), 3, NULL, NULL)

# Sample code: get cross-val predictions for price ~ size
houseprice$pred_lin <- 0  # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_lin <- lm(price ~ size, data = houseprice[split$train, ])
  houseprice$pred_lin[split$app] <- predict(model_lin, newdata = houseprice[split$app, ])
}

# Get cross-val predictions for price as a function of size^2 (use fmla_sqr)
houseprice$pred_sqr <- 0 # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_sqr <- lm(fmla_sqr, data = houseprice[split$train, ])
  houseprice$pred_sqr[split$app] <- predict(model_sqr, newdata = houseprice[split$app, ])
}

# Gather the predictions and calculate the residuals
houseprice_long <- houseprice %>%
  gather(key = modeltype, value = pred, pred_lin, pred_sqr) %>%
  mutate(residuals = pred - price)

# Compare the cross-validated RMSE for the two models
houseprice_long %>% 
  group_by(modeltype) %>%
  summarize(rmse = sqrt(mean(residuals^2)))