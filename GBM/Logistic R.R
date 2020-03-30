
library(broom)
# Build a glm that models Bus predicted by CommuteDays
# using data.frame bus. Remember to use a binomial family.
busOut <- glm(Bus ~ CommuteDays, data= bus, family = 'binomial')

# Print the busOut (be sure to use the print function)
print(busOut)

# Look at the summary of busOut
summary(busOut)

# Look at the tidy output of busOut
tidy(busOut)

# Simulate 1 draw with a sample size of 100
binomialSim <- rbinom(n = 1, size = 100, prob = 0.5)

# Simulate 100 draw with a sample size of 1 
BernoulliSim <- rbinom(n = 100, size = 1, prob = 0.5)

# Print the results from the binomial
print(binomialSim)

# Sum the results from the Bernoulli
sum(BernoulliSim)

# Long-form logistic regression input


# Wide-form input logistic regression

# x fail success Total successProportion
# 1 a   12       2    14         0.1428571
# 2 b    3      11    14         0.7857143
cbind(success, fail) ~ predictor
proportion of successes ~ response variable is used with weights = number in treatment.

# Fit a wide form logistic regression
lr_2 <- glm(cbind(success, fail) ~ x, family = 'binomial', data = dataWide)

# Using dataWide, fit a glm with successProportion predicted by x and weights = Total
lr_3 <- glm(successProportion ~ x, family = 'binomial', data = dataWide, weights = Total)

# Fit a GLM with a logit link and save it as busLogit
busLogit <- glm(Bus ~ CommuteDays, data = bus, family = binomial(link = "logit"))

# Fit a GLM with probit link and save it as busProbit
busProbit <- glm(Bus ~ CommuteDays, data = bus, family = binomial(link = "probit"))

# Print model summaries
summary(busLogit)
summary(busProbit)


# Use geom_smooth to plot a continuous predictor variable
ggplot(data = dat, aes(x = dose, y = cells)) + 
  geom_jitter(width = 0.05, height = 0.05) + 
  geom_smooth(method = 'glm', method.args = list(family = 'poisson'))

# Extract out the coefficients 
coefOut <- coef(busOut)

# Convert the coefficients to odds-ratios 
exp(coefOut)

library(broom)
# use tidy on busOut and exponentiate the results and extract the confidence interval
tidy(busOut, exponentiate = TRUE, conf.int= TRUE)


# add in the missing parts of the ggplot
ggJitter <- ggplot(data = bus, aes(x = MilesOneWay, y = Bus2)) + 
  geom_jitter(width = 0, height = 0.05)

# add in geom_smooth()
ggJitter + geom_smooth()

# add in the missing parts of the ggplot
ggJitter + geom_smooth(method = 'glm', method.args = list(family = 'binomial'))

# add in the missing parts of the ggplot
ggJitter + 
  geom_smooth(method = 'glm', 
              method.args = list(family = binomial(link = 'probit')), 
              color = 'red', se = FALSE) +
  geom_smooth(method = 'glm', 
              method.args = list(family = binomial(link = 'logit')), 
              color = 'blue', se = FALSE)

# Build a logistic regression with Bus predicted by CommuteDays and MilesOneWay
busBoth <- glm(Bus ~ CommuteDays + MilesOneWay, data = bus, family = 'binomial')

# Look at the summary of the output
summary(busBoth)

# Run a correlation
cor(bus$CommuteDays, bus$MilesOneWay)


# supervised regression sparrow is in the workspace
summary(sparrow)

# Create the survived column
sparrow$survived <- sparrow$status == "Survived"

# Create the formula
(fmla <- survived ~ total_length + weight + humerus)

# Fit the logistic regression model
sparrow_model <- glm(fmla, data = sparrow, family = binomial)

# Call summary
summary(sparrow_model)

# Call glance
(perf <- glance(sparrow_model))

# Calculate pseudo-R-squared
(pseudoR2 <- 1 - perf$deviance/perf$null.deviance)


# sparrow is in the workspace
summary(sparrow)

# sparrow_model is in the workspace
summary(sparrow_model)

# Make predictions
sparrow$pred <- predict(sparrow_model, type = "response")


# Load the package WVPlots
library(WVPlots)

# Look at gain curve
GainCurvePlot(sparrow, "pred", "survived", "sparrow survival model")

# bikesJuly is in the workspace
str(bikesJuly)

# The outcome column
outcome 

# The inputs to use
vars 

# Create the formula string for bikes rented as a function of the inputs
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))

# Calculate the mean and variance of the outcome
(mean_bikes <- mean(bikesJuly$cnt))
(var_bikes <- var(bikesJuly$cnt))

# Fit the model
bike_model <- glm(fmla, data = bikesJuly, family = quasipoisson)

# Call glance
(perf <- glance(bike_model))

# Calculate pseudo-R-squared
(pseudoR2 <- 1 - perf$deviance/perf$null.deviance)


# bikesAugust is in the workspace
str(bikesAugust)

# bike_model is in the workspace
summary(bike_model)

# Make predictions on August data
bikesAugust$pred <- predict(bike_model, newdata = bikesAugust, type = "response")

# Calculate the RMSE
bikesAugust %>% 
  mutate(residual = pred - cnt) %>%
  summarize(rmse  = sqrt(mean(residual^2)))

# Plot predictions vs cnt (pred on x-axis)
ggplot(bikesAugust, aes(x = pred, y = cnt)) +
  geom_point() + 
  geom_abline(color = "darkblue")

# Plot predictions and cnt by date/time
bikesAugust %>% 
  # set start to 0, convert unit to days
  mutate(instant = (instant - min(instant))/24) %>%  
  # gather cnt and pred into a value column
  gather(key = valuetype, value = value, cnt, pred) %>%
  filter(instant < 14) %>% # restrict to first 14 days
  # plot value by instant
  ggplot(aes(x = instant, y = value, color = valuetype, linetype = valuetype)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Predicted August bike rentals, Quasipoisson model")


# soybean_train is in the workspace
summary(soybean_train)

# Plot weight vs Time (Time on x axis)
ggplot(soybean_train, aes(x = Time, y = weight)) + 
  geom_point() 


# soybean_test is in the workspace
summary(soybean_test)

# Get predictions from linear model
soybean_test$pred.lin <- predict(model.lin, newdata = soybean_test)

# Get predictions from gam model
soybean_test$pred.gam <- as.numeric(predict(model.gam, newdata = soybean_test))

# Gather the predictions into a "long" dataset
soybean_long <- soybean_test %>%
  gather(key = modeltype, value = pred, pred.lin, pred.gam)

# Calculate the rmse
soybean_long %>%
  mutate(residual = weight - pred) %>%     # residuals
  group_by(modeltype) %>%                  # group by modeltype
  summarize(rmse = sqrt(mean(residual^2))) # calculate the RMSE

# Compare the predictions against actual weights on the test data
soybean_long %>%
  ggplot(aes(x = Time)) +                          # the column for the x axis
  geom_point(aes(y = weight)) +                    # the y-column for the scatterplot
  geom_point(aes(y = pred, color = modeltype)) +   # the y-column for the point-and-line plot
  geom_line(aes(y = pred, color = modeltype, linetype = modeltype)) + # the y-column for the point-and-line plot
  scale_color_brewer(palette = "Dark2")


###############################machine learning toolbox

library(mlbench)
library(caret)
data("Sonar")

# Get the number of observations
n_obs <- nrow(Sonar)

# Shuffle row indices: permuted_rows
permuted_rows <- sample(n_obs)

# Randomly order data: Sonar
Sonar_shuffled <- Sonar[permuted_rows, ]

# Identify row to split on: split
split <- round(n_obs * 0.6)

# Create train
train <- Sonar_shuffled[1:split, ]

# Create test
test <- Sonar_shuffled[(split + 1):n_obs, ]

# Fit glm model: model  Class has to be 1 and 0
train$Class1=ifelse(train$Class=="R",1,0)
model <- glm(Class1 ~ ., family = "binomial", train)
model_b <- glm(Class1 ~ ., family = binomial(link="logit"), train)
# Predict on test: p
p <- predict(model, test, type = "response")

# If p exceeds threshold of 0.5, M else R: m_or_r
m_or_r <- ifelse(p > 0.5, "R", "M")

# Convert to factor: p_class
p_class <- factor(m_or_r, levels = levels(test[["Class"]]))

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])

library(caTools)

# Predict on test: p
p <- predict(model, test, type = "response")

# Make ROC curve
colAUC(p, test[["Class"]], plotROC = TRUE)

# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train glm with custom trainControl: model
model <- train(
  Class ~ ., 
  Sonar, 
  method = "glm",
  metric = "ROC",
  trControl = myControl
)

model

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
#################supervised learning in R classification
str(donors)
table(donors$donated)
# Estimate the donation probability
donors$donation_prob <- predict(donation_model, type = "response")

# Find the donation probability of the average prospect
mean(donors$donated)

# Predict a donation if probability of donation is greater than average
donors$donation_pred <- ifelse(donors$donation_prob > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donated == donors$donation_pred)

# Load the pROC package
library(pROC)

# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)

# Convert the wealth rating to a factor
donors$wealth_rating <- factor(donors$wealth_rating, levels = c(0, 1, 2, 3), labels = c("Unknown", "Low", "Medium", "High"))

# Use relevel() to change reference category
donors$wealth_rating <- relevel(donors$wealth_rating, ref = "Medium")

# See how our factor coding impacts the model
summary(glm(donated ~ wealth_rating, data = donors, family = "binomial"))

# Find the average age among non-missing values
summary(donors$age)

# Impute missing age values with the mean age
donors$imputed_age <- ifelse(is.na(donors$age), round(mean(donors$age, na.rm = TRUE), 2), donors$age)

# Create missing value indicator for age
donors$missing_age <- ifelse(is.na(donors$age), 1, 0)

# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = donors, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = donors, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# Estimate the stepwise donation probability
step_prob <- predict(step_model, type = "response")

library(pROC)


# Estimate the donation probability
donors$donation_prob <- predict(donation_model, type = "response")
# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)

# Specify the training configuration
ctrl <- trainControl(method = "cv",     # Cross-validation
                     number = 5,        # 5 folds
                     classProbs = TRUE,                  # For AUC
                     summaryFunction = twoClassSummary)  # For AUC

# Cross validate the credit model using "treebag" method; 
# Track AUC (Area under the ROC curve)
set.seed(1)  # for reproducibility
credit_caret_model <- train(default ~ ., 
                            data = credit_train, 
                            method = "treebag",
                            metric = "ROC",
                            trControl = ctrl)

# Look at the model object
print(credit_caret_model)

# Inspect the contents of the model list 
names(credit_caret_model)

# Print the CV AUC
credit_caret_model$results[,"ROC"]

pred <- predict(object = credit_caret_model, 
                newdata = credit_test,
                type = "prob")

# Compute the AUC (`actual` must be a binary (or 1/0 numeric) vector)
auc(actual = ifelse(credit_test$default == "yes", 1, 0), 
    predicted = pred[,"yes"])
