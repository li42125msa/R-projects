
# Fit a linear model on the breast_cancer_data.
linear_model <- lm(concavity_mean ~ symmetry_mean,
                   data = breast_cancer_data)

# Look at the summary of the linear_model.
summary(linear_model)

# Extract the coefficients.
linear_model$coefficients


library(ggplot2)

# Plot linear relationship.
ggplot(data = breast_cancer_data, 
       aes(x = symmetry_mean, y = concavity_mean)) +
  geom_point(color = "grey") +
  geom_abline(slope = linear_model$coefficients[2], 
              intercept = linear_model$coefficients[1])

# Fit a lm()
# mean >30 , can use normal instead of poisson
library(readr)
ChickWeightEnd <- read_csv("ChickWeightEnd.csv")



lm(formula = weight ~ Diet, data = ChickWeightEnd)

# Fit a glm()
glm(formula = weight ~ Diet, data = ChickWeightEnd, family = 'gaussian')

poissonOut <- glm(count ~ time, data = dat, family = 'poisson')

# Fit a glm() that estimates the difference between players
head(scores)
summary(glm(goal ~ player, data = scores, family = 'poisson'))

# Fit a glm() that estimates an intercept for each player 
summary(glm(goal ~ player - 1, data = scores, family = 'poisson'))

# build your models
lmOut <- lm(Number ~ Month, data = dat)
poissonOut <- glm(Number ~ Month, data = dat, family = 'poisson')

# examine the outputs using print
print(lmOut)
print(poissonOut)

# examine the outputs using summary
summary(lmOut)
summary(poissonOut)

# examine the outputs using tidy
library(broom)
tidy(lmOut)
tidy(poissonOut)

# Extract the regression coefficients
coef(poissonOut)

# Extract the confidence intervals 
confint(poissonOut)

# use the model to predict with new data 
predOut <- predict(object = poissonOut, newdata = newDat, type = "response")

# print the predictions
print(predOut)

