library(lubridate)
library(circular)



# Create vector predictions containing 0 for every transfer
predictions <- factor(rep.int(0, nrow(transfers)), levels = c(0, 1))
# factor(rep.int(0, 15), levels = c(0, 1))
# Compute confusion matrix
confusionMatrix(data = predictions, reference = transfers$fraud_flag)

# Compute cost of not detecting fraud
cost <- sum(transfers$amount[transfers$fraud_flag == 1])
print(cost)

# Convert the plain text to hours
ts <- as.numeric(hms(timestamps)) / 3600

# Estimate the periodic mean and concentration on the first 24 timestamps
p_mean <- estimates$mu %% 24
concentration <- estimates$kappa

# Estimate densities of all 25 timestamps
densities <- dvonmises(ts, mu = p_mean, kappa = concentration)

# Check if the densities are larger than the cutoff of 95%-CI
cutoff <- dvonmises(qvonmises((1 - alpha)/2, mu = p_mean, kappa = concentration), mu = p_mean, kappa = concentration)

# Define the variable time_feature
time_feature <- densities >= cutoff
print(cbind.data.frame(ts, time_feature))