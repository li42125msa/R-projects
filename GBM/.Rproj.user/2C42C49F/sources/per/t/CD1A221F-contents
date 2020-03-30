# Load packages
library(moderndive)
library(ggplot2)
library(dplyr)
data("evals")

glimpse(evals)

# Plot the histogram
ggplot(evals, aes(x = score)) +
  geom_histogram(binwidth = .1) +
  labs(x = "score", y = "count")

# Load packages
library(moderndive)
library(dplyr)

# Compute summary stats
evals %>%
  summarize(mean_age = mean(age),
            median_age = median(age),
            sd_age = sd(age))
# Add log10_size
house_prices_2 <- house_prices %>%
  mutate(log10_size = log10(sqft_living))

evals %>% 
  summarize(correlation = cor(score, age))


library(ggplot2)
library(dplyr)
library(moderndive)

# Apply log10-transformation to outcome variable
house_prices <- house_prices %>%
  mutate(log10_price = log10(price))
# Boxplot
ggplot(house_prices, aes(x = condition, y = log10_price)) +
  geom_boxplot() +
  labs(x = "house condition", y = "log10 price", 
       title = "log10 house price over condition")

# Calculate stats
house_prices %>%
  group_by(waterfront) %>%
  summarize(mean_log10_price = mean(log10_price), n = n())


# Plot
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "beauty score", y = "score") +
  geom_smooth(method = "lm", se = FALSE)

# Fit model
model_score_2 <- lm(score ~ bty_avg, data = evals)

# Output content
model_score_2

get_regression_table(model_score_2)

# Fit regression model
model_score_2 <- lm(score ~ bty_avg, data = evals)

# Get regression table
get_regression_table(model_score_2)

# Get all fitted/predicted values and residuals
get_regression_points(model_score_2)

library(ggplot2)
library(dplyr)
library(moderndive)

ggplot(evals, aes(x = score)) +
  geom_histogram(binwidth = 0.25) +
  facet_wrap(~gender) +
  labs(x = "score", y = "count")


ggplot(evals, aes(x = rank, y = score)) +
  geom_boxplot() +
  labs(x = "rank", y = "score")
