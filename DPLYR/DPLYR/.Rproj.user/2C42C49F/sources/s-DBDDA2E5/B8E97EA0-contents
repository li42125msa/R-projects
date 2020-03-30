# Load the gapminder package
library(gapminder)

# Load the dplyr package
library(dplyr)

# Look at the gapminder dataset
gapminder

# Filter for China in 2002
gapminder %>%
  filter(country == "China", year == 2002)

gapminder %>%
  arrange(desc(lifeExp))

gapminder %>%
  filter(year == 1957) %>%
  arrange(desc(pop))

gapminder %>%
  filter(year == 2007) %>%
  mutate(lifeExpMonths = 12 * lifeExp) %>%
  arrange(desc(lifeExpMonths))

gapminder %>%
  filter(year == 1957) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

library(dslabs)
library(tidyverse)
glimpse(gapminder)
dim(gapminder)
# Explore gapminder
head(gapminder)

# Prepare the nested dataframe gap_nested
library(tidyverse)
gap_nested <- gapminder %>% 
  group_by(country) %>% 
  nest()
gap_nested 


# Create the unnested dataframe called gap_unnnested
gap_unnested <- gap_nested %>% 
  unnest()

# Confirm that your data was not modified  
identical(gapminder, gap_unnested)

# Extract the data of Algeria
algeria_df <- gap_nested$data[[1]]

# Calculate the minimum of the population vector
min(algeria_df$population,na.rm = T)

# Calculate the maximum of the population vector
max(algeria_df$population,na.rm = T)

# Calculate the mean of the population vector
mean(algeria_df$population,na.rm = T)


# Calculate the mean population for each country
pop_nested <- gap_nested %>%
  mutate(mean_pop = map(data, ~mean(.x$population)))

# Take a look at pop_nested
head(pop_nested)

# Extract the mean_pop value by using unnest
pop_mean <- pop_nested %>% 
  unnest(mean_pop)

# Take a look at pop_mean
head(pop_mean)

# Calculate mean population and store result as a double
pop_mean <- gap_nested %>%
  mutate(mean_pop = map_dbl(data, ~mean(.x$population)))

# Take a look at pop_mean
head(pop_mean)

# Build a linear model for each country
gap_models <- gap_nested %>%
  mutate(model = map(data, ~lm(formula = life_expectancy~year, data = .x)))

# Extract the model for Algeria    
algeria_model <- gap_models$model[[1]]

# View the summary for the Algeria model
summary(algeria_model)
