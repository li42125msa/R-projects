
library(gapminder)
library(dplyr)
library(ggplot2)

# Summarize the median gdpPercap by year, then save it as by_year
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year, aes(x = year, y = medianGdpPercap)) +
  geom_line() +
  expand_limits(y = 0)

library(gapminder)
library(dplyr)
library(ggplot2)

# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent)) +
  geom_line() +
  expand_limits(y = 0)


# Print out head of economics
head(economics)

# Plot unemploy as a function of date using a line plot
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line()

# Adjust plot to represent the fraction of total population that is unemployed
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_line()

# Basic line plot
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_line()

recess
# Expand the following command with geom_rect() to draw the recess periods
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_rect(data = recess,
            aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.2) +
  geom_line()

# Check the structure as a starting point
str(fish.species)

# Use gather to go from fish.species to fish.tidy
fish.tidy <- gather(fish.species, Species, Capture, -Year)

# Recreate the plot shown on the right
ggplot(fish.tidy, aes(x = Year, y = Capture, color = Species)) +
  geom_line()


job_titles_by_perc
# A tibble: 16 x 2
CurrentJobTitleSelect                perc_w_title
<chr>                                       <dbl>
  1 Business Analyst                          0.0673 
2 Computer Scientist                        0.0283 
3 Data Analyst                              0.103  
4 Data Miner                                0.00997
5 Data Scientist                            0.206  
6 DBA/Database Engineer                     0.0158



ggplot(job_titles_by_perc, 
       aes(x = fct_rev(fct_reorder(CurrentJobTitleSelect, perc_w_title)),
           y = perc_w_title)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Job Title", y = "Percent with title") + 
  scale_y_continuous(labels = scales::percent_format())


ggplot(usefulness_by_platform, aes(x = learning_platform, y = avg_usefulness)) + 
  geom_point() + 
  # rotate x-axis text by 90 degrees
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  # rename y and x axis labels
  labs(x = "Learning Platform", y = "Percent finding at least somewhat useful") + 
  # change y axis scale to percentage
  scale_y_continuous(labels = scales::percent)