# A scatter plot has been made for you
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# Replace ___ with the correct column
ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point()

# Replace ___ with the correct column
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point()

# Add geom_point() and geom_smooth() with +
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth()

ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point(alpha = 0.1)+
  geom_smooth(se = FALSE)


library(tidyr)

# Fill in the ___ to produce to the correct iris.tidy dataset
(iris.tidy <- iris %>%
  gather(key, Value, -Species) %>%
  separate(key, c("Part", "Measure"), "\\."))

ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) +
  geom_jitter() +
  facet_grid(. ~ Measure)


# Define a hexadecimal color
my_color <- "#4ABEFF"

# Draw a scatter plot with color *aesthetic*
ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
  geom_point()

# Same, but set color *attribute* in geom layer 
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = my_color)

# Set the fill aesthetic; color, size and shape attributes
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(size = 4, shape = 23, color = my_color)


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

usefulness_by_platform %>%
  # reorder learning_platform by avg_usefulness
  mutate(learning_platform = fct_reorder(learning_platform, avg_usefulness)) %>%
  # reverse the order of learning_platform
  mutate(learning_platform = fct_rev(learning_platform)) %>%
  ggplot(aes(x = learning_platform, y = avg_usefulness)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Learning Platform", y = "Percent finding at least somewhat useful") + 
  scale_y_continuous(labels = scales::percent)


ggplot(mtcars, aes(disp, mpg)) + geom_point() + 
  labs(x = "x axis label", y = "y axis label", title = "My title",
       subtitle = "and a subtitle", caption = "even a caption!")


initial_plot <- rude_behaviors %>%
  # reorder response_var by perc_rude
  mutate(response_var = fct_reorder(response_var, perc_rude)) %>%
  # make a bar plot of perc_rude by response_var
  ggplot(aes(x = response_var, y = perc_rude)) + 
  geom_col() 

# View your plot
initial_plot

titled_plot <- initial_plot + 
  # Add the title, subtitle, and caption
  labs(title = "Hell Is Other People In A Pressurized Metal Tube",
       subtitle = "Percentage of 874 air-passenger respondents who said action is very or somewhat rude",
       caption = "Source: SurveyMonkey Audience", 
       # Remove the x- and y-axis labels
       x = "",
       y = "") 

titled_plot

flipped_plot <- titled_plot + 
  # Flip the axes
  coord_flip() + 
  # Remove the x-axis ticks and labels
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

flipped_plot + 
  # Add labels above the bar with the perc value
  geom_text(aes(label = percent(perc_rude), 
                y = perc_rude + .03), 
            position = position_dodge(0.9),
            vjust = 1)