
library(gapminder)
library(dplyr)
library(ggplot2)

# Summarize the median gdpPercap by year and continent in 1952
by_continent <- gapminder %>%
  filter(year == 1952) %>%
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))
by_continent
# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(x = continent, y = medianGdpPercap)) +
  geom_col(fill="red",color="black")


cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))
# The base layer, cyl.am, is available for you
# Add geom (position = "stack" by default)
cyl.am + 
  geom_bar()

# Fill - show proportion
cyl.am + 
  geom_bar(position = "fill")  

# Dodging - principles of similarity and proximity
cyl.am +
  geom_bar(position = "dodge") 

# Clean up the axes with scale_ functions
val = c("#ff4949", "#4968ff")
lab = c("Manual", "Automatic")
cyl.am +
  geom_bar(position = "dodge") +
  scale_x_discrete("Cylinders") + 
  scale_y_continuous("Number") +
  scale_fill_manual("Transmission", 
                    values = val,
                    labels = lab) 










# Draw a bar plot of cyl, filled according to am
head(mtcars)
mtcars$am=factor(mtcars$am)
ggplot(mtcars, aes(x = cyl, fill = am) )+
  geom_bar()

# Change the position argument to "stack""
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "stack")

# Change the position argument to "fill""
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "fill")

# Change the position argument to "dodge""
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "dodge")

# 1 - The last plot form the previous exercise
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "dodge")

# 2 - Define posn_d with position_dodge()
posn_d <- position_dodge(0.2)

# 3 - Change the position argument to posn_d
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = posn_d)

# 4 - Use posn_d as position and adjust alpha to 0.6
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = posn_d, alpha = 0.6)  

# Plot of gender by align
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar()

# Plot proportion of gender, conditional on align
ggplot(comics, aes(x = align, fill = gender)) + 
  geom_bar(position = "fill") +
  ylab("proportion")


# Change the order of the levels in align
comics$align <- factor(comics$align, 
                       levels = c("Bad", "Neutral", "Good"))

# Create plot of align
ggplot(comics, aes(x = align)) + 
  geom_bar()

# Plot of alignment broken down by gender
ggplot(comics, aes(x = align)) + 
  geom_bar() +
  facet_wrap(~ gender)
