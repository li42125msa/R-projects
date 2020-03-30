library(gapminder)
library(dplyr)
library(ggplot2)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a histogram of population (pop)
ggplot(gapminder_1952, aes(x = pop)) +
  geom_histogram() +
  scale_x_log10()


# 1 - Make a univariate histogram
ggplot(mtcars, aes(mpg)) +
  geom_histogram()

# 2 - Plot 1, plus set binwidth to 1 in the geom layer
ggplot(mtcars, aes(mpg)) +
  geom_histogram(binwidth = 1)

# 3 - Plot 2, plus MAP ..density.. to the y aesthetic (i.e. in a second aes() function)
ggplot(mtcars, aes(mpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1)

# 4 - plot 3, plus SET the fill attribute to "#377EB8"
ggplot(mtcars, aes(mpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#377EB8")


# A basic histogram, add coloring defined by cyl
ggplot(mtcars, aes(mpg, fill = cyl)) +
  geom_histogram(binwidth = 5)

# Change position to identity
ggplot(mtcars, aes(mpg, fill = cyl)) +
  geom_histogram(binwidth = 1, position = "identity")

# Change geom to freqpoly (position is identity by default)
ggplot(mtcars, aes(mpg, color = cyl)) +
  geom_freqpoly(binwidth = 1)

# 4 - plot 3, plus SET the fill attribute to "#377EB8"
ggplot(mtcars, aes(mpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#ff4949")


# Check the levels of gender
levels(comics$gender)

# Create a 2-way contingency table
table(comics$align, comics$gender)

# Load dplyr
library(dplyr)

# Print tab
tab

# Remove align level
comics_filtered <- comics %>%
  filter(align != "Reformed Criminals") %>%
  droplevels()


# Create side-by-side barchart of alignment by gender
ggplot(comics, aes(x = gender, fill = align)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))


# Load package
library(ggplot2)

# Learn data structure
str(cars)

# Create faceted histogram
ggplot(cars, aes(x = city_mpg)) +
  geom_histogram() +
  facet_wrap(~ suv)


# Filter cars with 4, 6, 8 cylinders
common_cyl <- filter(cars, ncyl %in% c(4, 6, 8))

# Create box plots of city mpg by ncyl
ggplot(common_cyl, aes(x = as.factor(ncyl), y = city_mpg)) +
  geom_boxplot()

# Create overlaid density plots for same data
ggplot(common_cyl, aes(x = city_mpg, fill = as.factor(ncyl))) +
  geom_density(alpha = .3)

# Create hist of horsepwr for affordable cars
cars %>%
  filter(msrp < 25000) %>%
  ggplot(aes(x = horsepwr)) +
  geom_histogram() +
  xlim(c(90, 550)) +
  ggtitle("Distribution of horsepower for affordable cars")


# Create hist of horsepwr with binwidth of 30
cars %>% 
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 30) +
  ggtitle("binwidth = 30")

# Facet hists using hwy mileage and ncyl
common_cyl %>% 
  ggplot(aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_grid(ncyl ~ suv,labeller=label_both) +
  ggtitle("Mileage by suv and ncyl")


# Compute groupwise measures of spread
gap2007 %>%
  group_by(continent) %>%
  summarize(sd(lifeExp),
            IQR(lifeExp),
            n())

# Generate overlaid density plots
gap2007 %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.3)