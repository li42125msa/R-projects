
# Convert cyl to factor (don't need to change)
mtcars$cyl <- as.factor(mtcars$cyl)

# Example from base R (don't need to change)
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
abline(lm(mpg ~ wt, data = mtcars), lty = 2)
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
})
legend(x = 5, y = 33, legend = levels(mtcars$cyl),
       col = 1:3, pch = 1, bty = "n")

# Plot 1: add geom_point() to this command to create a scatter plot
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point()

# Plot 2: include the lines of the linear models, per cyl
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Plot 3: include a lm for the entire dataset in its whole
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, linetype = 2)


p <- ggplot(mtcars, aes(wt, mpg,colour = factor(cyl), size = 16))



# Load the tidyr package
library(tidyr)
iris
# Fill in the ___ to produce to the correct iris.tidy dataset
iris.tidy <- iris %>%
  gather(key, Value, -Species) %>%
  separate(key, c("Part", "Measure"), "\\.")
str(iris.tidy)
iris.tidy
head(iris.tidy)
ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) +
  geom_jitter() +
  facet_grid(. ~ Measure)

ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) +
  geom_jitter() +
  facet_grid(. ~ Species)

# Load the tidyr package
library(tidyr)

# Add column with unique ids (don't need to change)
iris$Flower <- 1:nrow(iris)

# Fill in the ___ to produce to the correct iris.wide dataset
iris.wide <- iris %>%
  gather(key, value, -Species, -Flower) %>%
  separate(key, c("Part", "Measure"), "\\.") %>%
  spread(Measure, value)
iris.wide


# 1 - Map mpg to x and cyl to y
library(ggplot2)
ggplot(mtcars, aes(x = mpg, y = cyl)) +
  geom_point()
ggplot(mtcars, aes(x = mpg, y = cyl)) +
  geom_jitter()
# 2 - Reverse: Map cyl to x and mpg to y
ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_point()

# 3 - Map wt to x, mpg to y and cyl to col
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point()

# Change shape and size of the points in the above plot
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point(shape = 1, size = 4)


ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point(shape = 1, size = 4)

ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(shape = 1, size = 4)

ggplot(mtcars, aes(x = wt, y = mpg, col = factor(am) +
  geom_point(shape = 1, size = 4)
  # 1 - Map cyl to fill
  ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
    geom_point(shape = 1, size = 4)
  
  # 2 - Change shape and alpha of the points in the above plot
  ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
    geom_point(shape = 21, size = 4, alpha = 0.6)
  
  # 3 - Map am to col in the above plot
  ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl, col = am)) +
    geom_point(shape = 21, size = 4, alpha = 0.6)
  
  
  # Map cyl to size
  ggplot(mtcars, aes(x = wt, y = mpg, size = cyl)) +
    geom_point()
  
  # Map cyl to alpha
  ggplot(mtcars, aes(x = wt, y = mpg, alpha = cyl)) +
    geom_point()
  
  # Map cyl to shape 
  ggplot(mtcars, aes(x = wt, y = mpg, shape = cyl)) +
    geom_point()
  
  # Map cyl to labels
  ggplot(mtcars, aes(x = wt, y = mpg, label = cyl)) +
    geom_text()

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
    geom_point(size = 10, shape = 23, color = my_color)
  
  # Expand to draw points with alpha 0.5
  ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
    geom_point(alpha = 0.5)
  
  # Expand to draw points with shape 24 and color yellow
  ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
    geom_point(shape = 24, color = 'yellow')
  
  # Expand to draw text with label rownames(mtcars) and color red
  ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
    geom_text(label = rownames(mtcars), color = 'red')
  # Expand to draw points with alpha 0.5
  ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
    geom_point(alpha = 0.5)
  
  # Expand to draw points with shape 24 and color yellow
  ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
    geom_point(shape = 24, color = 'yellow')
  
  # Expand to draw text with label rownames(mtcars) and color red
  ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
    geom_text(label = rownames(mtcars), color = 'red')
  
  # Map mpg onto x, qsec onto y and factor(cyl) onto col (3 aesthetics):
  ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl))) +
    geom_point()
  
  # Add mapping: factor(am) onto shape (now 4 aesthetics):
  ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl), shape = factor(am))) +
    geom_point()
  
  # Add mapping: (hp/wt) onto size (now 5 aesthetics):
  ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl), shape = factor(am), size = (hp/wt))) +
    geom_point()
  # The base layer, cyl.am, is available for you
  # Add geom (position = "stack" by default)
  
  
  cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))
  cyl.am + 
    geom_bar()
  
  # Fill - show proportion
  cyl.am + 
    geom_bar(position = "fill")  
  
  # Dodging - principles of similarity and proximity
  cyl.am +
    geom_bar(position = "dodge") 
  
  # Clean up the axes with scale_ functions
  val = c("#E41A1C", "#377EB8")
  lab = c("Manual", "Automatic")
  cyl.am +
    geom_bar(position = "dodge") +
    scale_x_discrete("Cylinders") + 
    scale_y_continuous("Number") +
    scale_fill_manual("Transmission", 
                      values = val,
                      labels = lab)   
  
  # 1 - Create jittered plot of mtcars, mpg onto x, 0 onto y
  ggplot(mtcars, aes(x = mpg, y = 0)) +
    geom_jitter()
  
  # 2 - Add function to change y axis limits
  ggplot(mtcars, aes(x = mpg, y = 0)) +
    geom_jitter() +
    scale_y_continuous(limits = c(-2,2))
  
  # Basic scatter plot of wt on x-axis and mpg on y-axis; map cyl to col
  ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
    geom_point(size = 4)
  
  # Hollow circles - an improvement
  ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
    geom_point(size = 4, shape = 1)
  
  # Add transparency - very nice
  ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
    geom_point(size = 4, alpha = 0.6)
  
  # Scatter plot: carat (x), price (y), clarity (color)
  ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
    geom_point()
  
  # Adjust for overplotting
  ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
    geom_point(alpha = 0.5)
  
  # Scatter plot: clarity (x), carat (y), price (color)
  ggplot(diamonds, aes(x = clarity, y = carat, color = price)) +
    geom_point(alpha = 0.5)
  
  # Dot plot with jittering
  ggplot(diamonds, aes(x = clarity, y = carat, color = price)) +
    geom_point(alpha = 0.5, position = "jitter")

  # Shown in the viewer:
  ggplot(mtcars, aes(x = cyl, y = wt)) +
    geom_point()
  
  # Solutions:
  # 1 - With geom_jitter()
  ggplot(mtcars, aes(x = cyl, y = wt)) +
    geom_jitter()
  
  # 2 - Set width in geom_jitter()
  ggplot(mtcars, aes(x = cyl, y = wt)) +
    geom_jitter(width =.1)
  
  # 3 - Set position = position_jitter() in geom_point() ()
  ggplot(mtcars, aes(x = cyl, y = wt)) +
    geom_point(position = position_jitter(0.1))  

  
 
  