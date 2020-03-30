# You probably either did ratio(3, 4), which relies on matching by position, or ratio(x = 3, y = 4), which relies on matching by name.

f <- function(x) {
  if (TRUE) {
    return(x + 1)
  }
  x
}
f(2)

y <- 10
f <- function(x) {
  x + y
}
f(10)
typeof(y)
class(y)
# That is, my_list[[1]] extracts the first element of the list my_list, and my_list[["name"]] extracts the element in my_list that is called name. If the list is nested you can travel down the hierarchy by recursive subsetting. For example, mylist[[1]][["name"]] is the element called name inside the first element of my_list.

# 2nd element in tricky_list
typeof(tricky_list[[2]])

# Element called x in tricky_list
typeof(tricky_list[["x"]])

# 2nd element inside the element called x in tricky_list
typeof(tricky_list[["x"]][[2]])

# Guess where the regression model is stored
names(tricky_list)

# Use names() and str() on the model element
names(tricky_list[["model"]])
str(tricky_list[["model"]])

# Subset the coefficients element
tricky_list[["model"]][["coefficients"]]

# Subset the wt element
tricky_list[["model"]][["coefficients"]][["wt"]]

# Replace the 1:ncol(df) sequence
for (i in seq_along(df)) {
  print(median(df[[i]]))
}

# Create an empty data frame
empty_df <- data.frame()

# Repeat for loop to verify there is no error
for (i in seq_along(empty_df)) {
  print(median(empty_df[[i]]))
}

# Create new double vector: output
output <- vector("double", ncol(df))

# Alter the loop
for (i in seq_along(df)) {
  # Change code to store result in output
  output[i] <- median(df[[i]])
}

# Print output
output

# Rename the function f() to replace_missings()
replace_missings <- function(x, replacement) {
  # Change the name of the y argument to replacement
  x[is.na(x)] <- replacement
  cat(sum(is.na(x)), replacement, "\n")
  x
}

# Rewrite the call on df$z to match our new names
df$z <- replace_missings(df$z, replacement = 0)

# Turn this code into col_median()
col_median <- function(df) {
  output <- numeric(ncol(df))
  for (i in seq_along(df)) {
    output[[i]] <- median(df[[i]])
  }
  output
}

# All the map functions in purrr take a vector, .x, as the first argument, then return .f applied to each element of .x. The type of object that is returned is determined by function suffix (the part after _):
#   
#   map() returns a list or data frame
# map_lgl() returns a logical vector
# map_int() returns a integer vector
# map_dbl() returns a double vector
# map_chr() returns a character vector


# Load the purrr package
library(purrr)

# Use map_dbl() to find column means
map_dbl(df, mean)

# Use map_dbl() to column medians
map_dbl(df, median)

# Use map_dbl() to find column standard deviations
map_dbl(df, sd)

# Find the mean of each column
map_dbl(planes, mean)

# Find the mean of each column, excluding missing values
map_dbl(planes, mean, na.rm = TRUE)

# Find the 5th percentile of each column, excluding missing values
map_dbl(planes, quantile, probs = c(0.05), na.rm = TRUE)

# Find the columns that are numeric
map_lgl(df3, is.numeric)

# Find the type of each column
map_chr(df3, typeof)

# Find a summary of each column
map(df3, summary)


# Our goal is to fit a separate linear regression of miles per gallon (mpg) against weight (wt) for each group of cars in our list of data frames, where each data frame in our list represents a different group. How should we get started?
cyl<- split(mtcars,mtcars$cyl)
str(cyl)

# Extract the first element into four_cyls
four_cyls <- cyl[[1]]

# Fit a linear regression of mpg on wt using four_cyls
lm(mpg ~ wt, data = four_cyls)


fit_reg <- function(df) {
  lm(mpg ~ wt, data = df)
}

# Rewrite to call an anonymous function
map(cyl, function(df) lm(mpg ~ wt, data = df))

map_dbl(cyl, function(df) mean(df$disp))
map_dbl(cyl, ~ mean(.$disp))

map(cyl, function(df) lm(mpg ~ wt, data = df))
map(cyl, ~ lm(mpg ~ wt, data = .))

# Create a list n containing the values: 5, 10, and 20
n <- list(5, 10, 20)

# Call map() on n with rnorm() to simulate three samples
map(n, rnorm)


list_of_results <- list(
  list(a = 1, b = "A"), 
  list(a = 2, b = "C"), 
  list(a = 3, b = "D")
)

# We might want to pull out the a element from every entry. We could do it with the string shortcut like this:

map(list_of_results, "a")
library(purrr)
# Save the result from the previous exercise to the variable models
models <- purrr::map(cyl, ~ lm(mpg ~ wt, data = .))

# Use map and coef to get the coefficients for each model: coefs
coefs <- map(models, coef)

# Use string shortcut to extract the wt coefficient 
map(coefs, "wt")
# use map_dbl with the numeric shortcut to pull out the second element
map_dbl(coefs, 2)

# Define models (don't change)
models <- mtcars %>% 
  split(mtcars$cyl) %>%
  map(~ lm(mpg ~ wt, data = .))

# Rewrite to be a single command using pipes 
library(purrr)
models %>% 
  map(summary) %>%
  map_dbl("r.squared")

# Mapping over two arguments
# Initialize n
library(purrr)
n <- list(5, 10, 20)

# Create a list mu containing the values: 1, 5, and 10
mu <- list(1, 5, 10)

# Edit to call map2() on n and mu with rnorm() to simulate three samples
map2(n, mu, rnorm)

# Mapping over more than two arguments
# Initialize n and mu
n <- list(5, 10, 20)
mu <- list(1, 5, 10)

# Create a sd list with the values: 0.1, 1 and 0.1
sd <- list(0.1, 1, 0.1)

# Edit this call to pmap() to iterate over the sd list as well
pmap(list(n, mu, sd), rnorm)

# Name the elements of the argument list
pmap(list(mean = mu, n = n, sd = sd), rnorm)

# Mapping over functions and their arguments

# Define list of functions
funs <- list("rnorm", "runif", "rexp")

# Parameter list for rnorm()
rnorm_params <- list(mean = 10)

# Add a min element with value 0 and max element with value 5
runif_params <- list(min = 0, max = 5)

# Add a rate element with value 5
rexp_params <- list(rate = 5)

# Define params for each function
params <- list(
  rnorm_params,
  runif_params,
  rexp_params
)

# Call invoke_map() on funs supplying params and setting n to 5
invoke_map(funs, params, n = 5)


d <- map(files, read_csv)
map(survey_data, ~nrow(.x))
map_dbl(survey_data, ~nrow(.x))

list_of_df <- map(list_of_df, as.numeric)


library(purrr)
# Create a list n containing the values: 5, 10, and 20
n <- list(5, 10, 20)

# Call map() on n with rnorm() to simulate three samples
map(n, rnorm)

# Initialize n
n <- list(5, 10, 20)

# Create a list mu containing the values: 1, 5, and 10
mu <- list(1, 5, 10)

# Edit to call map2() on n and mu with rnorm() to simulate three samples
map2(n, mu, rnorm)
pmap(list(n,mu),rnorm)
# Initialize n and mu
n <- list(5, 10, 20)
mu <- list(1, 5, 10)

# Create a sd list with the values: 0.1, 1 and 0.1
sd <- list(0.1, 1, 0.1)

# Edit this call to pmap() to iterate over the sd list as well
pmap(list(n, mu, sd), rnorm)


pmap(list(mean = mu, n = n, sd = sd), rnorm)


# Define list of functions
funs <- list("rnorm", "runif", "rexp")

# Parameter list for rnorm()
rnorm_params <- list(mean = 10)

# Add a min element with value 0 and max element with value 5
runif_params <- list(min = 0, max = 5)

# Add a rate element with value 5
rexp_params <- list(rate = 5)

# Define params for each function
params <- list(
  rnorm_params,
  runif_params,
  rexp_params
)

# Call invoke_map() on funs supplying params and setting n to 5
invoke_map(funs, params, n = 5)


# Define list of functions
funs <- list(Normal = "rnorm", Uniform = "runif", Exp = "rexp")

# Define params
params <- list(
  Normal = list(mean = 10),
  Uniform = list(min = 0, max = 5),
  Exp = list(rate = 5)
)

# Assign the simulated samples to sims
sims <- invoke_map(funs, params, n = 5000)
sims

# Use walk() to make a histogram of each element in sims
walk(sims, hist)


# Replace "Sturges" with reasonable breaks for each sample
breaks_list <- list(
  Normal = seq(6, 16, 0.5),
  Uniform = seq(0, 5, 0.25),
  Exp = seq(0, 1.5, 0.1)
)

# Use walk2() to make histograms with the right breaks
walk2(sims, breaks_list, hist)

# Replace "Sturges" with reasonable breaks for each sample
breaks_list <- list(
  Normal = seq(6, 16, 0.5),
  Uniform = seq(0, 5, 0.25),
  Exp = seq(0, 1.5, 0.1)
)

# Use walk2() to make histograms with the right breaks
walk2(sims, breaks_list, hist)


# Turn this snippet into find_breaks()
find_breaks <- function(x) {
  rng <- range(x, na.rm = TRUE)
  seq(rng[1], rng[2], length.out = 30)
}

# Call find_breaks() on sims[[1]]
find_breaks(sims[[1]])

# Use map() to iterate find_breaks() over sims: nice_breaks
nice_breaks <- map(sims, find_breaks)

# Use nice_breaks as the second argument to walk2()
walk2(sims, nice_breaks, hist)


# Increase sample size to 1000
sims <- invoke_map(funs, params, n = 1000)

# Compute nice_breaks (don't change this)
nice_breaks <- map(sims, find_breaks)

# Create a vector nice_titles
nice_titles <- c("Normal(10, 1)", "Uniform(0, 5)", "Exp(5)")

# Use pwalk() instead of walk2()
pwalk(list(x = sims, breaks = nice_breaks, main = nice_titles), hist, xlab = "")


# Pipe this along to map(), using summary() as .f
sims %>%
  walk(hist) %>%
  map(summary)

map(list, ~function(.x))