library(purrr)
coefs <- map(models, coef)

# use map_dbl with the numeric shortcut to pull out the second element
map_dbl(coefs, 2)

cyl <- split(mtcars, mtcars$cyl) 
map(cyl, ~ lm(mpg ~ wt, data = .))

split(mtcars, mtcars$cyl) %>% 
  map(~ lm(mpg ~ wt, data = .))

# Define models (don't change)
models <- mtcars %>% 
  split(mtcars$cyl) %>%
  map(~ lm(mpg ~ wt, data = .))

# Rewrite to be a single command using pipes 
summaries <- map(models, summary)
map_dbl(summaries, "r.squared")

# Rewrite to be a single command using pipes 
models %>% 
  map(summary) %>%
  map_dbl("r.squared")

# Create safe_readLines() by passing readLines() to safely()
safe_readLines <- safely(readLines)

# Call safe_readLines() on "http://example.org"
example_lines <- safe_readLines("http://example.org")
example_lines

# Define safe_readLines()
safe_readLines <- safely(readLines)
urls <- list(
  example = "http://example.org",
  rproj = "http://www.r-project.org",
  asdf = "http://asdfasdasdkfjlda"
)
# Use the safe_readLines() function with map(): html
html <- map(urls, safe_readLines)

# Call str() on html
str(html)

# Extract the result from one of the successful elements
html[["example"]][["result"]]

# Extract the error from the element that was unsuccessful
html[["asdf"]][["error"]]