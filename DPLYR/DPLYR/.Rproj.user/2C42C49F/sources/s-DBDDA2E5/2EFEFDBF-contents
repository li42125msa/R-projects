
# DeaLING WITH MISSING DATA IN R
# Create x, a vector, with values NA, NaN, Inf, ".", and "missing"
x <- c(NA, NaN, Inf, ".", "missing")
library(rlang);library(naniar)
# Use any_na() and are_na() on to explore the missings
anyNA(x)
are_na(x)

# Use n_miss() to count the total number of missing values in dat_hw
n_miss(x)

# Use n_miss() on dat_hw$weight to count the total number of missing values
n_miss(dat_hw$weight)

# Use n_complete() on dat_hw to count the total number of complete values
n_complete(x)

# Use n_complete() on dat_hw$weight to count the total number of complete values
n_complete(dat_hw$weight)

# Summarise missingness in each variable of the `airquality` dataset
miss_var_summary(airquality)

# Tabulate missingness in each variable and case of the `airquality` dataset
miss_var_table(airquality)
miss_case_table(airquality)
library(dplyr)
# Tabulate the missingness in each variable, grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_var_table()

# Tabulate of missingness in each case, grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_case_table()


# Visualize all of the missingness in the `riskfactors`  dataset
vis_miss(riskfactors)

# Visualize and cluster all of the missingness in the `riskfactors` dataset
vis_miss(riskfactors, cluster = TRUE)

# Visualize and sort the columns by missingness in the `riskfactors` dataset
vis_miss(riskfactors, sort_miss = TRUE)


# Visualize the number of missings in cases using `gg_miss_case()`
gg_miss_case(riskfactors)

# Explore the number of missings in cases using `gg_miss_case()` and facet by the variable `education`
gg_miss_case(riskfactors, facet = education)

# Visualize the number of missings in variables using `gg_miss_var()`
gg_miss_var(riskfactors)

# Explore the number of missings in variables using `gg_miss_var()` and facet by the variable `education`
gg_miss_var(riskfactors, facet = education)

# Using the airquality dataset, explore the missingness pattern using gg_miss_upset()
gg_miss_upset(airquality)

# With the riskfactors dataset, explore how the missingness changes across the marital variable using gg_miss_fct()
gg_miss_fct(x = riskfactors, fct = marital)

# Using the pedestrian dataset, explore how the missingness of hourly_counts changes over a span of 3000 
gg_miss_span(pedestrian, var = hourly_counts, span_every = 3000)

# Using the pedestrian dataset, explore the impact of month by facetting by month
# and explore how missingness changes for a span of 1000
gg_miss_span(pedestrian, var = hourly_counts, span_every = 1000, facet = month)

# Explore the strange missing values "N/A"
miss_scan_count(data = pacman, search = list("N/A"))

# Explore the strange missing values "missing"
miss_scan_count(data = pacman, search = list("missing"))

# Explore the strange missing values "na"
miss_scan_count(data = pacman, search = list("na"))

# Explore the strange missing values " " (a single space)
miss_scan_count(data = pacman, search = list(" "))

# Explore all of the strange missing values, "N/A", "missing", "na", " "
miss_scan_count(data = pacman, search = list("N/A", "missing","na", " "))

# Print the top of the pacman data using `head()`
head(pacman)

# Replace the strange missing values "N/A", "na", and "missing" with `NA` for the variables, year, and score
pacman_clean <- replace_with_na(pacman, replace = list(year = c("N/A", "na", "missing"),
                                                       score = c("N/A", "na", "missing")))

# Test if `pacman_clean` still has these values in it?
miss_scan_count(pacman_clean, search = list("N/A", "na", "missing"))

# Use `replace_with_na_at()` to replace with NA
replace_with_na_at(pacman,
                   .vars = c("year", "month", "day"), 
                   ~.x %in% c("N/A", "missing", "na", " "))

# Use `replace_with_na_if()` to replace with NA the character values using `is.character`
replace_with_na_if(pacman,
                   .predicate = is.character, 
                   ~.x %in% c("N/A", "missing", "na", " "))

# Use `replace_with_na_all()` to replace with NA
replace_with_na_all(pacman, ~.x %in% c("N/A", "missing", "na", " "))

# Print the frogger data to have a look at it
frogger

# Use `complete()` on the `time` variable to make implicit missing values explicit
frogger_tidy <- frogger %>% complete(time, name)

library(tidyr)
# Print the frogger data to have a look at it
frogger

# Use `fill()` to fill down the name variable in the frogger dataset
frogger %>% fill(name)

# Print the frogger data to have a look at it
frogger

# Correctly fill() and complete() missing values so that our dataset becomes sensible
frogger %>%
  fill(name) %>%
  complete(name, time)


# Arrange by year
oceanbuoys %>% arrange(year) %>% vis_miss()

# Arrange by latitude
oceanbuoys %>% arrange(latitude) %>% vis_miss()

# Arrange by wind_ew (wind east west)
oceanbuoys %>% arrange(wind_ew) %>% vis_miss()