# ype 2^5 in the editor to calculate 2 to the power 5.
# Type 28 %% 6 to calculate 28 modulo 6.

# Decimal values like 4.5 are called numerics.
# Natural numbers like 4 are called integers. Integers are also numerics.
# Boolean values (TRUE or FALSE) are called logical.
# Text (or string) values are called characters.

my_logical <- FALSE

class(my_numeric)
[1] "numeric"
> 
  > # Check class of my_character
  > class(my_character)
[1] "character"
> 
  > # Check class of my_logical
  > class(my_logical)
[1] "logical"

you create a vector with the combine function c(). 

numeric_vector <- c(1, 2, 3)
character_vector <- c("a", "b", "c")

Naming a vector

some_vector <- c("John Doe", "poker player")
names(some_vector) <- c("Name", "Profession")


It is important to know that if you sum two vectors in R, it takes the element-wise sum. For example, the following three statements are completely equivalent:
  
  c(1, 2, 3) + c(4, 5, 6)
c(1 + 4, 2 + 5, 3 + 6)
c(5, 7, 9)
You can also do the calculations with variables that represent vectors:
  
  a <- c(1, 2, 3) 
b <- c(4, 5, 6)
c <- a + b


Vector selection
poker_vector[c(1, 5)]
poker_vector[2:4]

# Poker and roulette winnings from Monday to Friday:
poker_vector <- c(140, -50, 20, -120, 240)
roulette_vector <- c(-24, -50, 100, -350, 10)
days_vector <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
names(poker_vector) <- days_vector
names(roulette_vector) <- days_vector

# Select poker results for Monday, Tuesday and Wednesday
poker_start <- poker_vector[c("Monday", "Tuesday", "Wednesday")]

# Calculate the average of the elements in poker_start
mean(poker_start)

The (logical) comparison operators known to R are:
  
  < for less than
> for greater than
<= for less than or equal to
>= for greater than or equal to
== for equal to each other
!= not equal to each other

# Which days did you make money on poker?
selection_vector <- poker_vector > 0

# Select from poker_vector these days
(poker_winning_days <- poker_vector[selection_vector])
poker_vector[c("Monday","Tuesday")]


# Construct a matrix with 3 rows that contain the numbers 1 up to 9
matrix(1:9, byrow = TRUE, nrow = 3)


# Box office Star Wars (in millions!)
new_hope <- c(460.998, 314.4)
empire_strikes <- c(290.475, 247.900)
return_jedi <- c(309.306, 165.8)

# Create box_office
(box_office <- c(new_hope, empire_strikes, return_jedi))

# Construct star_wars_matrix
(star_wars_matrix <- matrix(box_office, nrow = 3, byrow = TRUE) )

rownames(my_matrix) <- row_names_vector
colnames(my_matrix) <- col_names_vector

the function rowSums() conveniently calculates the totals for each row of a matrix. This function creates a new vector:
  
  box_office <- c(460.998, 314.4, 290.475, 247.900, 309.306, 165.8)
star_wars_matrix <- matrix(box_office, nrow = 3, byrow = TRUE,
                           dimnames = list(c("A New Hope", "The Empire Strikes Back", "Return of the Jedi"), 
                                           c("US", "non-US")))

# Calculate worldwide box office figures
(worldwide_vector <- rowSums(star_wars_matrix))

The function factor() will encode the vector as a factor:
  
  factor_sex_vector <- factor(sex_vector)

There are two types of categorical variables: a nominal categorical variable and an ordinal categorical variable.

A nominal variable is a categorical variable without an implied order. This means that it is impossible to say that 'one is worth more than the other'. For example, think of the categorical variable animals_vector with the categories "Elephant", "Giraffe", "Donkey" and "Horse". Here, it is impossible to say that one stands above or below the other. (Note that some of you might disagree ;-) ).

In contrast, ordinal variables do have a natural ordering. Consider for example the categorical variable temperature_vector with the categories: "Low", "Medium" and "High". Here it is obvious that "Medium" stands above "Low", and "High" stands above "Medium".


# Animals
animals_vector <- c("Elephant", "Giraffe", "Donkey", "Horse")
factor_animals_vector <- factor(animals_vector)
factor_animals_vector

# Temperature
temperature_vector <- c("High", "Low", "High","Low", "Medium")
factor_temperature_vector <- factor(temperature_vector, order = TRUE, levels = c("Low", "Medium", "High"))
factor_temperature_vector

levels(factor_vector) <- c("name1", "name2",...)

# Code to build factor_survey_vector
survey_vector <- c("M", "F", "F", "M", "M")
factor_survey_vector <- factor(survey_vector)

# Specify the levels of factor_survey_vector
levels(factor_survey_vector) <- c("Female", "Male")

factor_survey_vector

# Code to build factor_survey_vector
survey_vector <- c("M", "F", "F", "M", "M")
factor_survey_vector <- factor(survey_vector)

# Specify the levels of factor_survey_vector
levels(factor_survey_vector)
levels(factor_survey_vector) <- c("Female", "Male")

factor_survey_vector


# Build factor_survey_vector with clean levels
survey_vector <- c("M", "F", "F", "M", "M")
factor_survey_vector <- factor(survey_vector)
levels(factor_survey_vector) <- c("Female", "Male")
factor_survey_vector

# Generate summary for survey_vector
summary(survey_vector)

# Generate summary for factor_survey_vector
summary(factor_survey_vector)


# Create speed_vector
speed_vector <- c("medium", "slow", "slow", "medium", "fast")

# Add your code below
factor_speed_vector <- factor(speed_vector, ordered = TRUE, levels = c("slow", "medium", "fast"))

# Print factor_speed_vector
factor_speed_vector
summary(factor_speed_vector)


# Definition of vectors
name <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
type <- c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet", 
          "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant")
diameter <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

# Create a data frame from the vectors
(planets_df <- data.frame(name, type, diameter, rotation, rings))

planets_df[1:3,2]
planets_df[1:3,"type"]
planets_df$diameter

planets_df[rings_vector, ]
is equal to, keep TRUE rows
subset(planets_df, subset = rings)

subset(planets_df, subset = diameter < 1)

rings_vector
[1] FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE


Sorting

> a <- c(100, 10, 1000)
> order(a)
[1] 2 1 3
a[order(a)]

positions <- order(planets_df$diameter)

# Use positions to sort planets_df
planets_df[positions, ]


# Vector with numerics from 1 up to 10
my_vector <- 1:10 

# Matrix with numerics from 1 up to 9
my_matrix <- matrix(1:9, ncol = 3)

# First 10 elements of the built-in data frame mtcars
my_df <- mtcars[1:10,]

# Construct list with these different elements:
my_list <- list(my_vector, my_matrix, my_df)

my_list <- list(your_comp1, your_comp2)
names(my_list) <- c("name1", "name2")

# Vector with numerics from 1 up to 10
my_vector <- 1:10 

# Matrix with numerics from 1 up to 9
my_matrix <- matrix(1:9, ncol = 3)

# First 10 elements of the built-in data frame mtcars
my_df <- mtcars[1:10,]

# Adapt list() call to give the components names
my_list <- list(vec = my_vector, mat = my_matrix, df = my_df)

For example, to "grab" the first component of shining_list you type

shining_list[[1]]
shining_list[["reviews"]]
shining_list$reviews

shining_list[[2]][1]
# Print the second element of the vector representing the actors
shining_list$actors[2]

shining_list_full <- c(shining_list, year = 1980)

mean(linkedin, na.rm = TRUE)


my_fun <- function(arg1, arg2) {
  body
}

triple <- function(x) {
  x <- 3*x
  x
}
a <- 5
triple(a)
a


lapply(X, FUN, ...)
o put it generally, lapply takes a vector or list X, and applies the function FUN to each of its members.

# The vector pioneers has already been created for you
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")

# Split names from birth year
split_math <- strsplit(pioneers, split = ":")

# Convert to lowercase strings: split_low
split_low <- lapply(split_math, tolower)

# Take a look at the structure of split_low
str(split_low)

# The vector pioneers has already been created for you
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")

# Split names from birth year
(split_math <- strsplit(pioneers, split = ":"))

# Convert to lowercase strings: split_low
(split_low <- lapply(split_math, tolower))

# Take a look at the structure of split_low
str(split_low)

# Code from previous exercise:
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")
split <- strsplit(pioneers, split = ":")
split_low <- lapply(split, tolower)

# Write function select_first()
select_first <- function(x) {
  x[1]
}

# Apply select_first() over split_low: names
unlist(lapply(split_low,select_first))

# Use anonymous function inside lapply()
lapply(list(1,2,3), function(x) { 3 * x })

multiply <- function(x, factor) {
  x * factor
}
lapply(list(1,2,3), multiply, factor = 3)

# Definition of split_low
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")
split <- strsplit(pioneers, split = ":")
split_low <- lapply(split, tolower)

# Generic select function
select_el <- function(x, index) {
  x[index]
}

# Use lapply() twice on split_low: names and years
names <- lapply(split_low, select_el, index = 1)
years <- lapply(split_low, select_el, index = 2)

sapply(temp, max)
extremes <- function(x) {
  c(min = min(x), max = max(x))
}

# Apply extremes() over temp with sapply()
sapply(temp, extremes)

print_info <- function(x) {
  cat("The average temperature is", mean(x), "\n")
}

# Apply print_info() over temp using sapply()
sapply(temp, print_info)

# Apply print_info() over temp using lapply()
lapply(temp, print_info)

# temp is already available in the workspace

# Definition of the basics() function
basics <- function(x) {
  c(min = min(x), mean = mean(x), median = median(x), max = max(x))
}

# Fix the error:
vapply(temp, basics, numeric(4))

vapply(temp, max, numeric(1))

# Convert to vapply() expression
vapply(temp, function(x, y) { mean(x) > y }, logical(1), y = 5)