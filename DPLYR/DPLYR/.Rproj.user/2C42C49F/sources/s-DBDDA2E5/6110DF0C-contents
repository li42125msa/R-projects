library(skimr)
library(tidyverse)
library(readr)
library(skimr)
library(here)
here()
bakeoff %>% 
  distinct(result)
bakeoff


glimpse(bakeoff)
skim(bakeoff)

# How many variables of each type do we have
bakeoff%>% 
  skim() %>%  # no argument needed here
  summary() # no argument needed here


bakers_mini %>% 
  group_by(series) %>% 
  skim() # no argument needed here


bakeoff %>% 
  count(us_season,result) %>% 
  mutate(prop_bakers = n/sum(n))


bakeoff %>% 
  group_by(us_season,result) %>% 
  summarize(n = n()) %>% 
  ungroup() %>%  # need ungroup
  mutate(prop_bakers = n/sum(n))


bakeoff %>% 
  count(us_season,result) %>% 
 count(us_season)

# Find format to parse uk_airdate 
parse_date("17 August 2010", format = "%d %B %Y")

library(skimr)
library(tidyverse)
bakeoff %>% 
  group_by(series) %>% 
  skim() %>%
  summary()# no argument needed here

# View distinct results
bakeoff %>% 
  distinct(result)

ggplot(bakeoff, aes(episode)) + 
  geom_bar() + 
  facet_wrap(~series)

# Try to cast technical as a number
# Cast result a factor
library(readr)
desserts <- read_csv("desserts.csv", 
                     na = c("", "NA", "N/A"),
                     col_types = cols(
                       technical = col_number(),
                       uk_airdate = col_date(format = "%d %B %Y"),
                       result = col_factor(levels = NULL)
                     ))


# Create dummy variable: 1 if won, 0 if not
library(dplyr)
desserts <- desserts %>% 
  mutate(tech_win = recode(technical, `1` = 1, #'0'=NA_character
                           .default = 0))

#Convert to NA only
young_bakers %>% 
  mutate(student = na_if(student, 0))   #Convert to NA only, convert 0 to NA


# Glimpse to view
glimpse(desserts)


# Count rows grouping by nut variable
desserts %>% 
  count(nut, sort = TRUE)

# Move channel to first column
ratings %>% 
  select(channel, everything())


# Select & change variable names
young_bakers3 %>% 
  select(baker, tech_ = tre1:tre3)

young_bakers3 %>%
  select(baker, tech_ = starts_with("tr"),
         result_ = starts_with("rs"))

# Select & change names without reordering

young_bakers3 %>% 
  select(everything(), tech_ = starts_with("tr"),
         result_ = starts_with("rs"))

# Clean all variable names
library(janitor)
young_bakers3 %>% 
  clean_names()

# Adapt code to drop 28-day columns; keep 7-day in front
viewers_7day <- ratings %>% 
  select(viewers_7day_ = ends_with("7day"), 
         everything(), 
         -ends_with("28day"))

# Glimpse
glimpse(viewers_7day)



juniors_tidy %>% 
  count(baker, wt = correct)


# Plot of episode 1 viewers by series
library(ggplot2);library(dplyr);library(tidyverse)
ggplot(messy_ratings, aes(x = series, y = e1)) +
  geom_col()


tidy_ratings <- messy_ratings %>%
  # Gather and convert episode to factor
  gather(key = "episode", value = "viewers_7day", -series, factor_key = TRUE, na.rm = TRUE)

library(tidyr)
week_ratings <- ratings2 %>% 
  select(series, ends_with("7day")) %>% 
  gather(episode, viewers_7day, ends_with("7day"), 
         na.rm = TRUE) %>% 
  # Edit to separate key column and drop extra
  separate(episode, into = "episode", extra = "drop",convert = TRUE) 

# Print to view
week_ratings

data %>%
  unite(new_var, old_var1, old_var2)


data %>%
  unite(new_var, old_var1, old_var2)

# Create tidy data with 7- and 28-day viewers
tidy_ratings_all <- ratings2 %>%
  gather(key = episode, value = viewers, ends_with("day"), na.rm = TRUE) %>% 
  separate(episode, into = c("episode", "days")) %>%  
  mutate(episode = parse_number(episode),
         days = parse_number(days))

# Gather viewer columns and remove NA rows
tidy_ratings <- ratings %>%
  gather(key = episode, value = viewers, -series, na.rm = TRUE)

# Recode first/last episodes
first_last <- tidy_ratings %>% 
  mutate(episode = recode(episode, `1` = "first", .default = "last"))


series episode viewers
<fct>  <chr>     <dbl>
  1 1      first      2.24
2 2      first      3.1 

# Spread into three columns
bump_by_series <- first_last %>% 
  spread(episode, viewers)

bakers %>% 
  mutate(gen = if_else(between(birth_year, 1981, 1996), 
                       "millenial",
                       "not millenial"))

bakers %>% 
  mutate(gen = case_when(
    between(birth_year, 1928, 1945) ~ "silent",
    between(birth_year, 1946, 1964) ~ "boomer",
    between(birth_year, 1965, 1980) ~ "gen_x",
    between(birth_year, 1981, 1996) ~ "millenial",
    TRUE ~ "gen_z"
  ))

bakers %>% 
  count(gen, sort = TRUE) %>% 
  mutate(prop = n / sum(n))

# Create skills variable with 3 levels
bakers_skill <- bakers %>% 
  mutate(skill = case_when(
    star_baker > technical_winner ~ "super_star",
    star_baker < technical_winner ~ "high_tech",
    TRUE ~ "well_rounded"
  ))

# Reorder from most to least bakers
library(forcats);library(ggplot2)
ggplot(bakeoff, aes(x = fct_infreq(factor(technical)))) +
  geom_bar()


ggplot(bakers, aes(x = fct_rev(fct_infreq(gen)))) +
  geom_bar()


# Relevel using natural order
library(forcats)
# Reorder by hand

bakers <- bakers %>% 
  mutate(gen = fct_relevel(gen, "silent", "boomer", 
                           "gen_x", "millenial", "gen_z"))

bakers %>% 
  dplyr::pull(gen) %>% 
  levels()

bakers <- bakers %>% 
  mutate(gen = fct_relevel(gen, "silent", "boomer", 
                           "gen_x", "millenial", "gen_z"))
ggplot(bakers, aes(x = gen)) + geom_bar()

bakers <- bakers %>% 
  mutate(series_winner = as.factor(series_winner))
ggplot(bakers, aes(x = gen, fill = series_winner)) + geom_bar()

library(lubridate);library(dplyr)

dmy("17 August 2010")

hosts <- tibble::tribble(
  ~host, ~bday, ~premiere, 
  "Mary", "24 March 1935", "August 17th, 2010", 
  "Paul", "1 March 1966", "August 17th, 2010")

hosts <- hosts %>% 
  mutate(bday = dmy(bday),
         premiere = mdy(premiere))

hosts <- hosts %>% 
  mutate(age_int = interval(bday, premiere))

years(1)
years(2)
months(12)


hosts %>% 
  mutate(years_decimal = age_int / years(1),
         years_whole = age_int %/% years(1)) 

hosts %>% 
  mutate(age_y = age_int %/% years(1),
         age_m = age_int %/% months(12))

# Cast characters as dates
# Cast last_date_appeared_us as a date
baker_dates_cast <- baker_dates %>% 
  mutate(last_date_appeared_us = dmy(last_date_appeared_us))

# Create interval between first and last UK dates
baker_time <- baker_time %>% 
  mutate(time_on_air = interval(first_date_appeared_uk, last_date_appeared_uk))

library(readr);library(stringr)
series5 <- series5 %>% 
  separate(about, into = c("age", "occupation"), sep = ", ") %>% 
  mutate(age = parse_number(age))

series5 <- series5 %>% 
  mutate(baker = str_to_upper(baker),
         showstopper = str_to_lower(showstopper))

series5 %>% 
  mutate(pie = str_detect(showstopper, "pie"))

series5 %>% 
  mutate(showstopper = str_replace(showstopper, "pie", "tart"))
series5 %>% 
  mutate(showstopper = str_remove(showstopper, "pie"))

series5 %>% 
  mutate(showstopper = str_remove(showstopper, "pie"),
         showstopper = str_trim(showstopper))