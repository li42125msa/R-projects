library(readr)
library(tidyverse)
library(forcats)
multiple_choice_responses<- read_csv("smc_with_js.csv")


# Print out the dataset
str(multiple_choice_responses)

# Check if CurrentJobTitleSelect is a factor
is.factor(multiple_choice_responses$CurrentJobTitleSelect)

multipleChoiceResponses %>%
  mutate_if(is.character, as.factor)

nlevels(multipleChoiceResponses$LearningDataScienceTime)

levels(multipleChoiceResponses$LearningDataScienceTime)

multipleChoiceResponses %>%
  summarise_if(is.factor, nlevels)

names(multiple_choice_responses)

str(mtcars)
mtcars %>%
  top_n(2, mpg) %>%
  pull(mpg)
mtcars%>% select(mpg)%>% arrange(desc(mpg))


# Change all the character columns to factors
responses_as_factors <- multiple_choice_responses %>% 
  mutate_if(is.character, as.factor)

number_of_levels <- responses_as_factors %>% 
  # apply the function nlevels to each column
  summarise_all(nlevels) %>%
  # change the dataset from wide to long
  gather(variable, num_levels)


# Select the 3 rows with the highest number of levels 
number_of_levels %>%
  top_n(3, num_levels)

number_of_levels %>%
  # filter for where variable is CurrentJobTitleSelect
  filter(variable == "CurrentJobTitleSelect") %>%
  # pull num_levels
  pull(num_levels)

# Get the levels of WorkInternalVsExternalTools
levels(multiple_choice_responses$WorkInternalVsExternalTools)

responses_as_factors %>%
  # pull CurrentJobTitleSelect
  pull(CurrentJobTitleSelect) %>%
  # get the values of the levels
  levels()

# Make a bar plot
library(forcats)
ggplot(multiple_choice_responses, aes(x = fct_infreq(CurrentJobTitleSelect))) + 
  geom_bar() + 
  # flip the coordinates
  coord_flip()


ggplot(WorkChallenges) + 
  geom_point(aes(x = fct_reorder(question, perc_problem), y = perc_problem))

ggplot(multiple_choice_responses) + 
  geom_bar(aes(x = fct_rev(fct_infreq(CurrentJobTitleSelect))))

# mutate_all() and summarise_all take one argument,
# a function, and apply it to all columns.

multiple_choice_responses %>%
  # remove NAs
  filter(!is.na(EmployerIndustry) & !is.na(Age)) 

ggplot(aes(nlp_frequency, 
           x = fct_relevel(response, 
                           "Rarely", "Sometimes", "Often", "Most of the time"))) + 
  geom_bar()

# move levels to front
nlp_frequency %>%
  mutate(response = fct_relevel(response, 
                                "Often", "Most of the time")) %>%
  pull(response) %>%
  levels()


nlp_frequency %>%
  mutate(response = fct_relevel(response, 
                                "Often", "Most of the time", after = 2)) %>%
  pull(response) %>%
  levels()

nlp_frequency %>%
  mutate(response = fct_relevel(response, 
                                "Often", "Most of the time", after = Inf) %>%
           pull(response) %>%
           levels()

# Reorder the levels from internal to external 
mc_responses_reordered <- multiple_choice_responses %>%
  mutate(WorkInternalVsExternalTools = fct_relevel(WorkInternalVsExternalTools,
                                                   "Entirely internal", 
                                                   "More internal than external",
                                                   "Approximately half internal and half external",
                                                   "More external than internal", 
                                                   "Entirely external"
  ))


multiple_choice_responses %>%
  # Move "I did not complete any formal education past high school" and "Some college/university study without earning a bachelor's degree" to the front
  mutate(FormalEducation = fct_relevel(FormalEducation, 
                                       "I did not complete any formal education past high school", 
                                       "Some college/university study without earning a bachelor's degree")) %>%
  # Move "I prefer not to answer" to be the last level.
  mutate(FormalEducation = fct_relevel(FormalEducation, "I prefer not to answer", after = Inf)) %>%
  # Move "Doctoral degree" to be the sixth level
  mutate(FormalEducation = fct_relevel(FormalEducation, "Doctoral degree", after = 5)) %>%
  # Examine the new level order
  pull(FormalEducation) %>%
  levels()


flying_etiquette %>% 
  mutate(middle_arm_rest_three = fct_recode(middle_arm_rest_three,
                                            "Other" = "Other (please specify)",
                                            "Everyone should share" = "The arm rests should be shared",
                                            "Aisle and window people" = 
                                              "The people in the aisle and window seats get both arm rests",
                                            "Middle person" = "The person in the middle seat gets both arm rests",
                                            "Fastest person" = "Whoever puts their arm on the arm rest first"
  )) %>%
  count(middle_arm_rest_three)


flying_etiquette %>%
  mutate(height = fct_collapse(height, 
                               under_5_3 = c("Under 5 ft.", "5'0\"", "5'1\"", "5'2\""),
                               over_6_1 = c("6'1\"", "6'2\"", "6'3\"", "6'4\"", 
                                            "6'5\"", "6'6\" and above"))) %>%
  pull(height) %>%
  levels()

flying_etiquette %>%
  mutate(new_height = fct_other(height, keep = c("6'4\"", "5'1\""))) %>%
  count(new_height)

flying_etiquette %>%
  mutate(new_height = fct_other(height, 
                                drop = c("Under 5 ft.", "5'0\"", "5'1\"", "5'2\"", "5'3\""))) %>%
  pull(new_height) %>%
  levels()

flying_etiquette %>%
  mutate(new_height = fct_lump(height, prop = .08)) %>%
  count(new_height)

flying_etiquette %>%
  mutate(new_height = fct_lump(height, n = 3)) %>%
  count(new_height)

multiple_choice_responses %>%
  # Create new variable, grouped_titles, by collapsing levels in CurrentJobTitleSelect
  mutate(grouped_titles = fct_collapse(CurrentJobTitleSelect, 
                                       "Computer Scientist" = c("Programmer", "Software Developer/Software Engineer"), 
                                       "Researcher" = "Scientist/Researcher", 
                                       "Data Analyst/Scientist/Engineer" = c("DBA/Database Engineer", "Data Scientist", 
                                                                             "Business Analyst", "Data Analyst", 
                                                                             "Data Miner", "Predictive Modeler"))) %>%
  # Turn every title that isn't now one of the grouped_titles into "Other"
  mutate(grouped_titles = fct_other(grouped_titles, 
                                    keep = c("Computer Scientist", 
                                             "Researcher", 
                                             "Data Analyst/Scientist/Engineer"))) %>% 
  # Get a count of the grouped titles
  count(grouped_titles)

multiple_choice_responses %>%
  # remove NAs of MLMethodNextYearSelect
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # create ml_method, which lumps all those with less than 5% of people into "Other"
  mutate(ml_method = fct_lump(MLMethodNextYearSelect, prop = .05)) %>%
  # count the frequency of your new variable, sorted in descending order
  count(ml_method, sort = TRUE)

multiple_choice_responses %>%
  # remove NAs 
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # create ml_method, retaining the 5 most common methods and renaming others "other method" 
  mutate(ml_method = fct_lump(MLMethodNextYearSelect, n = 5, other_level = "other method")) %>%
  # count the frequency of your new variable, sorted in descending order
  count(ml_method, sort = TRUE)


multipleChoiceResponses %>%
  select(contains("WorkChallengeFrequency")) %>%
  gather(work_challenge, frequency)


work_challenges <- multipleChoiceResponses %>%
  select(contains("WorkChallengeFrequency")) %>%
  gather(work_challenge, frequency) %>%
  mutate(work_challenge = str_remove(work_challenge, 
                                     "WorkChallengeFrequency"))


work_challenges %>%
  filter(!is.na(frequency)) %>%
  mutate(frequency = if_else(
    frequency %in% c("Most of the time", "Often"), 
    1, 
    0)
  ) %>%
  group_by(work_challenge) %>%
  summarise(perc_problem = mean(frequency))


learning_platform_usefulness <- multiple_choice_responses %>%
  # select columns with LearningPlatformUsefulness in title
  select(contains("LearningPlatformUsefulness")) %>%
  # change data from wide to long
  gather(learning_platform, usefulness) %>%
  # remove rows where usefulness is NA
  filter(!is.na(usefulness)) %>%
  # remove "LearningPlatformUsefulness" from each string in `learning_platform 
  mutate(learning_platform = str_replace(learning_platform, "LearningPlatformUsefulness", ""))


iris %>%
  add_count(Species, wt = Petal.Length) %>%
  select(Species, Petal.Length, n)

iris%>% group_by(Species)%>%summarize(sum(Petal.Length))

learning_platform_usefulness %>%
  # change dataset to one row per learning_platform usefulness pair with number of entries for each
  count(learning_platform, usefulness)

usefulness_by_platform <- learning_platform_usefulness %>%
  # If usefulness is "Not Useful", make 0, else 1 
  mutate(usefulness = if_else(usefulness == "Not Useful", 0, 1)) %>%
  # Group by learning platform 
  group_by(learning_platform) %>%
  # Summarize the mean usefulness for each platform
  summarize(avg_usefulness = mean(usefulness))


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

multiple_choice_responses %>%
  # Filter out people who selected Data Scientist as their Job Title
  filter(CurrentJobTitleSelect != "Data Scientist") %>%
  # Create a new variable, job_identity
  mutate(job_identity = case_when(
    CurrentJobTitleSelect == "Data Analyst" & 
      DataScienceIdentitySelect == "Yes" ~ "DS analysts", 
    CurrentJobTitleSelect == "Data Analyst" & 
      DataScienceIdentitySelect %in% c("No", "Sort of (Explain more)") ~ "NDS analyst", 
    CurrentJobTitleSelect != "Data Analyst" & 
      DataScienceIdentitySelect == "Yes" ~ "DS non-analysts", 
    TRUE ~ "NDS non analysts")) %>%
  # Get the average job satisfaction by job_identity, removing NAs
  group_by(job_identity) %>%
  summarize(avg_js = mean(JobSatisfaction, na.rm = TRUE))


flying_etiquette %>%
  # Change characters to factors
  mutate_if(is.character, as.factor) %>%
  # Filter out those who have never flown on a plane
  filter(`How often do you travel by plane?` != "Never")


gathered_data <- flying_etiquette %>%
  mutate_if(is.character, as.factor) %>%
  filter(`How often do you travel by plane?` != "Never") %>%
  # Select columns containing "rude"
  select(contains("rude")) %>%
  # Change format from wide to long
  gather(response_var, value)


gathered_data %>%
  # Remove everything before and including "rude to "
  mutate(response_var = str_remove(response_var, '.*rude to ')) %>%
  # Remove "on a plane"
  mutate(response_var = str_remove(response_var, 'on a plane'))

dichotimized_data <- gathered_data %>%
  mutate(response_var = str_replace(response_var, '.*rude to ', '')) %>%
  mutate(response_var = str_replace(response_var, 'on a plane', '')) %>%
  # Remove rows that are NA in the value column
  filter(!is.na(value)) %>%
  # Dichotomize the value variable to make a new variable, rude
  mutate(rude = if_else(value %in% c('No, not rude at all', 'No, not at all rude'), 0, 1))


rude_behaviors <- gathered_data %>%
  mutate(response_var = str_replace(response_var, '.*rude to ', '')) %>%
  mutate(response_var = str_replace(response_var, 'on a plane', '')) %>%
  # Remove rows that are NA in the value column
  filter(!is.na(value)) %>%
  mutate(rude = if_else(value %in% c("No, not rude at all", "No, not at all rude"), 0, 1)) %>%
  # Create perc_rude, the percent considering each behavior rude
  group_by(response_var) %>%
  summarise(perc_rude = mean(rude))

rude_behaviors