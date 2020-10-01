# loading libraries
library(tidyverse)
library(purrr)
library(readr)

# loading in data
rwa <- read_csv("raw_data/rwa.csv")

# reverse score function 
reverse_score <- function(x) {
    return(10 - x)
}

# apply reverse socre function to vairables listed in the question
# calculate average scores as 'overall' raw score 
rwa1 <- map_at(rwa, c(4, 6, 8, 9, 11, 13, 15, 18, 20, 21), reverse_score) %>%
    as_tibble() %>%
    mutate(rwa = rowMeans(select(., Q3:Q22))) %>%
    select(rwa, education, urban, gender, age, hand, religion, orientation, race, voted, married, familysize, testelapse)

# recoding some variables to give context and ensure human readable
# also removing na rows 
rwa2 <-
    rwa1 %>%
    mutate(
        education = recode(education,
                           `1` = "Less than high school",
                           `2` = "High school",
                           `3` = "University degree",
                           `4` = "Graduate degree"
                           ),
        childhood = recode(urban,
                           `1` = "Rural (country side)",
                           `2` = "Suburban",
                           `3` = "Urban (town, city)"
                           ),
        gender = recode(gender,
                        `1` = "Male",
                        `2` = "Female",
                        `3` = "Other"
                        ),
        hand = recode(hand,
                      `1` = "Right",
                      `2` = "Left",
                      `3` = "Both"
                      ),
        ) %>%
    select(rwa, education, childhood, gender, hand, family_size = familysize, age, test_time = testelapse) %>%
    na.omit()
# do get warnings here but this is when people have input a code level which does not exist (e.g. 0)

# Removing outliers/unlikely values for family size, test time and age to avoid these results skewing averages (have assumed these are errors)
rwa3 <- rwa2 %>%
    filter(family_size < 25) %>%
    filter(test_time < 10000) %>%
    filter(age < 100)

# Write clean data to csv
write_csv(rwa3, 'clean_data/rwa_clean.csv')
