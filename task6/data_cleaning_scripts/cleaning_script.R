# Loading libraries
library(tidyverse)
library(janitor)

# Reading in data
dogs <- read_csv("raw_data/dog_survey.csv")

# First cleaning pass
dogs_1 <- dogs %>%
    clean_names() %>% 
    remove_empty("cols") %>% # Catch all for empty columns
    # removing symbols
    mutate(amount_spent_on_dog_food = str_replace(amount_spent_on_dog_food, "£", "")) %>%
    mutate_all(str_replace_all, "-", NA_character_) %>%
    mutate(dog_gender = toupper(dog_gender)) %>%
    mutate(dog_size = toupper(dog_size))

# Checking for ID duplication
dogs_1 %>%
    select(id) %>%
    group_by(id) %>%
    summarise(count_id = n()) %>%
    arrange(desc(count_id)) %>%
    filter(count_id > 1)


# Removing duplicate rows
dogs_2 <- dogs_1 %>%
    distinct()

# Checking removed
dogs_2 %>%
    select(id) %>%
    group_by(id) %>%
    summarise(count_id = n()) %>%
    arrange(desc(count_id)) %>%
    filter(count_id > 1)

# Manual fixes for size and gender
dogs_3 <- dogs_2 %>%
    mutate(
        dog_size = if_else(dog_size == "MEDIUM SIZED", "M", dog_size),
        dog_size = if_else(dog_size == "LARGE", "L", dog_size),
        dog_size = if_else(dog_size == "SMALLISH", "S", dog_size),
        dog_size = if_else(dog_size == "NO", NA_character_, dog_size),
        dog_size = if_else(dog_size == "N/A", NA_character_, dog_size)
    ) %>%
    mutate(
        dog_gender = if_else(dog_gender == "FEMALE", "F", dog_gender),
        dog_gender = if_else(dog_gender == "MALE", "M", dog_gender),
        dog_gender = if_else(dog_gender == "FEMLAE", "F", dog_gender),
        dog_gender = if_else(dog_gender == "DON’T KNOW", NA_character_, dog_gender),
        dog_gender = if_else(dog_gender == "UNKNOWN", NA_character_, dog_gender),
        dog_gender = if_else(dog_gender == "UNKOWN", NA_character_, dog_gender)
    ) %>%
    mutate(
        amount_spent_on_dog_food = if_else(amount_spent_on_dog_food == "Between 10 and £20", "15", amount_spent_on_dog_food),
        amount_spent_on_dog_food = str_remove(amount_spent_on_dog_food, "[^[:alnum:]+\\.]"),
        amount_spent_on_dog_food = str_remove(amount_spent_on_dog_food, "[\\!]+"),
        amount_spent_on_dog_food = str_remove(tolower(amount_spent_on_dog_food), "[a-z ]*"),
        amount_spent_on_dog_food = if_else(amount_spent_on_dog_food == "", NA_character_, amount_spent_on_dog_food),
    ) %>%
    mutate(
        dog_age = if_else(dog_age == "5 and 4", "5,4", dog_age),
        dog_age = if_else(dog_age == "Less than 20", NA_character_, dog_age),
        dog_age = if_else(dog_age == "12+", NA_character_, dog_age)
    )

# Removing multiple dog ownership for purpose of this analysis, fixing datatypes, fixing negative values and removing ID column
dogs_4 <- dogs_3 %>%
    filter(nchar(dog_gender) == 1) %>%
    mutate(amount_spent_on_dog_food = abs(as.numeric(amount_spent_on_dog_food))) %>%
    mutate(dog_age = as.numeric(dog_age)) %>% 
    select(-id)

# Removing rownames
rownames(dogs_4) <- c()


# Writing data to clean csv
write.csv(dogs_4, "clean_data/dog_survey_clean.csv",row.names = FALSE)
    
# clearing objects from enivornment
rm(list=ls())

