# Loading libraries

library(janitor)
library(tidyverse)
library(readxl)
library(here)


#  Loading Excel data and cleaning column names

raw_data_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx")
raw_data_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx")
raw_data_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx")


# Cleaning column names

candy_2015 <- clean_names(raw_data_2015)
candy_2016 <- clean_names(raw_data_2016)
candy_2017 <- clean_names(raw_data_2017)


# Removing question numbers from 2017 data

names(candy_2017) <- str_remove(names(candy_2017), "q[0-9]+_")

# Tidying data by adding id number, year, missing gender and country columns before pivoting longer and then binding datasets.

candy_2015_clean <- 
    candy_2015 %>%
    mutate(person_id = row_number()) %>%
    pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
    mutate(
        year = 2015,
        gender = NA,
        country = NA
    ) %>%
    select(
        year,
        person_id,
        going_out = are_you_going_actually_going_trick_or_treating_yourself,
        age = how_old_are_you,
        gender,
        country,
        candy,
        rating
    ) 

candy_2016_clean <-
    candy_2016 %>%
    # ensuring that the id numbers continue
    mutate(person_id = max(candy_2015_clean$person_id) + row_number()) %>%
    pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
    mutate(
        year = 2016
    ) %>%
    select(
        year,
        person_id,
        going_out = are_you_going_actually_going_trick_or_treating_yourself,
        age = how_old_are_you,
        gender = your_gender,
        country = which_country_do_you_live_in,
        candy,
        rating
    ) 


candy_2017_clean <- 
    candy_2017 %>%
    rename(x100_grand_bar = `100_grand_bar`) %>%
    # ensuring that the id numbers continue on
    mutate(person_id = max(candy_2016_clean$person_id) + row_number()) %>%
    pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
    mutate(
        year = 2017
    )  %>%
    select(
        year,
        person_id,
        going_out,
        age,
        gender,
        country,
        candy,
        rating
    )

# binding the years together
candy_combined <- rbind(candy_2015_clean, candy_2016_clean, candy_2017_clean)

# fixing age to numeric and fixing capitals

candy_combined_tidy <- 
    candy_combined %>% 
    mutate(
        age = as.numeric(age),
        rating = str_to_lower(rating),
        country = str_to_lower(country)
    )


# Fixing country spellings and consistency

us_mispellings <- c(
    "unites states",
    "murica",
    "united state",
    "united stated",
    "united ststes",
    "trumpistan",
    "united sates",
    "merica",
    "'merica",
    "ahem....amerca",
    "alaska",
    "murrika",
    "california",
    "new jersey",
    "new york",
    "north carolina",
    "pittsburgh",
    "u s",
    "unhinged states",
    "unied states",
    "unite states",
    "united staes",
    "united statea",
    "united statss",
    "the yoo ess of aaayyyyyy",
    "united stetes",
    "units states",
    "cascadia",
    "the republic of cascadia"
)


uk_mispellings <- c(
    "uk",
    "scotland",
    "england",
    "ireland",
    "u.k.",
    "united kindom"
)

unknown_country <- c(
    "a",
    "atlantis",
    "canae",
    "earth",
    "endland",
    "europe",
    "fear and loathing",
    "i don't know anymore",
    "insanity lately",
    "narnia",
    "soviet canuckistan",
    "ud",
    "a tropical island south of the equator",
    "denial",
    "eua",
    "god's country",
    "neverland",
    "one of the best ones",
    "see above",
    "somewhere",
    "there isn't one for old men",
    "this one"
)


candy_combined_clean <- 
    candy_combined_tidy %>%
    mutate(
        # Find all US
        country = if_else(str_detect(country, "usa"), "united states", country),
        country = if_else(str_detect(country, "us"), "united states", country),
        country = if_else(str_detect(country, "united states"), "united states", country),
        country = if_else(str_detect(country, "u.s."), "united states", country),
        country = if_else(str_detect(country, "america"), "united states", country),
        country = if_else(country %in% us_mispellings, "united states", country),
        # Tidy other countries
        country = if_else(country %in% uk_mispellings, "united kingdom", country),
        country = if_else(country %in% c("canada`", "can"), "canada", country),
        country = if_else(country == "españa", "spain", country),
        country = if_else(country == "korea", "south korea", country),
        country = if_else(country == "the netherlands", "netherlands", country),
        # Change remaining into missing values
        country = if_else(str_detect(country, "[0-9]"), NA_character_, country), 
        country = if_else(country %in% unknown_country, NA_character_, country)
    )


# Removing unlikely ages for better analysis

candy_combined_clean <- 
    candy_combined_clean %>%
    mutate(age = if_else(age > 99 | age == 0, NA_real_, age))


# Writing to csv

write_csv(candy_combined_clean , "clean_data/candy_clean.csv")