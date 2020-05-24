#loading in libraries

library(readr)
library(here)
library(janitor)
library(tidyverse)


# loading in the data
raw_data <- read_rds("raw_data/decathlon.rds")

# checking column names
names(raw_data)

# cleaning and tidying data before adjusting column titles for readability
clean_data <-
    raw_data %>%
    clean_names() %>% 
    rownames_to_column(var = "competitor") %>% 
    rename(ranking = rank,
           overall_competition_points = points,
           "100m_sprint" = x100m,
           "400m_sprint" = x400m,
           "110m_hurdles" = x110m_hurdle,
           "1500m_race" = x1500m,
           javlin = javeline
           ) %>%
    pivot_longer("100m_sprint":"1500m_race", names_to = "event", values_to = "event_points")

# writing clean data to rds document
write_rds(clean_data, "clean_data/clean_data.rds")
