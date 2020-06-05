

# loading libraries
library(tidyverse)
library(janitor)
library(readr)
library(here)


# loading in data
raw_cake_recipe <-
    here("raw_data/cake/cake-ingredients-1961.csv") %>% 
    read_csv()


raw_ingredients_conversion <-
    here("raw_data/cake/cake_ingredient_code.csv") %>% 
    read_csv()

# tidying data and replacing ingredient codes with full names

clean_cake_recipe <-
    raw_cake_recipe %>%
    pivot_longer(-Cake, names_to = "code", values_to = "amount") %>%
    rename(cake = Cake) %>%
    left_join(raw_ingredients_conversion) %>%
    drop_na(amount) %>%
    mutate(cake = str_trim(cake)) %>%
    select(-code)

# write clean data to a csv file
write_csv(clean_cake_recipe, "clean_data/cake_recipes.csv")

