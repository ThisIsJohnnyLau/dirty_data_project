# loading libraries
library(readxl)
library(tidyverse)
library(janitor)


# loading in data
clean_ship_names <-
    read_excel("raw_data/seabirds.xls",
               sheet = "Ship data by record ID") %>%
    # standardising column titles
    clean_names() %>% 
    # discarding unhelpful columns
    select(-sdir, -wdir, -atmp, -aprs, -stmp, -sal, -depth, -time) %>%
    # fixing data type for date column
    mutate(date = as.Date(date))


clean_bird_names <-
    read_excel("raw_data/seabirds.xls",
               sheet = "Bird data by record ID") %>%
    #standardising column titles
    clean_names() %>%
    #removing useless columns
    select(-age, -wanplum, -plphase, -sex)

# Joining datatables
full_data <-
    full_join(clean_bird_names, clean_ship_names, by = "record_id") %>% 
    select(record_id,date,species_common_name_taxon_age_sex_plumage_phase:count, lat:long)



# Tidying final dataset
full_data %>%
    pivot_longer(-record_id:-species_abbreviation,
                 names_to = "info",
                 values_to = "value")



# writing clean data to csv
write_csv(full_data, "clean_data/birds_clean.csv")