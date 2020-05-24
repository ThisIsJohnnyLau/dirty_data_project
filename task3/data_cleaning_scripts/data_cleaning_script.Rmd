```{r}

# Hurdle: 
# Fix: 
library(dplyr)
library(tidyverse)
library(janitor)

# Hurdle: 
# Fix: 

clean_ship_names <- clean_names(raw_data$`Ship data by record ID`) %>% 
       select(-sdir, -wdir, -atmp, -aprs, -stmp, -sal, -depth, -time) %>% 
    mutate(date = as.Date(date))


clean_bird_names <- clean_names(raw_data$`Bird data by record ID`) %>% 
    select(-age, -wanplum, -plphase, -sex)

# Hurdle: 
# Fix: 
full_data <-
    full_join(clean_bird_names, clean_ship_names, by = "record_id") %>% 
    select(record_id,date,species_common_name_taxon_age_sex_plumage_phase:count, lat:long)
```

```{r}
# Hurdle: 
# Fix: 


full_data %>%
    pivot_longer(-record_id:-species_abbreviation,
                 names_to = "info",
                 values_to = "value")

```

