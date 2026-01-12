### Schools Index

# Libraries
library(tidyverse)
library(jsonlite)

# Read the json
json <- read_json("https://maxinfosite-api-live.dragonflyathletics.com/states/AHSAA/directory/1",
                  simplifyVector = TRUE)

# Pull out just the results
raw_results <- json$results

# Clean up the results
full_school_information <- raw_results %>% 
  # Flatten the results
  flatten() %>% 
  # Unnest the media
  unnest_wider(media) %>% 
  # Deselect Colors
  select(-heraldry.colors) %>% 
  # Convert to a data.frame
  data.frame() %>% 
  # Fix Variable Names again
  janitor::clean_names() %>% 
  # Filter to only AL Schools
  filter(state_code == "AL") %>% 
  # Fix city and places where it's listed as "undefined"
  mutate(city = str_squish(str_to_title(city)),
         mascot = str_squish(str_to_title(heraldry_mascot)),
         across(.cols = starts_with("competition_levels_ahsaa"),
                ~ case_when(.x == "undefined" ~ NA,
                                 TRUE ~ .x)),
         logo_url = x_url
         ) %>%
  # Replace "" with NAs
  mutate(across(.cols = everything(),
                ~ case_when(.x == "" ~ NA,
                               TRUE ~ .x))) 

# Get School Information in 1 table
school_information <- full_school_information %>% 
  # Select non-changing information about the school
  select(school_id = short_code,
         school_name = name,
         city,
         state = state_code,
         mascot,
         address,
         logo_url)

# Not sure how this fits in with the rest of the information so 
# leaving it out for now

# # Get school classification in another
# school_classification <- full_school_information %>% 
#   #TODO: Needs to be changed yearly
#   # Mutate on the school year
#   mutate(season_year = "2024-2025") %>% 
#   # Select information about classifications
#   select(school_id = short_code,
#          season_year,
#          contains('competition_levels_')
#          )
# 
# # Make variable names shorts for school_classifications
# names(school_classification) <- str_remove_all(string = names(school_classification),
#                                                pattern = 'competition_levels_')

# Clean Environment
rm(raw_results, json, full_school_information)