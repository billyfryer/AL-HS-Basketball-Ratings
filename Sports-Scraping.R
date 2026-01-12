### Competitions

# Libraries
library(tidyverse)
library(jsonlite)

json <- read_json("https://maxinfosite-api-live.dragonflyathletics.com/sites/AHSAA/config",
                  simplifyVector = TRUE)

competition_information <- json$competitionLevels %>% 
  # Flatten to a dataframe
  flatten() %>% 
  # Clean Names
  janitor::clean_names() %>%
  # Spread out the different classifications
  unnest_longer(applies_to_values) %>% 
  # Spread out the sport codes into different rows
  unnest_longer(sport_codes) %>% 
  # Unnest longer values
  unnest_longer(values) %>% 
  # Make a column that is all that information put together
  mutate(sport_class_division = paste(sport_codes,
                                      "Class",
                                      applies_to_values,
                                      name,
                                      values)) %>% 
  # Select down to only needed columns
  select(sport_code = sport_codes, class = applies_to_values, 
         subdivision_name = name, subdivision_value = values, sport_class_division)
  

# Clean Environment
rm(json)

# Year is the year that starts the school year, even for spring sports
# EX: 2024-25 Golf, year = 2024; 2023-2024 Basketball, year = 2023


### Sport Code Definitions
#' MBB/WBB - Basketball
#' MBA - Baseball
#' WSB - Softball
#' MSO/WSO - Soccer
#' MBO/WBW - Men's Bowling
#' MFB - Football
#' MWR - Men's Wrestling (??? Women's)
#' MTE/WTE - Tennis
#' MGO/WGO - Golf
#' WVB - Volleyball
#' WFL - Women's Flag Football

### Sports Not Included
#' MTO/WTO - ???
#' MSW - ???
#' MTI/WTI - ???
#' #' MCC/WCC - Cross Country - Not included
