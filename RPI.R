# General RPI file

# Libraries
library(tidyverse)

# Overarching function to calculate RPI
calculate_rpi <- function(schedule = current_year_mbb_schedule) {

#' Variables to include in model
#' Win Margin
#' * Capped at  +/- 30 for basketball, +/- 10 for everything else head to head
#' Home/Away/Neutral
#' Classification Difference
#' * Reward for playing someone bigger, no penalty for playing smaller

# Function to convert vector of "W" and "L" to a win percent
convert_w_l_vec <- function(w_l_vec_input = c()) {
  
  # If there's not a W in the w_l_vec_input, return 0
  # Also works if w_l_vec_input is empty
  if(!"W" %in% w_l_vec_input) {return(0)} else{
    
    # Otherwise calculate the win%
    w_l_vec_input %>% 
      table() %>% 
      prop.table() %>% 
      as.data.frame() %>% 
      filter(. == "W") %>% 
      pull(Freq) %>% 
      return()
  }
  
}

# Function to pull a win/loss vec from a vector of game_ids
pull_w_l_vec <- function(game_id_vec_input = opponents_game_ids,
                         team_id_input = "42XLF4",
                         schedule_input = schedule) {
  schedule_input %>% 
    # Filter to specified games
    filter(event_id %in% game_id_vec_input) %>%
    # Filter to when team_id is not in that game
    filter(opponent_school_id != team_id_input) %>% 
    # Pull the w_l_vec
    pull(result_code) %>% 
    # Return
    return()
}

# Function to find a team's w_l vec
find_team_opponents <- function(team_id_input = "42XLF4",
                                schedule_input = schedule) {
  # Take current year's data
  schedule_input %>% 
    filter(school_id %in% team_id_input) %>% 
    # Don't want to reward team_id_input for losing a game!
    filter(!(opponent_school_id %in% team_id_input)) %>% 
    pull(opponent_school_id) %>% 
    return()
}

# Function to Find team's game ids
find_team_game_ids <- function(team_id_input = "42XLF4",
                               schedule_input = schedule) {
  # Take current year's data
  schedule_input %>% 
    # Filter to team's games
    filter(school_id %in% team_id_input) %>% 
    # Pull the game_ids
    pull(event_id) %>% 
    # Return
    return()
}

# Function to Find opponent's game_ids
find_opp_game_ids <- function(team_id_input = "42XLF4",
                              schedule_input = schedule) {
  
  # First thing is find out who the opponents are
  opponents_vec <- find_team_opponents(team_id_input)
  
  # Find out what games those teams played
  schedule_input %>% 
    # Games where the opponent is the main team but not against specified opponent
    filter(team_school_id %in% opponents_vec & opponent_school_id != team_id_input) %>% 
    # Pull the game IDs
    return()
}

# Function to Calculate team_win_pct
calculate_team_win_pct <- function(team_id_input = "V6NHFM") {
  team_id_input %>% 
    find_team_game_ids(.) %>% 
    pull_w_l_vec(., team_id_input = team_id_input) %>% 
    convert_w_l_vec(.) %>% 
    return()
}

# Calculate opp_win_pct
calculate_opp_win_pct <- function(team_id_input = "42XLF4") {
  
  team_id_input %>% 
    find_team_opponents(.) %>% 
    find_team_game_ids(.) %>% 
    pull_w_l_vec(., team_id_input = team_id_input) %>% 
    convert_w_l_vec(.) %>% 
    return()
}

# Function to Calculate opp_opp_win_pct
calculate_opp_opp_win_pct <- function(team_id_input = "42XLF4") {
  team_id_input %>% 
    # Find the opponents (excluding team_id_input)
    find_team_opponents(.) %>% 
    # Find the opponents' opponents (excluding team_id_input and opponent at 2nd level)
    find_team_opponents(.) %>% 
    find_team_game_ids(.) %>% 
    pull_w_l_vec(., team_id_input = team_id_input) %>% 
    convert_w_l_vec(.) %>% 
    return()
}

# Calculate regular wins and losses
calculate_w_l <- function(team_id_input = "V6NHFM",
                          outcome = "W",
                          schedule_input = schedule) {
  
  game_results <- schedule_input %>% 
    # Filter to when team_id is not in that game
    filter(school_id == team_id_input) %>% 
    pull(result_code)
  
  return(sum(game_results == outcome))
}

# Calculate all the win percentages and RPI
rpi_df <- data.frame(school_id = unique(schedule$school_id)) %>%
  # First calculate wins and losses
  mutate(W = map_dbl(school_id, ~ calculate_w_l(., "W")),
         L = map_dbl(school_id, ~ calculate_w_l(., "L"))) %>% 
  # Calculate win percentages
  mutate(win_pct = map_dbl(school_id, ~ calculate_team_win_pct(.)),
         opp_win_pct = map_dbl(school_id, ~ calculate_opp_win_pct(.)),
         opp_opp_win_pct = map_dbl(school_id, ~ calculate_opp_opp_win_pct(.))) %>% 
  # Calculate RPI according to bestfitsportsdata
  # https://www.bestfitsportsdata.com/alabama_rpi/how_it_works.html
  mutate(rpi = .4 * win_pct + .35 * opp_win_pct + .25 * opp_opp_win_pct) %>% 
  # Arrange from greatest to least
  arrange(desc(rpi))

# Return rpi_df
return(rpi_df)
}

