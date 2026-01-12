### Scoreboard Scraping

# Libraries
library(tidyverse)
library(jsonlite)

scrape_scoreboard <- function(sport = "WBB_Varsity", year = 2025) {

  # Initialize has_next_page, i and empty schedule Data.Frame
  has_next_page <- TRUE
  i <- 1
  full_schedule <- data.frame()
  
  # While Loop for iteration over full schedule
  while (has_next_page) {
  # Sleep 3
  Sys.sleep(3)
  
  # Create the url
  url <- paste0("https://maxinfosite-api-live.dragonflyathletics.com/states/AHSAA/schedules/",
                year, "/", sport, "/", i)
  
  # Read the JSON
  json <- jsonlite::read_json(url)
  
  partial_schedule <- json$schedule %>%
    # Convert to a tibble (which puts the columns as lists instead of DFs)
    tibble::tibble() |> 
    # Unnest wider
    tidyr::unnest_wider(1) |> 
    # Drop rows where no results are available ie column is null
    dplyr::filter(lengths(results) > 0 | is.na(results))
  
  # If a column name is missing, break
  if(!("date" %in% names(partial_schedule))) {
    break
  }
  
  # Resume cleaning
  partial_schedule <- partial_schedule %>% 
    # Select necessary columns
    dplyr::select(event_id = eventId, event_name = name, date,
                  hostOrg, hostOrgName, participants) |> 
    # 2 rows per game - one for home team, other for away team
    tidyr::unnest_longer(participants) |>
    # Unnest the participants Column to see all the info
    tidyr::unnest_wider(participants) %>%
    # Filter to where result ! NA
    filter(!is.na(result))
  
  # If result is NA
  if(nrow(partial_schedule) == 0) {
    break
  } 
  
  partial_schedule <- partial_schedule %>% 
    # Unnest the scores and team info
    tidyr::unnest_wider(c(team, result), names_sep = "_") %>% 
    # Select necessary information from here
    select(event_id, event_name, school_name = name,
           date, hostOrg, hostOrgName,
           school_short_id = orgShortCode, isHome, school_long_id = team_code,
           result_code, team_score = result_score, opponent_score = result_opponentScore) %>% 
    # Convert score variables to numeric
    # And fix Home/Away/Neutral
    mutate(team_score = as.character(team_score),
           opponent_score = as.character(opponent_score),
           # If the host team is not in the event name and !isHome, team is away
           is_away = str_detect(event_name, hostOrgName) & !isHome,
           # If the team isn't home or away, it is a neutral site game
           is_neutral = case_when(isHome ~ FALSE,
                                  is_away ~ FALSE,
                                 TRUE ~ TRUE))
  
  # Merge to full_schedule
  full_schedule <- bind_rows(full_schedule, partial_schedule) %>% 
    arrange(date)
  
  # Check and see if there is a next page
  has_next_page <- json$hasNextPage
  
  # Print
  print(paste("Scraping", year, sport, "Page", i, "Complete!"))
  
  # Finally iterate i
  i <- i + 1
}
  
  print(paste("Scraping All Pages for", sport, year, "Complete"))
  
  # Cleaning full_schedule
  clean_schedule <- full_schedule %>% 
    # String Cleaning
    mutate(
      # Convert Scores to Numeric
      team_score = suppressWarnings(as.numeric(team_score)),
      opponent_score = suppressWarnings(as.numeric(opponent_score)),
      # Clean Team Names
      event_name = str_squish(str_to_title(event_name)),
      host_name = str_squish(str_to_title(hostOrgName)),
      school_name = str_squish(str_to_title(school_name)),
      # Calculate Win Margin
      raw_win_margin = team_score - opponent_score
      ) %>% 
    # Get rid of NA Scores
    filter(!is.na(team_score) & !is.na(opponent_score)) %>% 
    # Only Distinct Rows
    distinct()
  
  # Attaching opponent ID to each row
  matchup_lookup <- clean_schedule %>% 
    # Select distinct game_id and school id
    select(event_id, school_short_id) %>% 
    distinct() %>% 
    # Assigned each team as team_1 or team_2 for pivoting
    mutate(name = row_number(),
           .by = event_id) %>% 
    pivot_wider(values_from = school_short_id,
                names_prefix = "team_")
  
  # Add the opponent id to clean_schedule
  final_schedule <- clean_schedule %>% 
    # Join the matchup_lookup by event_id
    left_join(., matchup_lookup,
              by = join_by("event_id")) %>% 
    # Mutate opponent id and a season
    mutate(opponent_school_short_id = case_when(team_1 == school_short_id ~ team_2,
                                   team_2 == school_short_id ~ team_1),
           # If it's before July previous_year-current_year
           season_year = case_when(month(date) < 7 ~ paste0(year(date)-1, "-", year(date)),
           # Otherwise, it's current_year-next_year
                                   TRUE ~ paste0(year(date), "-", year(date)+1)
                                   )
           ) %>% 
    # Order and select Columns
    select(event_id, date, season_year,
           school_id = school_short_id, result_code, team_score, 
           opponent_school_id = opponent_school_short_id, opponent_score,
           is_home = isHome, is_away, is_neutral)
  
  # Return
  return(final_schedule)
}

# Test Scrape 2025 MBB Varsity
# MBB_Varsity_Scoreboard_2025 <- scrape_scoreboard("MBB_Varsity", 2025)

# Test Scrape 2025 WBB Varsity
# WBB_Varsity_Scoreboard_2025 <- scrape_scoreboard("WBB_Varsity", 2025)

# MSO_Varsity_Scoreboard_2024 <- scrape_scoreboard("MSO_Varsity", 2024)


#final_schedule %>% filter(school_name == "Shelby County High School") %>% View()
