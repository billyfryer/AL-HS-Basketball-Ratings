# Libraries ----
library(tidyverse)
library(jsonlite)
library(janitor)

# Source Files ----
source("Schools-Scraping.R") # Information on schools (logos, address, etc)
source("Sports-Scraping.R") # Not particularly sure if I need this but I have it
source("Scoreboard-Scraping.R") # Loads Function for scraping per sport
source("RPI.R") # Loads a function for calculating RPI

# Get Current Season ----

## If it's before July, it's the previous year
season <- ifelse(month(Sys.Date()) < 7, 
                 year(Sys.Date()) - 1,
                 year(Sys.Date()))

season_string <- ifelse(month(Sys.Date()) < 7, 
                        paste0(year(Sys.Date()) - 1, "-", year(Sys.Date())),
                        paste0(year(Sys.Date()), "-", year(Sys.Date()) + 1))

# Scrape MBB ----
current_year_mbb_schedule <- scrape_scoreboard("MBB_Varsity", year = season)

mbb_rpi <- calculate_rpi(current_year_mbb_schedule) %>% 
  # Join with school info
  left_join(school_information, .,
            by = join_by("school_id")) %>% 
  # Filter and arrange rows
  # Minimum 10 games played
  filter(!is.na(rpi) & (W + L >= 10)) %>% 
  arrange(desc(rpi))
  


# Scrape WBB ----
current_year_wbb_schedule <- scrape_scoreboard("WBB_Varsity", year = season)

wbb_rpi <- calculate_rpi(current_year_wbb_schedule) %>% 
  # Join with school info
  left_join(school_information, .,
            by = join_by("school_id")) %>% 
  # Filter and arrange rows
  filter(!is.na(rpi) & (W + L >= 10)) %>% 
  arrange(desc(rpi))

# Sample Code for GT Tables----

# library(gt)
# library(gtExtras)
# wbb_rpi %>% 
#   head(10) %>% 
#   select(school_name, logo_url, W, L, win_pct, opp_win_pct, opp_opp_win_pct, rpi) %>% 
#   gt() %>% 
#   tab_header("AHSAA WBB RPI",
#              subtitle = "Minimum 10 Games Played") %>% 
#   tab_footnote(footnote = "Data from Dragonfly (Does Not Consider Out of State Opponents)") %>% 
#   tab_footnote(footnote = "RPI Formula: (.4 * Win%) + (.35 * Opp Win%) + (.25 * Opp Opp Win%)") %>% 
#   cols_label(school_name = "School",
#              logo_url = "",
#              win_pct = "Win%",
#              opp_win_pct = "Opp Win%",
#              opp_opp_win_pct = "Opp Opp Win%",
#              rpi = "RPI") %>% 
#   gt_img_rows(columns = logo_url) %>% 
#   fmt_percent(columns = ends_with("pct"),
#               decimals = 1) %>% 
#   fmt_number(columns = rpi,
#              decimals = 3) %>% 
#   gt_theme_espn() %>% 
#   tab_options(table.align = "center",
#               heading.align = "center") %>% 
#   cols_align("center")

# Write RPI tables to CSVs ----

write.csv(mbb_rpi, "mbb_rpi.csv", row.names = FALSE)
write.csv(wbb_rpi, "wbb_rpi.csv", row.names = FALSE)
