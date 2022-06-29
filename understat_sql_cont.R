
# Load packages -----------------------------------------------------------
library(worldfootballR)
library(glue)
library(tidyverse)
library(DBI)
library(RSQLite)
library(here)

##connect to db
con <- dbConnect(drv = RSQLite::SQLite(),
                 here::here("db", "understat_shot.db"))

##leagues
leagues <- c("EPL",
             "La liga",
             "Bundesliga",
             "Serie A",
             "Ligue 1")


# function ----------------------------------------------------------------
get_shots <- function(league_name, season) {
  
  ##print league data being obtained
  print(glue::glue("Pulling data for {league_name} - {season}"))
  
  ##pull data
  dat <- understat_league_season_shots(league = league_name,
                                       season_start_year = season)
  
  ##edit string for saving files
  table_name <- league_name %>% 
    str_replace(., " ","_") %>%
    paste0(., "_", season) %>%
    tolower
  
  ##save to .csv
  write_csv(dat, here::here("data", glue::glue("{table_name}.csv")))
  
  ##write data to SQL db
  dbWriteTable(con,
               name = table_name,
               dat,
               append = TRUE)
}


# pull data ---------------------------------------------------------------
purrr::walk(leagues, ~get_shots(league_name = .x, 
                            season = "2021"),
            con)

##disconnect fron db
dbDisconnect(con)
                 
