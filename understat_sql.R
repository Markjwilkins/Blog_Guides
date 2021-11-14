# Load packages -----------------------------------------------------------
library(tidyverse)
library(glue)
library(progress) ##install.packages("progress")
library(DBI) ##install.packages("DBI")
library(RSQLite) ##install.packages("RSQLite")
library(here) ##install.packages("here")

##league names ----
leagues <- c("bundesliga",
             "epl",
             "la_liga",
             "ligue_1",
             "serie_a")

##years ----
year <- 2014:2020

##create league and year combinations ----
data <- crossing(leagues, year) %>%
  mutate(join = glue::glue("{leagues}_{year}"))

##create progress bar ----
pb <- progress::progress_bar$new(total = length(data$join),
                                 format = "Downloading :[:bar] :percent eta: :eta")

##create function to pull understat all .csv files ----
csv_files <- function(file_name) {
  pb$tick() ##show progress 
  readr::read_csv(url(
    glue::glue('https://raw.githubusercontent.com/Markjwilkins/Understat/main/Shot%20Data%20by%20Year/{file_name}.csv'
    )
  ))
}

##run function for each csv creating a list ----
df <- data$join %>%
  set_names(.) %>%
  map(csv_files)

# Create SQL db -----------------------------------------------------------

##check where .db will be saved ----
here::here()

##create .db ---- 
con <- dbConnect(drv = RSQLite::SQLite(),
                 here::here("understat_shot.db"))

##copy understat data to new .db ----
purrr::map2(data$join, df, ~ dbWriteTable(con, .x, .y))

##check data exists in .db ----
dbListTables(con) 

##query .db ----
x <- tbl(con, "epl_2018")

##query .db and show SQL output ----
x <- tbl(con, "epl_2018") %>% 
  filter(result == "Goal",
         player == "Paul Pogba") %>% 
  select(player, result, x, y, x_g, shot_type) %>% 
  show_query()

##all data ----
tables <- dbListTables(con) 

data <- map_df(tables, dbReadTable, conn = con)

data

##close .db connection
dbDisconnect(con)
