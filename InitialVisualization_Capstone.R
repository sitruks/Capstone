# exploratory scripts for Capstone

Packages <- c("readxl", "tidyverse", "tidytext")
lapply(Packages, library, character.only = TRUE)

#load files

teams <- read.csv("Data/Teams.csv")
seasons <- read.csv("Data/Seasons.csv")
seeds <- read.csv("Data/NCAATourneySeeds.csv")
conferences <- read.csv("Data/Conferences.csv")
coaches <- read.csv("Data/TeamCoaches.csv")
team_conferences <- read.csv("Data/TeamConferences.csv")
tour_compact_res <- read.csv("Data/NCAATourneyCompactResults.csv")
tour_detail_res <- read.csv("Data/NCAATourneyDetailedResults.csv")
seas_compact_res <- read.csv("Data/RegularSeasonCompactResults.csv")
seas_detail_res <- read.csv("Data/RegularSeasonDetailedResults.csv")
cities <- read_csv("Data/Cities.csv")
game_cities <- read_csv("Data/GameCities.csv")
tour_games <- read_csv("Data/ConferenceTourneyGames.csv")
tour_slots <- read_csv("Data/NCAATourneySlots.csv")
second_tour_compact_res <- read_csv("Data/SecondaryTourneyCompactResults.csv")
second_tour_teams <- read_csv("Data/SecondaryTourneyTeams.csv")
teams_spellings <- read_csv("Data/TeamSpellings.csv")
venues <- read_csv("Data/Venues_Clean_w_Coords.csv")

#combine files into one list, NCAA_Data, to ease exploratory data
NCAA_Data <- c(teams, seasons, seeds, conferences, coaches, team_conferences, tour_compact_res, tour_detail_res, seas_compact_res, seas_detail_res, cities, game_cities, tour_games, tour_slots ,second_tour_compact_res, second_tour_teams, teams_spellings, venues)
#NCAA_Data2 <- c("teams", "seasons", "seeds", "conferences", "coaches", "team_conferences", "tour_compact_res", "tour_detail_res", "seas_compact_res", "seas_detail_res", "cities", "game_cities", "tour_games", "tour_slots" ,"second_tour_compact_res", "second_tour_teams", "teams_spellings", "venues")

lapply(NCAA_Data, str)
lapply(NCAA_Data, head) #every column
#lapply(NCAA_Data, unique) #every column detail... 

NCAA_Model <- c(teams, seasons, seeds, conferences, coaches, team_conferences, tour_compact_res, tour_detail_res, seas_compact_res, seas_detail_res, cities, game_cities, tour_games, tour_slots, teams_spellings, venues)

