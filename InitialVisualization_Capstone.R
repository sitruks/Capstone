# exploratory scripts for Capstone

Packages <- c("readxl", "tidyverse", "tidytext")
lapply(Packages, library, character.only = TRUE)

#how to load multiple csv files in the same folder. assign the file name minus ".csv".
# is this a loop with lists?
# I would like to learn this trick since I believe being able to load data as quick as possible is important.
# files <- list.files(path = "C:/Users/v-kursan/Downloads/Capstone_DS/Data/", pattern = "*.csv")


Cities <- read_csv("Data/Cities.csv")
Conferences <- read_csv("Data/Conferences.csv")
ConferenceTourneyGames <- read_csv("Data/ConferenceTourneyGames.csv")
GameCities <- read_csv("Data/GameCities.csv")
NCAATourneyCompactResults <- read_csv("Data/NCAATourneyCompactResults.csv")
NCAATourneyDetailedResults <- read_csv("Data/NCAATourneyDetailedResults.csv")
NCAATourneySeeds <- read_csv("Data/NCAATourneySeeds.csv")
NCAATourneySlots <- read_csv("Data/NCAATourneySlots.csv")
#RegularSeasonCompactResults <- read_csv("Data/RegularSeasonCompactResults.csv")
#RegularSeasonDetailedResults <- read_csv("Data/RegularSeasonDetailedResults.csv")
Seasons <- read_csv("Data/Seasons.csv")
SecondaryTourneyCompactResults <- read_csv("Data/SecondaryTourneyCompactResults.csv")
SecondaryTourneyTeams <- read_csv("Data/SecondaryTourneyTeams.csv")
TeamCoaches <- read_csv("Data/TeamCoaches.csv")
TeamConferences <- read_csv("Data/TeamConferences.csv")
Teams <- read_csv("Data/Teams.csv")
TeamSpellings <- read_csv("Data/TeamSpellings.csv")
TourneyVenues <- read_csv("Data/TourneyVenues.csv")

