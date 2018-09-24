###exploratory scripts for Capstone

Packages <- c("readxl", "tidyverse", "tidytext", "stringr", "ggmap", "naniar")
lapply(Packages, library, character.only = TRUE)

###load main files
tour_detail_res <- read.csv("Data/NCAATourneyDetailedResults.csv")
seas_detail_res <- read.csv("Data/RegularSeasonDetailedResults.csv")
seeds <- read.csv("Data/NCAATourneySeeds.csv")
game_cities <- read_csv("Data/GameCities.csv")
tour_games <- read_csv("Data/ConferenceTourneyGames.csv")
teams <- read.csv("Data/Teams.csv")
cities <- read_csv("Data/Cities.csv")

venues <- read_csv("Data/Venues_Clean_w_Coords.csv")


###files for formatting
# seasons <- read.csv("Data/Seasons.csv")
# teams_spellings <- read_csv("Data/TeamSpellings.csv")
# tour_slots <- read_csv("Data/NCAATourneySlots.csv")
# conferences <- read.csv("Data/Conferences.csv")
# coaches <- read.csv("Data/TeamCoaches.csv")
# team_conferences <- read.csv("Data/TeamConferences.csv")

###supplementary files outside of model
# tour_compact_res <- read.csv("Data/NCAATourneyCompactResults.csv")
# seas_compact_res <- read.csv("Data/RegularSeasonCompactResults.csv")
# second_tour_compact_res <- read_csv("Data/SecondaryTourneyCompactResults.csv")
# second_tour_teams <- read_csv("Data/SecondaryTourneyTeams.csv")

###combine files into one list, NCAA_Data, to ease exploratory data
# NCAA_Data <- c(teams, seasons, seeds, conferences, coaches, team_conferences, tour_compact_res, tour_detail_res, seas_compact_res, seas_detail_res, cities, game_cities, tour_games, tour_slots ,second_tour_compact_res, second_tour_teams, teams_spellings, venues)
# NCAA_Data2 <- c("teams", "seasons", "seeds", "conferences", "coaches", "team_conferences", "tour_compact_res", "tour_detail_res", "seas_compact_res", "seas_detail_res", "cities", "game_cities", "tour_games", "tour_slots" ,"second_tour_compact_res", "second_tour_teams", "teams_spellings", "venues")

#lapply(NCAA_Data, str)
#lapply(NCAA_Data, head) #every column
#lapply(NCAA_Data, unique) #every column detail... 

###cleaning for joins
seed_reg_cleaning <- seeds %>% mutate(Region = 
                                if_else(grepl("^W", seeds$Seed), "W",
                                        if_else(grepl("^X", seeds$Seed), "X",
                                                if_else(grepl("^Y", seeds$Seed), "Y",
                                                        if_else(grepl("^Z", seeds$Seed), "Z",
                                                                "NA")))),
                              Play_In = str_count(seeds$Seed, "a|b"),
                              Seeding = str_sub(seeds$Seed,2,3)
) 
seed_reg_cleaning$Region <- as.factor(seed_reg_cleaning$Region)
seed_reg_cleaning$Seeding <- as.integer(seed_reg_cleaning$Seeding)

seed_reg <- seed_reg_cleaning %>% select(-Seed)

#note that game cities begins at 2010
game_cities_cleaning <- gather(game_cities, `WTeamID`, `LTeamID`, key = "TeamID Type", value = "TeamID") 

tour_games_cleaning <- gather(tour_games, `WTeamID`, `LTeamID`, key = "TeamID Type", value = "TeamID") %>% 
  select(-DayNum, -`TeamID Type`)

teams_cleaning <- teams %>%  select(-FirstD1Season, -LastD1Season)

venues_cleaning <- venues %>% 
  separate_rows(`Opening rounds`) %>% 
  separate_rows(Regionals) %>% 
  separate_rows(`Final Four`)

tourney_venues <- gather(venues_cleaning, `Opening rounds`, Regionals, `Final Four`, 
                           key = "Round", value = "Round Year") %>% 
  drop_na(`Round Year`)  %>% 
  filter(grepl("[[:digit:]]", `Round Year`))

# make file for all ncaa venue addresses, not just the tourney. 
# to be leveraged for determining proximity from home site.
options(max.print = 1000000)

ncaa_venues <- read_csv("Data/Venues_All.csv")

lat_arr <- rep(NA,length(ncaa_venues$Address))
lon_arr <- rep(NA,length(ncaa_venues$Address))

i = 1

while (sum(is.na(lat_arr)) > 0 & sum(is.na(lon_arr)) > 0)
  
{
  if (is.na(lat_arr[i])){
    print (i)
    address <- ncaa_venues$Address[i]
    latlon <- geocode(address)
    lat_arr[i] <- latlon[[1]]
    lon_arr[i] <- latlon[[2]]
  }
  
  i <- i + 1
  
  if (i > length(ncaa_venues$Address)){
    i <- 1
  }
}

# need to insert a conditional clause that stops loop after 50 failures
# actual value is (-85.60845,42.283372)
#%>% replace_na(list(lat_arr = "-85.60845"))
#%>% replace_na(list(lon_arr = "42.283372"))

ncaa_venues$Lat <- lat_arr 
ncaa_venues$Lon <- lon_arr 



write.csv(ncaa_venues,'Data/ncaa_venues.csv')



###append regular and tournament results
#full_detail_res <- bind_rows(seas_detail_res, tour_detail_res)
#full_detail_res$WLoc <- as.factor(full_detail_res$WLoc)

### begin building master


df1 <- gather(tour_detail_res, `WTeamID`, `LTeamID`, key = "TeamID Type", value = "TeamID")
df2 <- left_join(df1, seed_reg, by = c("Season","TeamID"))
df3 <- left_join(df2, game_cities_cleaning, by = c("Season", "DayNum", "TeamID"))
df4 <- left_join(df3, cities, by = "CityID")
df5 <- left_join(df4, unique(tour_games_cleaning), by = c("Season","TeamID"))
df6 <- left_join(df5, teams_cleaning, by = "TeamID")





