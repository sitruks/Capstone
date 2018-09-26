###exploratory scripts for Capstone

Packages <- c("readxl", "tidyverse", "tidytext", "stringr", "ggmap", "naniar", "data.table")
lapply(Packages, library, character.only = TRUE)

###load main files
tour_detail_res <- read.csv("Data/NCAATourneyDetailedResults.csv")
seas_detail_res <- read.csv("Data/RegularSeasonDetailedResults.csv")
seeds <- read.csv("Data/NCAATourneySeeds.csv")
game_cities <- read_csv("Data/GameCities.csv")
tour_games <- read_csv("Data/ConferenceTourneyGames.csv")
teams_spellings <- read_csv("Data/TeamSpellings.csv")
cities <- read_csv("Data/Cities.csv")
stateCodes <- read.csv("Data/stateCodes.csv")

venues <- read_csv("Data/Venues_Clean_w_Coords.csv")
venues_all <- read_csv("Data/Venues_all_w_Coords.csv")

###files for formatting
# seasons <- read.csv("Data/Seasons.csv")
# tour_slots <- read_csv("Data/NCAATourneySlots.csv")
# conferences <- read.csv("Data/Conferences.csv")
# coaches <- read.csv("Data/TeamCoaches.csv")
# team_conferences <- read.csv("Data/TeamConferences.csv")
# teams <- read.csv("Data/Teams.csv")

###supplementary files outside of model
# tour_compact_res <- read.csv("Data/NCAATourneyCompactResults.csv")
# seas_compact_res <- read.csv("Data/RegularSeasonCompactResults.csv")
# second_tour_compact_res <- read_csv("Data/SecondaryTourneyCompactResults.csv")
# second_tour_teams <- read_csv("Data/SecondaryTourneyTeams.csv")

###combine files into one list, NCAA_Data, to ease exploratory data ... quite large
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

#teams_cleaning <- teams %>%  select(-FirstD1Season, -LastD1Season)

venues_cleaning <- venues %>% 
  separate_rows(`Opening rounds`) %>% 
  separate_rows(Regionals) %>% 
  separate_rows(`Final Four`)
venues_cleaning$Address <- str_replace(venues_cleaning$Address, ", District of Columbia,", ",")

tourney_venues <- gather(venues_cleaning, `Opening rounds`, Regionals, `Final Four`, key = "Round", value = "Round Year") %>%
  drop_na(`Round Year`) %>% filter(grepl("[[:digit:]]", `Round Year`)) %>%  unique()
tourney_venues <- mutate(tourney_venues, Address_edit = tourney_venues$Address) %>% 
  separate(Address_edit, into = c("Tourney_City", "Tourney_State", "Tourney_Stadium"), sep = ",")
tourney_venues$Tourney_State <- str_replace(tourney_venues$Tourney_State, "D.C.", "DC")
tourney_venues$Tourney_State <-str_trim(tourney_venues$Tourney_State)
tourney_venues$Tourney_Stadium <-str_trim(tourney_venues$Tourney_Stadium)
stateCodes$State <-str_trim(stateCodes$State)

tourney_venues2 <- left_join(tourney_venues, stateCodes, by = c("Tourney_State" = "State"))

colnames(tourney_venues2) <- paste("Tourney", colnames(tourney_venues2), sep = "_")

# make file for all ncaa venue addresses, not just the tourney. 
# to be leveraged for determining proximity from home site.
options(max.print = 1000000)

ncaa_venues_test <- read_csv("Data/Venues_All.csv")

lat_arr <- rep(NA,length(ncaa_venues_test$Address))
lon_arr <- rep(NA,length(ncaa_venues_test$Address))

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

##edit venues_all to elicit team id number
venues_all <- read_csv("Data/Venues_all_w_Coords.csv")
venues_all <- mutate(venues_all, Team_Match = venues_all$Team)
venues_all$Team_Match <- tolower(venues_all$Team_Match)
venues_all$Team_Match <- gsub(" men", "", venues_all$Team_Match)
venues_all$Team_Match <- gsub(" women", "", venues_all$Team_Match)
venues_all$Team_Match <- gsub(" womens", "", venues_all$Team_Match)
venues_all$Team_Match <- str_replace(venues_all$Team_Match, "kentucky\\[i\\]", "kentucky")
venues_all$Team_Match <- str_replace(venues_all$Team_Match, "seattle redhawks's basketball", "seattle u")
venues_all$Team_Match <- str_replace(venues_all$Team_Match, "gardner–webb", "gardner webb")
venues_all$Team_Match <- str_replace(venues_all$Team_Match, "loyola \\(chicago\\)", "loyola chicago")
venues_all$Team_Match <- str_replace(venues_all$Team_Match, "loyola \\(maryland\\)", "loyola maryland")
venues_all$Team_Match <- str_replace(venues_all$Team_Match, "maryland eastern shore", "maryland-eastern shore")
venues_all$Team_Match <- str_replace(venues_all$Team_Match, "purdue fort wayne", "indiana - purdue")
venues_all$Team_Match <- str_replace(venues_all$Team_Match, "louisiana–monroe", "louisiana-monroe")
venues_all$Team_Match <- str_replace(venues_all$Team_Match, "texas a&m–corpus christi islanders", "texas a&m-corpus christi")
venues_all$Team_Match <- str_replace(venues_all$Team_Match, "idaho vandals", "idaho") 
colnames(venues_all) <- paste("Home", colnames(venues_all), sep = "_")

venues_all_id <- left_join(venues_all, teams_spellings, by = c("Home_Team_Match"="TeamNameSpelling"))
#setnames(venues_all_id, old = c("Lat", "Lon"), new = c("Home_Lat", "Home_Lon"))


###append regular and tournament results
#full_detail_res <- bind_rows(seas_detail_res, tour_detail_res)
#full_detail_res$WLoc <- as.factor(full_detail_res$WLoc)

### begin building master


df1 <- gather(tour_detail_res, `WTeamID`, `LTeamID`, key = "TeamID Type", value = "TeamID")
df2 <- left_join(df1, seed_reg, by = c("Season","TeamID"))
df3 <- left_join(df2, game_cities_cleaning, by = c("Season", "DayNum", "TeamID"))
df4 <- left_join(df3, cities, by = "CityID")
df5 <- left_join(df4, unique(tour_games_cleaning), by = c("Season","TeamID"))
df6 <- left_join(df5, teams_spellings, by = "TeamID")
df7 <- left_join(df6, venues_all_id, by = "TeamID")
df8 <- left_join(df7, tourney_venues2, by = c("Home_Arena" = "Tourney_Tourney_Stadium"))



write.csv(df8,'Data/df8.csv')
