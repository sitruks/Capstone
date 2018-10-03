
##master table build

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

### data cleaning

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

game_cities_cleaning <- gather(game_cities, `WTeamID`, `LTeamID`, key = "TeamID Type", value = "TeamID")

tour_games_cleaning <- gather(tour_games, `WTeamID`, `LTeamID`, key = "TeamID Type", value = "TeamID") %>% 
  select(-DayNum, -`TeamID Type`)

## tournament venues cleaning
venues_cleaning <- venues %>% 
  separate_rows(`Opening rounds`) %>% 
  separate_rows(Regionals) %>% 
  separate_rows(`Final Four`)
venues_cleaning$Address <- str_replace(venues_cleaning$Address, ", District of Columbia,", ",")
tourney_venues <- gather(venues_cleaning, `Opening rounds`, Regionals, `Final Four`, key = "Round", value = "Round Year") %>%
  drop_na(`Round Year`) %>% filter(grepl("[[:digit:]]", `Round Year`)) %>%  unique()
tourney_venues <- mutate(tourney_venues, Address_edit = tourney_venues$Address) %>% 
  separate(Address_edit, into = c("Tourney_City", "Tourney_State", "Tourney_Stadium"), sep = ",")
tourney_venues$Tourney_State <- str_replace(tourney_venues$Tourney_State, "D.C.", "District of Columbia")
tourney_venues$Tourney_State <-str_trim(tourney_venues$Tourney_State)
tourney_venues$Tourney_Stadium <-str_trim(tourney_venues$Tourney_Stadium)
stateCodes$State <-str_trim(stateCodes$State)

tourney_venues2 <- left_join(tourney_venues, stateCodes, by = c("Tourney_State" = "State"))
setnames(tourney_venues2, old = c("Address","Lat", "Lon", "Round", "Round Year", "Postal.code"), 
         new = c("Tourney_Address", "Tourney_Lat", "Tourney_Lon", "Tourney_Round", "Tourney_Round_Year", "Tourney_Postal_Code"))
tourney_venues2$Tourney_Round_Year <- as.integer(tourney_venues2$Tourney_Round_Year)
tourney_venues2$Tourney_City <- str_replace(tourney_venues2$Tourney_City, "New York City \\(Brooklyn\\)", "Brooklyn")
tourney_venues2$Tourney_City <- str_replace(tourney_venues2$Tourney_City, "New York City \\(Manhattan\\)", "New York")
tourney_venues2$Tourney_Postal_Code<- replace_na(tourney_venues2$Tourney_Postal_Code, "DC")
tourney_venues2 <-rbind(tourney_venues2, c("134", "Dayton, Ohio, University of Dayton Arena", "-84.19972", "39.73561", "Play-in rounds", "2017", "Dayton", "Ohio", "University of Dayton Arena", "OH"))
tourney_venues2 <-rbind(tourney_venues2, c("134", "Dayton, Ohio, University of Dayton Arena", "-84.19972", "39.73561", "Play-in rounds", "2016", "Dayton", "Ohio", "University of Dayton Arena", "OH"))
tourney_venues2 <-rbind(tourney_venues2, c("134", "Dayton, Ohio, University of Dayton Arena", "-84.19972", "39.73561", "Play-in rounds", "2015", "Dayton", "Ohio", "University of Dayton Arena", "OH"))
tourney_venues2 <-rbind(tourney_venues2, c("134", "Dayton, Ohio, University of Dayton Arena", "-84.19972", "39.73561", "Play-in rounds", "2014", "Dayton", "Ohio", "University of Dayton Arena", "OH"))
tourney_venues2 <-rbind(tourney_venues2, c("134", "Dayton, Ohio, University of Dayton Arena", "-84.19972", "39.73561", "Play-in rounds", "2012", "Dayton", "Ohio", "University of Dayton Arena", "OH"))
tourney_venues2 <-rbind(tourney_venues2, c("134", "Dayton, Ohio, University of Dayton Arena", "-84.19972", "39.73561", "Play-in rounds", "2011", "Dayton", "Ohio", "University of Dayton Arena", "OH"))
tourney_venues2 <-rbind(tourney_venues2, c("134", "Dayton, Ohio, University of Dayton Arena", "-84.19972", "39.73561", "Play-in rounds", "2010", "Dayton", "Ohio", "University of Dayton Arena", "OH"))

##edit venues_all to elicit team id number
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


### begin building master
df1 <- gather(tour_detail_res, `WTeamID`, `LTeamID`, key = "TeamID Type", value = "TeamID")
df2 <- left_join(df1, seed_reg, by = c("Season","TeamID"))
df3 <- left_join(df2, game_cities_cleaning, by = c("Season", "DayNum", "TeamID"))
df4 <- left_join(df3, cities, by = "CityID")
df5 <- left_join(df4, unique(tour_games_cleaning), by = c("Season","TeamID"))
df6 <- left_join(df5, teams_spellings, by = "TeamID")
df7 <- left_join(df6, venues_all_id, by = "TeamID")
df8 <- left_join(df7, tourney_venues2, by = c("Season" = "Tourney_Round_Year", "City" = "Tourney_City", "State" = "Tourney_Postal_Code" ))
df9 <- select(df8,-c(TeamNameSpelling, "TeamID Type.y", Home_X1, X1)) %>% 
  setnames(old = c("TeamID Type.x"), new = c("TeamID Type")) %>% 
  unique() 
df10 <- 
  filter(df9, `Home_Gender Distinction` != "Women" | is.na(df9$`Home_Gender Distinction`)) %>% 
  select(-`Home_Gender Distinction`)
df11$Home_Address <- str_replace(df11$Home_Address, "\\[[:alpha:]\\]", "")
#df11$Home_Address <- str_replace(df11$Home_Address, "\\<[:alpha:]|[:punct:]\\>", "")
df11$Home_Arena <- str_replace(df11$Home_Arena, "\\[[:alpha:]\\]", "")
df11$Home_Arena <- str_replace(df11$Home_Arena, "\\[[:alpha:]\\]", "")
df11$Home_Team <- str_replace(df11$Home_Team, "\\[[:alpha:]\\]", "")
df11$Home_Capacity <- str_replace(df11$Home_Capacity, "\\[[:digit:]\\]", "")
df11$Home_Team_Match <- str_to_title(df11$Home_Team_Match)

#alternate data cleaning attempt
###setkey(ncaa_table, Season, DayNum)
###length(unique(ncaa_table$Home_Team))
###
###e1 <- ncaa_table[, top_seed := as.numeric(Seeding) == 1][, sum(top_seed), by = Home_Team][order(V1, decreasing = TRUE)][1:10,]



### write master
write.csv(df11,'Data/df11.csv')

write.csv(tourney_venues2,'Data/tourney_venues2.csv')
