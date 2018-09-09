library(tidyverse)
library(ggmap)
library(naniar)

#TourneyVenues data clean
TourneyVenues <- read_csv("Data/TourneyVenues.csv")

options(max.print = 1000000)

Venues_Clean <- TourneyVenues %>% 
  mutate(State = if_else(is.na(Arena), City, "NA")) %>% 
  replace_with_na(list(State = "NA")) %>% 
  fill(State) %>% 
  filter(!is.na(Arena)) %>% 
  unite(City, State, Arena, col = "Address", sep = ", ")
#idea to call enough times to cover the NAs
# if there is a way to slow down the api call maybe it will process better/not get NAs
Venues_Lat_Long <- Venues_Clean %>% mutate_geocode(Address)
Venues_Lat_Long2 <- Venues_Clean %>% mutate_geocode(Address)

#If this worked, i would join all instances
Venues_Join <- inner_join(Venues_Lat_Long,Venues_Lat_Long2)
Venues_NA <- filter(Venues_Join, is.na(lon)) %>% mutate_geocode(Address)
Venues_NA2 <- filter(Venues_Join, is.na(lon)) %>% mutate_geocode(Address)

#continuation of this
Venues_Join2 <- inner_join(Venues_Join, Venues_NA)
Venues_Join3 <- inner_join(Venues_Join2, Venues_NA2)

#the while loop with empty containers i think is the way to go.

#optional extra cleaning
Venues_Clean %>% 
  separate_rows(`Opening rounds`) %>% 
  separate_rows(Regionals) %>% 
  separate_rows(`Final Four`)
