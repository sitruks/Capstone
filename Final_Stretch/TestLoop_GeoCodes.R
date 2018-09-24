
#This script pulls lat and lon coordinates for all game venues using ggmap Google map APIs

library(tidyverse)
library(ggmap)
library(naniar)
options(max.print = 1000000)

#https://en.wikipedia.org/wiki/List_of_NCAA_Men%27s_Division_I_Basketball_Tournament_venues

#TourneyVenues data clean
TourneyVenues <- read_csv("Data/TourneyVenues.csv")

Venues_Clean <- TourneyVenues %>% 
  mutate(State = if_else(is.na(Arena), City, "NA")) %>% 
  replace_with_na(list(State = "NA")) %>% 
  fill(State) %>% 
  filter(!is.na(Arena)) %>% 
  unite(City, State, Arena, col = "Address", sep = ", ")

#Initiate two vectors intially filled with NAs as placeholders for lat and lon
lat_arr <- rep(NA,length(Venues_Clean$Address))
lon_arr <- rep(NA,length(Venues_Clean$Address))

#Start the looping in postion 1
i = 1

#The following while loop executes as long as there are still
# NA values in the lat or lon array
while (sum(is.na(lat_arr)) > 0 & sum(is.na(lon_arr)) > 0)
{
  
  # Check if at the current ith position, if we still got an NA value for the lat/lon
  # if yes, let's get the venue address, feed into the geocode() function to attempt
  # again to get the lat and lon
  if (is.na(lat_arr[i])){
    print (i)
    address <- Venues_Clean$Address[i]
    latlon <- geocode(address)
    lat_arr[i] <- latlon[[1]]
    lon_arr[i] <- latlon[[2]]
  }
  
  # Otherwise, the lat/lon has been pulled sccuessfully for ith position
  # therefore, we move on to the next position by increasing i by 1
  i <- i + 1
  
  # If we reached to the end of the array, let's restart back from position 1
  # it takes about 3 completed cycles to get all the locations coordiated pulled up
  if (i > length(Venues_Clean$Address)){
    
    i <- 1
  }
  
}

# Add the lat and lon arrays to the dataframe as new columns
Venues_Clean$Lat <- lat_arr
Venues_Clean$Lon <- lon_arr

write.csv(Venues_Clean,'Data/Venues_Clean_w_Coords.csv')


#optional extra cleaning
Venues_Clean %>% 
  separate_rows(`Opening rounds`) %>% 
  separate_rows(Regionals) %>% 
  separate_rows(`Final Four`)
