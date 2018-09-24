library(tidyverse)
library(ggmap)
library(naniar)

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

ncaa_venues$Lat <- lat_arr
ncaa_venues$Lon <- lon_arr 