

#Install Libraries


library(tidyverse)
library(lubridate)
library(sf)
library(readxl)


#########################################################

############ Rhode Island Dept of Env Mgmt  #############

#########################################################

#read in sample data
ri <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Fish Data/jenrogers data request.xls",
                 skip = 56, col_names = FALSE)
str(ri)

#assign column names
names(ri) <- c("basin", "subbasin", "station_number", "date", "station_length", "station_width", "spp_code", "length_mm", 
               "total_num_spp_collected_per_station", "subsample", "total_num_specimens_for_each_spp_collected", 
               "gear", "efish_duration_s", "no_name", "waterbody_roadcrossing", "town", "latitude", "longitude")

#assign a UID
ri$UID <- paste("RI", ri$basin, ri$subbasin, ri$station_number, ri$date, sep = "_")

#prepare the 3 files:; ri_event, ri_fish, ri_method

#ri_event
ri_event <- ri %>% 
  select(UID, date, latitude, longitude, waterbody_roadcrossing) %>% 
  mutate(state = "RI", 
         source = "Allan Liby - RIDEM",
         date2 = round(date, 0),
         date3 = ifelse(date2 < 1000000, paste(19, date2, sep = ""), 
                        ifelse(date2>999999, sub(".", "20", date2), date2)),
         date4 = ymd(date3)) %>%
  separate(waterbody_roadcrossing, into = c("waterbody", "roadcrossing"), sep = "/") %>% 
  select(-date, -date2, -date3, -roadcrossing) %>% 
  rename(date = date4) %>% 
  select(UID, state, date, waterbody, latitude, longitude, source) %>% 
  unique()
