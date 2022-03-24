

#Install Libraries


library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(measurements)
library(stringi)


#########################################################

############ Rhode Island Dept of Env Mgmt  #############

#########################################################

#read in sample data
ri <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Fish Data/jenrogers data request.xls",
                 skip = 56, col_names = FALSE)

#assign column names
names(ri) <- c("basin", "subbasin", "station_number", "date", "station_length", "station_width", "spp_code", "length_mm", 
               "total_num_spp_collected_per_station", "subsample", "total_num_specimens_for_each_spp_collected", 
               "gear", "efish_duration_s", "no_name", "waterbody_roadcrossing", "town", "latitude", "longitude")

#assign a UID
ri$UID <- paste("RI", ri$basin, ri$subbasin, ri$station_number, ri$date, sep = "_")



code <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Fish Data/Species Codes.xls")
names(code) <- c("common_name", "scientific_name", "spp_code")




#left join the fish name abbreviation table so we have the common and scientific names

test <- left_join(ri, code, by = "spp_code")




#prepare the 3 files:; ri_event, ri_fish, ri_method

#ri_event
ri_event <- ri %>% 
  select(UID, date, latitude, longitude, waterbody_roadcrossing) %>% 
  mutate(state = "RI", 
         source = "Allan Liby - RIDEM",
         project = "fish_community_RI_streams_Rivers",
         date2 = round(date, 0),
         date3 = ifelse(date2 < 1000000, paste(19, date2, sep = ""), 
                        ifelse(date2>999999, sub(".", "20", date2), date2)),
         date4 = ymd(date3)) %>%
  separate(waterbody_roadcrossing, into = c("waterbody", "roadcrossing"), sep = "/") %>% 
  select(-date, -date2, -date3, -roadcrossing) %>% 
  rename(date = date4) %>% 
  select(UID, state, date, waterbody, latitude, longitude, project, source) %>% 
  unique()


#make all lat long decimal degrees
tmp <- ri_event %>% 
  filter(latitude <100) %>%  #these are the ones that already are in decimal degrees
  mutate(longitude = ifelse(longitude >0, -longitude, longitude)) #make the longitudes all negative

tmp2 <- ri_event %>% 
  filter(latitude > 100)

tmp2 <- tmp2 %>% 
  mutate(latitude2 = stri_sub_replace(latitude, 3, 2, value = " "),
         latitude3 = stri_sub_replace(latitude2, 6, 5, value = " "),
         longitude2 = stri_sub_replace(longitude, 3, 2, value = " "),
         longitude3 = stri_sub_replace(longitude2, 6, 5, value = " "))


tmp2$latitudefinal <- conv_unit(tmp2$latitude3, from = "deg_min_sec", to= "dec_deg")
tmp2$longitudefinal <- conv_unit(tmp2$longitude3, from = "deg_min_sec", to= "dec_deg")

tmp2 <- tmp2 %>% 
  select(UID, state, date, waterbody, latitudefinal, longitudefinal, project, source) %>% 
  rename(latitude = latitudefinal, 
         longitude = longitudefinal) %>% 
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         longitude = ifelse(longitude > 0, -longitude, longitude))

ri_event <- rbind(tmp, tmp2)
