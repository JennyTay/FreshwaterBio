library(tidyverse)
library(lubridate)
library(sf)
library(readxl)



#########################################################

################### Vermont DEC ########################

#########################################################
vt <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT DEC Fish Data/VT DEC Fish Data 01-28-2022 (no TE).xlsx",
                 col_names = TRUE, sheet = "VDEC Fish Data")

fish <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT DEC Fish Data/VT DEC Fish Data 01-28-2022 (no TE).xlsx",
                 col_names = TRUE, sheet = "Fish Library", range = cell_cols("A:I"))
fish <- fish %>% 
  select(FishID, Species)

vt <- left_join(vt, fish, by = "FishID")

range(vt$Date)
str(vt)


#make shapefile the same crs as NHD
#load NHD data
MAflowline <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NDH/Shape/NHDFlowline.shp")


#make the fish dataframe an sf object so it can be plotted spatially
shp <- st_as_sf(x = vt,                         
                coords = c("Longitude (DD)", "Latitude (DD)"),
                crs = st_crs(MAflowline))
st_write(shp, dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT DEC Fish Data/VT DEC Fish Data 01-28-2022 (no TE).shp")



# tidy data
dec_event <- vt %>% 
  select(Date, "Latitude (DD)", "Longitude (DD)", EventID) %>% 
  mutate(source = "VTDEC - JimDeshler",
         UID = paste("VT", EventID, sep = "_"),
         project = "VTDEC",
         state = "VT") %>% 
  rename(latitude = "Latitude (DD)", longitude = "Longitude (DD)", date = Date) %>% 
  select(UID, state, date, latitude, longitude, project, source) %>% 
  unique()

dec_fish <- vt %>% 
  select(EventID, Species, Run1, Run2, Run3) %>% 
  mutate(UID = paste("VT", EventID, sep = "_")) %>% 
  rename(common_name = Species) %>% 
  pivot_longer(cols = 3:5, names_to = "run_num", values_to = "count" ) %>% 
  select(UID, common_name, count, run_num) 

keep <- complete.cases(dec_fish)
dec_fish$keep <- keep
dec_fish <- dec_fish %>% 
  filter(keep == TRUE) %>% 
  select(-keep)

dec_methods <- vt %>% 
  select(EventID, GearID, SectionWidth, SectionLength) %>% 
  rename(gear = GearID, reach_length_m = SectionLength, reach_width_avg_m = SectionWidth) %>% 
  mutate(UID = paste("VT", EventID, sep = "_"),
         goal = "Total Pick-up") %>% 
  select(UID, gear, goal, reach_length_m, reach_width_avg_m) %>% 
  unique()
dec_methods$gear[dec_methods$gear == "ES"] <- "efish"
dec_methods$gear[dec_methods$gear == "SN"] <- "seine"

dec_species <- vt %>% 
  select(Common.Name, FinalID) %>% 
  rename(common_name = Common.Name, scientific_name = FinalID) %>% 
  unique()
